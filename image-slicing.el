;;; image-slicing.el --- Display an image as overlays.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Installation:
;; Manual:
;; Download the source code and put it wherever you like, e.g. into
;; ~/.emacs.d/image-slice/
;; ```
;; git clone git@github.com:ginqi7/image-slice.git
;; ```
;; Add the downloaded directory to the load path:
;; ```
;; (add-to-list 'load-path "~/.emacs.d/image-slice/")
;; (require 'image-slicing)
;; ```
;;

;;; Code:

(require 'org-element)
(require 'image)
(require 'url-util)

(defvar image-slicing--image-match-regexp ".*\\.\\(png\\|jpg\\|jpeg\\|drawio\\|svg\\|webp\\)")

(defvar image-slicing--links nil)

(defvar image-slicing--overlay-list nil)

(defvar image-slicing--temporary-file-template "image-slicing-remote-images-")

(defvar image-slicing--timer nil)

(defvar image-slicing-debug-p nil)

(defvar image-slicing-max-width 700)

(defun image-slicing--async-start-process (name program finish-func &rest program-args)
  "Start the executable PROGRAM asynchronously named NAME.
PROGRAM is passed PROGRAM-ARGS, calling FINISH-FUNC with the
process object when done.  If FINISH-FUNC is nil, the future
object will return the process object when the program is
finished.  Set DEFAULT-DIRECTORY to change PROGRAM's current
working directory."
  (let* ((buf (generate-new-buffer (concat "*" name "*")))
         (buf-err (generate-new-buffer (concat "*" name ":err*")))
         (proc (let ((process-connection-type nil))
                 (make-process
                  :name name
                  :buffer buf
                  :stderr buf-err
                  :command (cons program program-args)
                  :noquery async-process-noquery-on-exit))))
    (set-process-sentinel
     (get-buffer-process buf-err)
     (lambda (proc _change)
       (unless (or image-slicing-debug-p (process-live-p proc))
         (kill-buffer (process-buffer proc)))))

    (set-process-sentinel
     proc
     (lambda (proc status)
       (when (string= status "finished\n")
         (with-current-buffer (process-buffer proc)
           (funcall finish-func (buffer-string))
           (unless image-slicing-debug-p
             (kill-buffer))))))))

(defun image-slicing--remote-file-p (image-src)
  "Check if IMAGE-SRC is a remote file."
  (string-prefix-p "http" image-src))

(defun image-slicing--download-file-if-need (image-src callback)
  "If IMAGE-SRC is a remote file, download it and run the CALLBACK function.
Otherwise, just run the CALLBACK function only."

  (let ((temp-image-file (make-temp-file image-slicing--temporary-file-template)))
    (if (image-slicing--remote-file-p image-src)
        (image-slicing--async-start-process
         "curl"
         "curl"
         (lambda (data)
           (funcall callback temp-image-file))
         image-src
         "-s"
         "-o"
         temp-image-file)
      (funcall callback image-src))))

(defun image-slicing-create-image (image-src)
  "Create an image object by IMAGE-SRC.
If the image width is greater than `image-slicing-max-width`, scale it down."
  (let* ((image (create-image image-src))
         (image-pixel-cons (image-size image t))
         (image-pixel-w (car image-pixel-cons)))
    (when (> image-pixel-w image-slicing-max-width)
      (setf (image-property image :width) image-slicing-max-width))
    image))

(defun image-slicing-display (start end display &optional buffer)
  "Make an overlay from START to END in the BUFFER to show DISPLAY."
  (let ((overlay
         (make-overlay start end buffer)))
    (add-to-list 'image-slicing--overlay-list overlay)
    (overlay-put overlay 'display display)
    (overlay-put overlay 'face 'default)
    overlay))

(defun image-slicing--create-render-timer ()
  "Create a timer to render images at regular intervals."
  (unless image-slicing--timer
    (run-at-time nil 0.5 #'image-slicing-run-tasks)))

(defun image-slicing--overlay-list-links ()
  "List all links in the current buffer."
  (let ((links '())
        (content (buffer-string))
        (buffer (current-buffer)))
    (with-temp-buffer
      (insert content)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (let ((type (org-element-property :type link))
                (path (org-element-property :path link))
                (begin (org-element-property :begin link))
                (end (org-element-property :end link))
                (raw-link (org-element-property :raw-link link)))
            (when (and link (string-match-p image-slicing--image-match-regexp raw-link))
              (push
               (list
                :status "init"
                :buffer buffer
                :begin begin
                :end end
                :type type
                :src
                (pcase type
                  ("file" (file-truename path))
                  (_ (url-encode-url raw-link))))
               links))))))
    (reverse links)))

(defun image-slicing-clear ()
  "Clear related overlays and some related variables."
  (interactive)
  (setq image-slicing--links nil)
  (when image-slicing--timer
    (cancel-timer image-slicing--timer)
    (setq image-slicing--timer nil))
  (mapc #'delete-overlay image-slicing--overlay-list)
  (setq image-slicing--overlay-list nil))

(defun image-slicing-slice (image-src max-rows)
  "Slice IMAGE-SRC into mutiple rows limited by MAX-ROWS."
  (let* ((image (image-slicing-create-image image-src))
         (image-pixel-cons (image-size image t))
         (image-pixel-h (cdr image-pixel-cons))
         (spliced-image-line-height (* 2 (default-font-height)))
         (rows (/ image-pixel-h spliced-image-line-height))
         (rows (min max-rows rows))
         (x 0.0)
         (dx 1.0001)
         (y 0.0)
         (dy (/ 1.0001 rows))
         (sliced-images))
    (while (< y 1.0)
      (push (list (list 'slice x y dx dy) image) sliced-images)
      (setq y (+ y dy)))
    (reverse sliced-images)))

(defun image-slicing-display-file (image-file-info)
  "Display image by IMAGE-FILE-INFO."
  (save-excursion
    (let* ((begin (plist-get image-file-info :begin))
           (end (plist-get image-file-info :end))
           (buffer (plist-get image-file-info :buffer))
           (image-src (plist-get image-file-info :src)))
      (plist-put image-file-info :status "start")
      (image-slicing--download-file-if-need
       image-src
       (lambda (image)
         (image-slicing-display begin end "" buffer)
         (dolist (image (image-slicing-slice image (/ (- end begin) 2)))
           (image-slicing-display begin (1+ begin) image buffer)
           (setq begin (1+ begin))
           (image-slicing-display begin (1+ begin) "\n" buffer)
           (setq begin (1+ begin)))
         (plist-put image-file-info :status "finished")))
      (image-slicing-display begin end "" buffer))))

(defun image-slicing-run-tasks ()
  "Run Tasks unstarted."
  (let ((count 0)
        status)
    (dolist (link image-slicing--links)
      (setq status (plist-get link :status))
      (when (string= status "start")
        (setq count (1+ count)))
      (when (and (< count 20) (string= status "init"))
        (image-slicing-display-file link)
        (setq count (1+ count))))))

(defun image-slicing-render-buffer ()
  "Auto image overlay."
  (setq image-slicing--links (image-slicing--overlay-list-links))
  (setq image-slicing--timer (image-slicing--create-render-timer)))

(defun image-slicing-tag-img (dom &optional _url)
  "Parse img DOM."
  (let ((url (dom-attr dom 'src)))
    (when (not (string-prefix-p "http" url))
      (setq url (concat "https:" url)))
    (insert (format "[[%s]]" url))))

(define-minor-mode image-slicing-mode
  "A minor mode that show image overlay."
  :init-value nil
  :global nil
  (if (not image-slicing-mode)
      (image-slicing-clear)
    (image-slicing-render-buffer)))

(provide 'image-slicing)
;;; image-slicing.el ends here
