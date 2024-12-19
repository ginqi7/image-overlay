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

(defcustom image-slicing-download-concurrency 20
  "Define the maximum concurrency of images download."
  :group 'image-slicing
  :type 'number)

(defcustom image-slicing-line-height-scale 2
  "Define how many line height an image slice occupies."
  :group 'image-slicing
  :type 'number)

(defcustom image-slicing-max-width 700
  "Define the maximum width of images display."
  :group 'image-slicing
  :type 'number)

(defvar image-slicing--image-match-regexp ".*\\.\\(png\\|jpg\\|jpeg\\|drawio\\|svg\\|webp\\)")

(defvar image-slicing--links nil)

(defvar image-slicing--overlay-list nil)

(defvar image-slicing--temporary-file-template "image-slicing-remote-images-")

(defvar image-slicing--timer nil)

(defvar image-slicing-debug-p nil)

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
                  :command (cons program program-args)))))
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

(defun image-slicing-display (start end display buffer &optional before-string after-string)
  "Make an overlay from START to END in the BUFFER to show DISPLAY.
If BEFORE-STRING or AFTER-STRING not nil, put overlay before-string or after-string."
  (let ((overlay (make-overlay start end buffer)))
    (add-to-list 'image-slicing--overlay-list overlay)
    (when before-string
      (overlay-put overlay 'before-string before-string))
    (when after-string
      (overlay-put overlay 'after-string after-string))
    (overlay-put overlay 'display display)
    (overlay-put overlay 'face 'default)
    (overlay-put overlay 'overlay-type "image-slicing")
    overlay))

(defun image-slicing-slice (image-src max-rows)
  "Slice IMAGE-SRC into mutiple rows limited by MAX-ROWS."
  (let* ((image (image-slicing-create-image image-src))
         (image-pixel-cons (image-size image t))
         (image-pixel-h (cdr image-pixel-cons))
         (spliced-image-line-height (* image-slicing-line-height-scale (default-font-height)))
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
           (image-src (plist-get image-file-info :src))
           (new-line-str (propertize "\n" 'face 'default))
           (line-beginning-p (progn (goto-char begin) (equal begin (line-beginning-position)))))
      (plist-put image-file-info :status "start")
      (image-slicing--download-file-if-need
       image-src
       (lambda (image)
         (unless line-beginning-p
           (image-slicing-display begin (1+ begin) "" buffer new-line-str)
           (setq begin (1+ begin)))
         (dolist (image (image-slicing-slice image (- end begin 1)))
           (image-slicing-display begin (1+ begin) image buffer nil new-line-str)
           (setq begin (1+ begin)))
         (image-slicing-display begin end "" buffer)
         (plist-put image-file-info :status "finished"))))))

(defun image-slicing-run-tasks ()
  "Run Tasks unstarted."
  (let ((count 0)
        status)
    (dolist (link image-slicing--links)
      (setq status (plist-get link :status))
      (when (string= status "start")
        (setq count (1+ count)))
      (when (and (< count image-slicing-download-concurrency) (string= status "init"))
        (image-slicing-display-file link)
        (setq count (1+ count))))))

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
  (setq image-slicing--overlay-list nil)

  (remove-hook 'post-command-hook #'image-slicing-post-command t))

(defun image-slicing-create-image (image-src)
  "Create an image object by IMAGE-SRC.
If the image width is greater than `image-slicing-max-width`, scale it down."
  (let* ((image (create-image image-src))
         (image-pixel-cons (image-size image t))
         (image-pixel-w (car image-pixel-cons)))
    (when (> image-pixel-w image-slicing-max-width)
      (setf (image-property image :width) image-slicing-max-width))
    image))

(defun image-slicing-post-command ()
  "Handle cursor visibility in eww-mode when over image slicing overlays.
This function is installed on `post-command-hook'."
  (when (derived-mode-p 'eww-mode)
    (setq cursor-type (not (cl-some (lambda (overlay)
                                      (string-equal (overlay-get overlay 'overlay-type)
                                                    "image-slicing"))
                                    (overlays-at (point)))))))

(defun image-slicing-render-buffer ()
  "Auto image overlay."
  (setq image-slicing--links (image-slicing--overlay-list-links))
  (setq image-slicing--timer (image-slicing--create-render-timer))

  (add-hook 'post-command-hook #'image-slicing-post-command nil t))

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
