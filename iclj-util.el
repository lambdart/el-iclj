;;; iclj-util.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/iclj-util.el
;; Version: 0.0.1 Alpha
;; Keywords:
;;
;; This file is NOT part of GNU Emacs.
;;
;;; MIT License
;;
;; Copyright (c) 2020 2021 2022 lambdart
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;;; Code:

(defvar iclj-util-host-history '()
  "Host history list.")

(defvar iclj-util-port "5555"
  "Default port number.")

(defvar iclj-util-eoc "\n--ICLJ-EOC-INDICATOR--\n"
  "Default end of command indicator.")

(defmacro iclj-util-log (string &rest body)
  "Message STRING and execute BODY forms.
Return the value of last evaluated form."
  (declare (indent 2))
  `(progn
     (message "%s" (concat "[ICLJ]: " ,string))
     ;; evaluated body forms
     ,@body))

(defmacro iclj-util-with-log (string force &rest body)
  "Message STRING if last evaluated BODY form is non-nill.
If FORCE is non-nill always force the message STRING."
  (declare (indent 2))
  `(let ((temp (progn ,@body)))
     (and (or ,force temp)
          (message "%s" (concat "[ICLJ]: " ,string))
          (message nil))
     temp))

(defun iclj-util-read-port (&optional default-port)
  "Read port, when DEFAULT-PORT is non-nil suggest it."
  (let* ((fmt (if default-port "Port[%s]: " "Port: %s"))
         (port (read-string (format fmt (or default-port ""))
                            nil
                            nil
                            default-port)))
    ;; return the choose port or the default one
    (if (string= port "")
        default-port
      port)))

(defun iclj-util-read-host ()
  "Read host and port."
  (let* ((host-history (or (car-safe iclj-util-host-history) ""))
         (host-prompt (format
                       (if (string= host-history "")
                           "Host: "
                         "Host[%s]: ")
                       host-history)))
    ;; read the values using the `minibuffer'
    (read-string host-prompt
                 nil
                 host-history
                 iclj-util-host-history)))

(defun iclj-util-bounds-of-thing-at-point ()
  "Return expression bounds at point."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (or (bounds-of-thing-at-point 'symbol)
        (bounds-of-thing-at-point 'word)
        (cons (point)
              (point)))))

(defun iclj-util-thing-at-point (&optional thing)
  "Return THING at point.
See the documentation of `thing-at-point' to understand what
thing means."
  (let ((bounds (bounds-of-thing-at-point (or thing 'symbol))))
    (buffer-substring-no-properties
     (or (car-safe bounds) (point))
     (or (cdr-safe bounds) (point)))))

(defun iclj-util-sexp-at-point ()
  "Return symbolic expression at point."
  (buffer-substring-no-properties
   (save-excursion
     (backward-sexp)
     (point))
   (point)))

(defun iclj-util--last-line (buffer regexp)
  "Return the BUFFER last line determined by REGEXP pattern."
  (with-current-buffer buffer
    (save-excursion
      (widen)
      (buffer-substring-no-properties
       (progn
         (goto-char (point-max))
         (forward-line -1)
         (while (and (> (point) (point-min))
                     (not (looking-at-p regexp)))
           (forward-line -1))
         (forward-line -1)
         (point))
       (progn (end-of-line) (point))))))

(defun iclj-util-last-line (buffer regexp &optional default)
  "Return the BUFFER last line determined by REGEXP pattern.
DEFAULT, value to be returned if the last-line isn't found."
  (if (buffer-live-p buffer)
      (iclj-util--last-line buffer regexp)
    (or default "nil")))

(defun iclj-util-buffer-content (buffer &optional regexp)
  "Return BUFFER content.
If REGEXP is non-nil remove its matches from the content."
  (when (buffer-live-p buffer)
    (save-excursion
      (with-current-buffer buffer
        (widen)
        (goto-char (point-max))
        (apply 'buffer-substring-no-properties
               (if regexp
                   (if (search-backward-regexp regexp nil t)
                       `(,(point-min) ,(point))
                     `(,(point) ,(point)))
                 `(,(point-min) ,(point-max))))))))

(defmacro iclj-util-with-buffer-content (buffer &optional cleanp &rest body)
  "Bind BUFFER content and evaluated the BODY forms.
If CLEANP is non-nil kill the BUFFER before the BODY forms are
evaluated."
  (declare (indent 2)
           (debug t))
  `(let ((content (iclj-util-buffer-content ,buffer)))
     (and ,cleanp (kill-buffer ,buffer))
     ,@body))

(defvar iclj-util-local-keymap
  (let ((keymap (make-sparse-keymap)))
    ;; quick commands
    (define-key keymap (kbd "C-q") (lambda ()
                                     (interactive)
                                     (kill-buffer (current-buffer))))
    ;; return keymap structure/object
    keymap)
  "Auxiliary keymap to provide quick-access to some useful commands.")

(defun iclj-util-get-buffer-create (buffer-or-name)
  "Get or create redirect buffer using the specify BUFFER-OR-NAME."
  (if (and (bufferp buffer-or-name)
           (buffer-live-p buffer-or-name))
      buffer-or-name
    (with-current-buffer (get-buffer-create buffer-or-name)
      ;; make the buffer read only
      (setq-local buffer-read-only t)
      ;; verifies if clojure-mode is available
      (and (require 'clojure-mode nil t)
           (fboundp 'clojure-mode)
           (clojure-mode))
      ;; set our local map
      (use-local-map
       (make-composed-keymap iclj-util-local-keymap (current-local-map)))
      ;; return buffer
      (current-buffer))))

(defun iclj-util-insert-chunk (buffer chunk)
  "Insert CHUNK string in target BUFFER."
  (unless (string-empty-p chunk)
    (let ((inhibit-read-only t))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-max))
            (insert chunk)))))))

(defun iclj-util-buffer-string (buffer-or-name)
  "Return BUFFER-OR-NAME content."
  (with-current-buffer (iclj-util-get-buffer-create buffer-or-name)
    (buffer-substring-no-properties (point-min)
                                    (point-max))))

(defun iclj-util-erase-buffer (buffer-or-name)
  "Delete the entire contents of the buffer specify by BUFFER-OR-NAME."
  (with-current-buffer (iclj-util-get-buffer-create buffer-or-name)
    (let ((buffer-read-only nil))
      (erase-buffer))))

(defun iclj-util-save-buffer (filename)
  "Check whether to save buffer visiting file FILENAME.
Prior to loading or compiling, this function can be called on the filename.
If the file is loaded into a buffer, and the buffer is modified, the user
is queried to see if he wants to save the buffer before proceeding with
the load or compile."
  (let ((buffer (get-file-buffer filename)))
    (when (and buffer
               (buffer-modified-p buffer)
               (y-or-n-p (format "Save buffer %s first? " (buffer-name buffer))))
      (with-current-buffer buffer
        (save-buffer)))))

(provide 'iclj-util)

;;; iclj-util.el ends here
