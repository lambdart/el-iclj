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
;; Copyright (c) 2020 lambdart
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

(defun iclj-util-setup-font-locks ()
  "Setup default font locks."
  (and (require 'clojure-mode nil t)
       (fboundp 'clojure-font-lock-setup)
       (funcall 'clojure-font-lock-setup)))

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
  (let* ((thing (or thing 'symbol))
         (bounds (bounds-of-thing-at-point thing)))
    (if (not bounds) ""
      (buffer-substring-no-properties (car bounds)
                                      (cdr bounds)))))

(defun iclj-util--last-line (buffer regexp)
  "Return the BUFFER last line determined by REGEXP pattern."
  (with-current-buffer buffer
    (save-excursion
      (widen)
      ;; go to the end of the buffer
      (goto-char (point-max))
      ;; go back one line
      (forward-line -1)
      ;; while last line not found, keep going backwards
      (while (and (> (point) (point-min))
                  (not (looking-at-p regexp)))
        (forward-line -1))
      ;; last line
      (forward-line -1)
      ;; return the string that represents the last line
      (buffer-substring-no-properties (point)
                                      (progn
                                        (end-of-line) (point))))))

(defun iclj-util-last-line (buffer regexp &optional default)
  "Return the BUFFER last line determined by REGEXP pattern.
DEFAULT, value to be returned if the last-line isn't found."
  (if (buffer-live-p buffer)
      (iclj-util--last-line buffer regexp)
    (or default "nil")))

(defun iclj-util-buffer-content (buffer &optional regexp)
  "Return BUFFER content.
If REGEXP is non-nil remove/filter it from the content."
  (save-excursion
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (widen)
        ;; go to the end of the buffer
        (goto-char (point-max))
        ;; remove end of command if necessary
        (if regexp
            (if (search-backward-regexp regexp nil t)
                (buffer-substring-no-properties (point-min)
                                                (point))
              "")
          ;; else return everything
          (buffer-substring-no-properties (point-min)
                                          (point-max)))))))

(defun iclj-util-get-buffer-create (buffer-or-name)
  "Get or create redirect buffer using the specify BUFFER-OR-NAME."
  (let ((buffer (get-buffer buffer-or-name)))
    (if (buffer-live-p buffer) buffer
      (let ((buffer (get-buffer-create buffer-or-name)))
        (with-current-buffer buffer
          ;; make the buffer read only
          (setq-local buffer-read-only t)
          ;; enable clojure-mode if available
          (and (require 'clojure-mode nil t)
               (fboundp 'clojure-font-lock-setup)
               (clojure-font-lock-setup)))
        ;; return the buffer
        buffer))))

(defun iclj-util-buffer-string (buffer-or-name)
  "Return BUFFER-OR-NAME content."
  (with-current-buffer (iclj-util-get-buffer-create buffer-or-name)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun iclj-util-erase-buffer (buffer-or-name)
  "Delete the entire contents of the buffer specify by BUFFER-OR-NAME."
  (with-current-buffer (iclj-util-get-buffer-create buffer-or-name)
    ;; remove read only protection
    (setq buffer-read-only nil)
    ;; clean buffer
    (erase-buffer)))

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
      (with-current-buffer buffer (save-buffer)))))

(provide 'iclj-util)

;;; iclj-util.el ends here
