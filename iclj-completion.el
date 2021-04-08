;;; iclj-completion.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/iclj-completions.el
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

(require 'iclj-comint)

(defvar iclj-completion-beg nil)
(defvar iclj-completion-end nil)

;; necessary?
(defconst iclj-bad-chars "^[] \"'`><,;|&{()[@\\^]"
  "Regexp that define undesired chars.")

(defun iclj-keyword-to-symbol (keyword)
  "Convert KEYWORD to a symbol."
  (when keyword (replace-regexp-in-string "\\`:+" "" keyword)))

(defun iclj-expression-bounds-at-point ()
  "Return expression bounds at point."
  (when (not (memq (char-syntax (following-char)) '(?w ?_)))
    (save-excursion
      (let* ((end (point))
             (skipped-back (skip-chars-backward iclj-bad-chars))
             (start (+ end skipped-back))
             (chars (or (thing-at-point 'symbol)
                        (iclj-keyword-to-symbol (buffer-substring start end)))))
        (when (> (length chars) 0)
          (let ((first-char (substring-no-properties chars 0 1)))
            (when (string-match-p "[^0-9]" first-char)
              (list (point) end))))))))

(defun iclj-expression-at-point ()
  "Return expression at point to complete."
  (let ((bounds (iclj-expression-bounds-at-point)))
    (and bounds (buffer-substring (car bounds) (cdr bounds)))))

(defun iclj-completion-set-bounds ()
  "Set completion bounds."
  (let* ((bounds (iclj-expression-bounds-at-point))
         (beg (car bounds))
         (end (cadr bounds)))
    (setq iclj-completion-beg beg
          iclj-completion-end end)))

(defun iclj-completion-prefix ()
  "Return completion prefix."
  (and iclj-completion-beg
       iclj-completion-end
       (buffer-substring-no-properties iclj-completion-beg
                                       iclj-completion-end)))

(defun iclj-completion-insert (beg end completion)
  "Insert COMPLETION using BEG/END delimiters."
  (let ((buffer-read-only nil)
        (deactivate-mark nil))
    ;; delete previous region
    (delete-region beg end)
    ;; insert the completion at point
    (insert completion)))

(defun iclj-completion-handler (buffer)
  "Completion response handler.
Insert completion in the current BUFFER."
  (iclj-comint-with-redirect-output
   ;; bounds?
   (when (and iclj-completion-beg
              iclj-completion-end)
     ;; redirect buffer has completions?
     (if (string= output "nil ") nil
       ;; parse completions to a list of string and chose one of them
       (let ((completions (split-string (substring output 1 -2) " ")))
         (unless (equal completions '(""))
           (with-current-buffer buffer
             ;; insert the completion in the current buffer
             (iclj-completion-insert iclj-completion-beg
                                     iclj-completion-end
                                     (completing-read "" completions)))))))
   ;; always reset the beg/end bounds
   (setq iclj-completion-beg nil
         iclj-completion-end nil)))

(provide 'iclj-completion)

;;; iclj-completion.el ends here
