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

(require 'iclj-util)
(require 'iclj-comint)

(defvar iclj-completion-beg 0
  "Begging of the completion prefix candidate (region related).")

(defvar iclj-completion-end 0
  "End of the completion prefix candidate (region related).")

(defvar iclj-completion-initial-input ""
  "Completion initial input.")

(defun iclj-completion-set-bounds ()
  "Set completion bounds."
  (let* ((bounds (iclj-util-bounds-of-thing-at-point))
         (beg (car bounds))
         (end (cdr bounds)))
    (setq iclj-completion-beg beg
          iclj-completion-end end)))

(defun iclj-completion-clean-variables ()
  "Reset variables."
  (setq iclj-completion-beg 0
        iclj-completion-end 0
        iclj-completion-initial-input ""))

(defun iclj-completion-bounds-p ()
  "Bounds predicate."
  (= iclj-completion-beg iclj-completion-end))

(defun iclj-completion-initial-input ()
  "Return or set (implicit) completion initial input."
  (unless (iclj-completion-bounds-p)
    (setq iclj-completion-initial-input
          (buffer-substring-no-properties iclj-completion-beg
                                          iclj-completion-end))))

(defun iclj-completion-insert (buffer beg end completion)
  "Insert COMPLETION in current BUFFER using BEG/END delimiters."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (deactivate-mark nil))
      ;; delete previous region
      (delete-region beg end)
      ;; insert the completion at point
      (insert completion))))

(defun iclj-completion-candidates (output)
  "Parse completion candidates from the OUTPUT."
  (let ((candidates (split-string (substring-no-properties output 1 -2) " ")))
    ;; clear the namespace from the candidates and append it
    (append
     (mapcar (lambda (x)
               (replace-regexp-in-string "^.+/" "" x))
             candidates)
     candidates)))

(defun iclj-completion-select (completions)
  "Select completion from COMPLETIONS."
  (if (equal completions '("")) ""
    (completing-read "Complete: "
                     completions
                     nil
                     nil
                     iclj-completion-initial-input)))

(defun iclj-completion-handler (buffer)
  "Completion response handler.
Insert completion in the current BUFFER."
  (iclj-comint-with-redirect-output
   ;; region of symbol available?
   ;; redirect buffer has any completions?
   (if (or (iclj-completion-bounds-p)
           (string= output "nil"))
       nil
     ;; parse completions to a list of string and chose one of them
     (let* ((completions (iclj-completion-candidates output))
            (completion (iclj-completion-select completions)))
       (unless (string= completion "")
         (iclj-completion-insert buffer
                                 iclj-completion-beg
                                 iclj-completion-end
                                 completion)))))
  ;; always reset the beg/end/initial-input vars
  (iclj-completion-clean-variables))

(provide 'iclj-completion)

;;; iclj-completion.el ends here
