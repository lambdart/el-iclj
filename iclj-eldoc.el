;;; iclj-eldoc.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/iclj-eldoc.el
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

(require 'iclj-op)
(require 'iclj-util)
(require 'iclj-comint)
(require 'iclj-op-table)

(defvar iclj-eldoc-thing ""
  "Cache eldoc thing at point.")

(defvar iclj-eldoc-meta-data nil
  "Cache eldoc meta data information.")

(defvar iclj-eldoc-callback nil
  "Cache eldoc callback function.")

(defun iclj-eldoc-beginning-of-sexp ()
  "Move to the beginning of current sexp.
Return the number of nested sexp the point was over or after."
  (let ((parse-sexp-ignore-comments t)
        (num-skipped-sexps 0))
    (condition-case _
        (progn
          ;; First account for the case the point is directly over a
          ;; beginning of a nested sexp.
          (condition-case _
              (let ((p (point)))
                (forward-sexp -1)
                (forward-sexp 1)
                (when (< (point) p)
                  (setq num-skipped-sexps 1)))
            (error))
          (while
              (let ((p (point)))
                (forward-sexp -1)
                (when (< (point) p)
                  (setq num-skipped-sexps (1+ num-skipped-sexps))))))
      (error))
    num-skipped-sexps))

(defun iclj-eldoc-fnsym ()
  "Return function symbol from current sexp."
  (save-excursion
    ;; goes to the beginning of the s-expression
    (iclj-eldoc-beginning-of-sexp)
    ;; don't do anything if current word is inside a string, vector,
    ;; hash or set literal
    (if (member (or (char-after (1- (point))) 0) '(?\" ?\{ ?\[))
        ""
      (or (thing-at-point 'symbol) ""))))

(defun iclj-eldoc-clean (output-buffer)
  "Clean internal variables and operation OUTPUT-BUFFER."
  ;; reset callback function
  (setq iclj-eldoc-callback nil)
  ;; kill output buffer
  (kill-buffer output-buffer))

(defun iclj-eldoc-parse-docstring (docstring)
  "Parse DOCSTRING output documentation string."
  (if (not docstring)
      ""
    (let ((docstring (replace-regexp-in-string "\n" "" docstring)))
      (replace-regexp-in-string "  " " " docstring))))

(defun iclj-eldoc-display-docstring (meta-data callback)
  "Display eldoc documentation string.
Parse the documentation string and its META-DATA,
using the CALLBACK function."
  ;; check and invoke callback function
  (let ((fnsym (car meta-data))
        (args (cadr meta-data))
        (docstring (caddr meta-data)))
    (when (functionp callback)
      (funcall callback
               (concat args " " (iclj-eldoc-parse-docstring docstring))
               :thing fnsym
               :face font-lock-function-name-face))))

(defun iclj-eldoc-function (callback &rest _ignored)
  "Clojure documentation function.
Each hook function is called with at least one argument CALLBACK,
a function, and decides whether to display a doc short string
about the context around point."
  (let ((thing (iclj-eldoc-fnsym)))
    (unless (string= thing "")
      ;; verify if we have the same thing
      (if (string= thing iclj-eldoc-thing)
          (iclj-eldoc-display-docstring iclj-eldoc-meta-data
                                        callback)
        ;; else: call the eldoc operations
        (when (process-live-p (get-buffer-process iclj-comint-buffer))
          (iclj-op-eldoc thing)
          ;; cache eldoc callback
          (setq iclj-eldoc-callback callback)
          ;; cache thing
          (setq iclj-eldoc-thing thing)
          ;; - If the computation of said doc string (or the decision whether
          ;; there is one at all) is expensive or can't be performed
          ;; directly, the hook function should return a non-nil, non-string
          ;; value and arrange for CALLBACK to be called at a later time,
          ;; using asynchronous processes or other asynchronous mechanisms.
          0)))))

(defun iclj-eldoc-handler (_)
  "Eldoc function handler."
  (let ((output-buffer (iclj-op-table-get-property 'eldoc :buf)))
    (iclj-comint-with-redirect-output
     ;; get eldoc operation output buffer
     output-buffer
     ;; parse output and display into echo area
     (let ((meta-data (read output)))
       (when (listp meta-data )
         ;; cache eldoc meta-data information
         (setq iclj-eldoc-meta-data meta-data)
         ;; display eldoc docstring
         (iclj-eldoc-display-docstring iclj-eldoc-meta-data
                                       iclj-eldoc-callback))))
    ;; clean variables and kill output buffer
    (iclj-eldoc-clean output-buffer)))

(defun iclj-eldoc-enable ()
  "Enable eldoc operation."
  (add-hook 'eldoc-documentation-functions #'iclj-eldoc-function nil t))

(defun iclj-eldoc-disable ()
  "Disable eldoc operation."
  (interactive)
  (remove-hook 'eldoc-documentation-functions #'iclj-eldoc-function t))

(provide 'iclj-eldoc)

;;; iclj-eldoc.el ends here

