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

(defun iclj-eldoc-beginning-of-sexp ()
  "Move to the beginning of current sexp."
  (let ((parse-sexp-ignore-comments t))
    (condition-case _
        (progn
          ;; First account for the case the point is directly over a
          ;; beginning of a nested sexp.
          (condition-case _
              (forward-sexp -1)
            (forward-sexp 1)
            (error))
          (while (forward-sexp -1)))
      (error))))

(defun inf-clojure-eldoc-top-symbol ()
  "Return top most symbol of the current s-expression."
  (save-excursion
    ;; goes to the beginning of the s-expression
    (iclj-eldoc-beginning-of-sexp)
    ;; get symbol at point
    (let ((thing (iclj-util-thing-at-point)))
      thing)))

(defvar iclj-eldoc-callback nil
  "Eldoc cached callback.")

(defun iclj-eldoc-function (callback &rest _ignored)
  "Clojure documentation function.
Each hook function is called with at least one argument CALLBACK,
a function, and decides whether to display a doc short string
about the context around point."
  (let ((thing (inf-clojure-eldoc-top-symbol)))
    (if (string= thing "") nil
      ;; call the eldoc operation
      (iclj-op-eldoc thing)
      ;; cache eldoc callback
      (setq iclj-eldoc-callback callback)
      ;; - If the computation of said doc string (or the decision whether
      ;; there is one at all) is expensive or can’t be performed
      ;; directly, the hook function should return a non-nil, non-string
      ;; value and arrange for CALLBACK to be called at a later time,
      ;; using asynchronous processes or other asynchronous mechanisms.
      0)))

(defun iclj-eldoc-handler (_)
  "Eldoc function handler."
  (iclj-comint-with-redirect-output
   ;; get eldoc operation output buffer
   (iclj-op-table-get-property 'eldoc :buf)
   ;; parse output and display into echo area
   (let ((docs (read output)))
     (funcall iclj-eldoc-callback
              (mapconcat (lambda (x) x) docs " ")))))

(provide 'iclj-eldoc)

;;; iclj-eldoc.el ends here

