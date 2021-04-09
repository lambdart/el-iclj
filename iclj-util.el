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

(defun iclj-util-keyword-to-symbol (keyword)
  "Convert KEYWORD to a symbol."
  (when keyword (replace-regexp-in-string "\\`:+" "" keyword)))

(defun iclj-util-bounds-of-thing-at-point ()
  "Return expression bounds at point."
  (when (not (memq (char-syntax (following-char)) '(?w ?_)))
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (beg (car-safe bounds))
           (end (cdr-safe bounds))
           ;; set thing (symbol or keyword) at point
           (thing (or (thing-at-point 'symbol)
                      (iclj-util-keyword-to-symbol
                       (buffer-substring beg end)))))
      ;; return the region delimiters
      (when (> (length thing) 0) (list beg end)))))

(defun iclj-util-thing-at-point ()
  "Return expression at point to complete."
  (let ((bounds (iclj-util-bounds-of-thing-at-point)))
    (and bounds (buffer-substring (car bounds) (cdr bounds)))))

(provide 'iclj-util)

;;; iclj-util.el ends here

