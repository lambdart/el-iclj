;;; iclj-apropos.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/iclj-apropos.el
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

(require 'button)
(require 'iclj-op)

(defvar iclj-apropos-buffer-name "clojure-apropos"
  "Apropos buffer name.")

(defvar iclj-apropos-buffer nil
  "Apropos temporary buffer.")

(define-button-type 'iclj-apropos-button
  'face
  'font-lock-keyword-face
  'help-echo "mouse-2, RET: Display more help on this special form"
  'follow-link t
  'action (lambda (button)
            (iclj-op-doc (button-get button 'symbol))))

(defun iclj-apropos-buffer ()
  "Return apropos buffer."
  (if (buffer-live-p iclj-apropos-buffer) iclj-apropos-buffer
    (let ((buffer (get-buffer-create iclj-apropos-buffer-name)))
      (with-current-buffer buffer
        (setq buffer-read-only t))
      (setq iclj-apropos-buffer buffer))))

(defun iclj-apropos-insert-button (symbol)
  "Insert button with SYMBOL property."
  (insert-button symbol 'symbol symbol :type 'iclj-apropos-button)
  (insert "\n"))

(defun iclj-apropos-collection (output)
  "Parse OUTPUT to a collection of string elements."
  (split-string (substring-no-properties output 1 -2) " "))

(defun iclj-apropos-handler (_)
  "Apropos operation handler."
  (iclj-comint-with-redirect-output
   (let ((collection (iclj-apropos-collection output))
         (buffer (iclj-apropos-buffer)))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         ;; clean buffer
         (erase-buffer)
         ;; insert each button
         (mapc (lambda (x)
                 (iclj-apropos-insert-button x))
               collection))
       ;; go back to the start
       (goto-char (point-min)))
     ;; display apropos buffer
     (display-buffer buffer))))

(provide 'iclj-apropos)

;;; iclj-apropos.el ends here
