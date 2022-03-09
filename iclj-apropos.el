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
(require 'iclj-util)

(defvar iclj-apropos-buffer-name "*iclj-clojure-apropos*"
  "Apropos buffer name.")

(defvar iclj-apropos-buffer nil
  "Apropos temporary buffer.")

(defvar iclj-apropos-collection '()
  "Apropos cached collection.")

(defvar iclj-apropos-cache-flag nil
  "Non-nil means cache collection flag.")

(define-button-type 'iclj-apropos-button
  'face
  'font-lock-keyword-face
  'help-echo "mouse-2, RET: Display documentation"
  'follow-link t
  'action (lambda (button)
            (and (fboundp 'iclj-op-doc)
                 (funcall 'iclj-op-doc (button-get button 'symbol)))))

(defun iclj-apropos-buffer ()
  "Return apropos buffer."
  (setq iclj-apropos-buffer
        (or (and (buffer-live-p iclj-apropos-buffer)
                 iclj-apropos-buffer)
            (with-current-buffer (iclj-util-get-buffer-create
                                  iclj-apropos-buffer-name)
              (setq-local buffer-read-only t)
              (current-buffer)))))

(defun iclj-apropos-insert-button (symbol)
  "Insert button with SYMBOL property."
  (insert-button symbol 'symbol symbol :type 'iclj-apropos-button)
  (insert "\n"))

(defun iclj-apropos-collection (output)
  "Parse OUTPUT to a collection of string elements."
  (split-string (substring-no-properties output 1 -2) " " t))

(defun iclj-apropos-handler (output-buffer _)
  "Apropos OUTPUT-BUFFER operation handler."
  (iclj-util-with-buffer-content output-buffer t
    (unless (string-empty-p content)
      (save-excursion
        (display-buffer
         (let ((inhibit-read-only t))
           (with-current-buffer (iclj-apropos-buffer)
             (erase-buffer)
             (mapc (lambda (symbol)
                     (iclj-apropos-insert-button symbol))
                   (iclj-apropos-collection content))
             (goto-char (point-min))
             (current-buffer))))))))

(provide 'iclj-apropos)

;;; iclj-apropos.el ends here
