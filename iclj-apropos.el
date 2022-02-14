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
  (if (buffer-live-p iclj-apropos-buffer)
      iclj-apropos-buffer
    (let ((buffer (iclj-util-get-buffer-create iclj-apropos-buffer-name)))
      (with-current-buffer buffer
        (setq buffer-read-only t))
      (setq iclj-apropos-buffer buffer))))

(defun iclj-apropos-insert-button (symbol)
  "Insert button with SYMBOL property."
  (insert-button symbol 'symbol symbol :type 'iclj-apropos-button)
  (insert "\n"))

(defun iclj-apropos-append-collection (collection)
  "Clean name space from the COLLECTION and append it."
  (append
   (mapcar (lambda (x)
             (replace-regexp-in-string "^.+/" "" x))
           collection)
   collection))

(defun iclj-apropos-collection (output &optional cache-flag)
  "Parse OUTPUT to a collection of string elements.
If CACHE-FLAG is true save the collection after removing name spaces and append
it to the original collection (this should be used in the completions
setup phase)."
  (let ((collection (split-string (substring-no-properties output 1 -2) " ")))
    (if (not cache-flag)
        ;; just return the collection
        collection
      ;; otherwise cache it
      (setq iclj-apropos-collection
            (iclj-apropos-append-collection collection)))))

(defun iclj-apropos-handler (output-buffer _source-buffer)
  "Apropos OUTPUT-BUFFER operation handler."
  (let ((content (iclj-util-buffer-content output-buffer iclj-util-eoc)))
    (unless (string-empty-p content)
      (save-excursion
        (display-buffer
         (let ((buffer (iclj-apropos-buffer))
               (collection (iclj-apropos-collection content))
               (inhibit-read-only t))
           (with-current-buffer buffer
             ;; clean buffer (just in case)
             (erase-buffer)
             ;; insert buttons
             (dolist (symbol collection)
               (iclj-apropos-insert-button symbol))
             ;; go to beginning of the buffer
             (goto-char (point-min))
             ;; return the buffer
             (current-buffer))))))))

(provide 'iclj-apropos)

;;; iclj-apropos.el ends here
