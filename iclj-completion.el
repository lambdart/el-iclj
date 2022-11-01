;;; iclj-completion.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/iclj
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

(require 'iclj-tq)
(require 'iclj-cmd)
(require 'iclj-util)
(require 'iclj-apropos)

(defvar iclj-completions '())
(defvar iclj-completion-thing nil)
(defvar iclj-completion-handler-ends nil)

(defun iclj-completion--append-clean-ns (collection)
  "Append COLLECTION after name space cleanup."
  (append
   (mapcar (lambda (x)
             (replace-regexp-in-string "^.+/" "" x))
           collection)
   collection))

(defun iclj-completion-handler (output-buffer _)
  "Completion OUTPUT-BUFFER handler."
  (setq iclj-completions
        (iclj-completion--append-clean-ns
         (iclj-apropos-collection
          (iclj-util-buffer-content output-buffer iclj-util-eoc)))))

(defun iclj-completion-send-cmd ()
  "Send completion command."
  ;; send apropos operation
  (iclj-tq-with-live-process iclj-cmd-tq
    (apply 'iclj-cmd-send
           `(apropos
             iclj-completion-handler
             t
             ""))))

(defun iclj-completion-all-completions ()
  "Return list of completions."
  (interactive)
  (iclj-tq-with-live-process iclj-cmd-tq
    (iclj-tq-eval-after-handler
        iclj-cmd-tq
        iclj-completion-send-cmd
      iclj-completions)))

(defun iclj-completion-completions (_input)
  "Return list of completions."
  iclj-completions)

(provide 'iclj-completion)
;;; iclj-completion.el ends here
