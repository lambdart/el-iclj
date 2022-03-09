;;; iclj-cmd.el --- summary -*- lexical-binding: t -*-
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
(require 'iclj-ns)
(require 'iclj-util)
(require 'iclj-op-table)

(require 'comint nil t)

(defvar iclj-cmd-tq nil
  "Default tq instance cache.")

(defvar iclj-cmd-subprompt-regexp " *#_=> *"
  "Regexp to recognize subprompts in the Inferior Clojure mode.")

(defvar iclj-cmd-prompt-regexp "\\( *#_\\|[^=> \n]+\\)=> *"
  "Regexp to recognize both main prompt and subprompt for comint.")

;;;###autoload
(defun iclj-connect (host port)
  "Make TQ Clojure repl server stream using the (HOST PORT) parameters."
  (interactive (list
                (iclj-util-read-host)
                (iclj-util-read-port iclj-util-port)))
  ;; maybe cache host to be used again
  (unless (member host iclj-util-host-history)
    (push host iclj-util-host-history))
  ;; update host and port, if necessary
  (cond ((string= host "")
         (iclj-util-log "error: missing host value!"))
        ;; default: tries open host/port stream and cache it
        (t (condition-case err
               (setq iclj-cmd-tq
                     (iclj-tq-make host
                                   port
                                   iclj-util-eoc
                                   iclj-cmd-prompt-regexp))
             ;; handle errors
             (error (iclj-util-log (concat "error: " (cadr err))))
             ;; handle success
             (:success (iclj-util-log "success: TQ connected!"))))))

;;;###autoload
(defun iclj-disconnect ()
  "Disconnect from transmission queue."
  (interactive)
  ;; erase if necessary
  (setq iclj-cmd-tq (and iclj-cmd-tq (iclj-tq-proc-delete iclj-cmd-tq)))
  ;; just a hint to the user
  (iclj-util-log "TQ disconnected!" t))

(defun iclj--default-handler (output-buffer &optional _)
  "Switch to OUTPUT-BUFFER (the process output buffer)."
  (save-excursion
    (display-buffer output-buffer)))

(defun iclj--format-with-eoc (op)
  "Return OP with end of command indicator."
  (format "%s (print %S)" op iclj-util-eoc))

(defun iclj--parse-input (input op-fmt)
  "Format INPUT (string or region) with OP-FMT (operation format)."
  (iclj--format-with-eoc
   (format op-fmt (let* ((temp (car input))
                         (str (if (not (stringp temp))
                                  (apply 'buffer-substring-no-properties input)
                                temp)))
                    str))))

(defun iclj--ensure-connection ()
  "Return cached transmission queue or create the connection."
  (or
   (setq iclj-cmd-tq
         (if (and iclj-cmd-tq
                  (not (iclj-tq-proc-live-p iclj-cmd-tq)))
             nil
           iclj-cmd-tq))
   (call-interactively 'iclj-cmd-connect)))

(defun iclj-cmd-send (op-key handler waitp &rest input)
  "Send the operation defined by OP-KEY.

HANDLER, function that will be called by the transmission queue with the
output buffer.

WAITP, when non-nil wait the end of command indicator before call the proper
handler.

INPUT, the string or the region bounds."
  (when (iclj--ensure-connection)
    (let ((op-plist (iclj-op-table-get-plist op-key)))
      (when op-plist
        (iclj-tq-enqueue iclj-cmd-tq
                         (iclj--parse-input input (plist-get op-plist :fmt))
                         (or waitp (plist-get op-plist :waitp))
                         (or handler
                             (plist-get op-plist :fun)
                             'iclj--default-handler)
                         ;; source buffer
                         (current-buffer)
                         ;; delay flag
                         t)))))

(defun iclj-eval-defn ()
  "Send definition to the Clojure comint process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (iclj-cmd-send 'eval-last nil nil (point) end))))

(defun iclj-eval-sexp (sexp)
  "Eval SEXP string, i.e, send it to Clojure comint process."
  (interactive (iclj-util-minibuffer-read 'sexp "Eval"))
  ;; eval string symbolic expression
  (iclj-cmd-send 'eval nil nil sexp))

(defun iclj-eval-last-sexp ()
  "Send the previous sexp to the inferior process."
  (interactive)
  ;; send region of the last expression
  (iclj-cmd-send 'eval-last-sexp
                 nil
                 nil
                 (save-excursion
                   (backward-sexp)
                   (point))
                 (point)))

(defun iclj-eval-region (beg end)
  "Eval BEG/END region."
  (interactive "r")
  (iclj-cmd-send 'eval nil nil beg end))

(defun iclj-eval-buffer ()
  "Eval current buffer."
  (interactive)
  (save-excursion
    (widen)
    (let ((case-fold-search t))
      (iclj-cmd-send 'eval
                     nil
                     nil
                     (point-min)
                     (point-max)))))

(defun iclj-eval-target-buffer (buffer)
  "Eval target BUFFER."
  (interactive "bBuffer: ")
  (with-current-buffer buffer
    (iclj-eval-buffer)))

(defun iclj-eval-file (filename)
  "Read FILENAME and evaluate it's region contents."
  (interactive "fFile: ")
  ;; insert buffer contents and call eval buffer operation
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (iclj-eval-buffer)))

;; TODO: check the list of open file buffer and evaluate it directly
(defun iclj--eval-file-contents (filename)
  "Copy FILENAME contents and eval the temporary buffer."
  (setq iclj-util-prev-l/c-dir/file
        (prog2
            ;; the user is queried to see if he wants to save the buffer before
            ;; proceeding with the load or compile
            (iclj-util-save-buffer filename)
            ;; cache previous directory/filename
            (cons (file-name-directory filename)
                  (file-name-nondirectory filename))
          (with-temp-buffer
            (insert-file-contents (expand-file-name filename))
            (iclj-eval-buffer)))))

(defun iclj-load-file (filename)
  "Load the target FILENAME."
  (interactive (iclj-util-read-source-file))
  ;; tries to eval file contest and cache previous local directory
  (iclj--eval-file-contents filename))

(defun iclj-run-tests ()
  "Invoke Clojure (run-tests) operation."
  (interactive)
  ;; send run test command
  (iclj-cmd-send 'run-tests nil nil ""))

(defun iclj-load-tests-and-run (filename)
  "Load FILENAME and send run-test command."
  (interactive (iclj-util-read-source-file))
  ;; send `eval-buffer' operation with file contents in a temporary buffer
  ;; and wait for it
  (iclj-tq-wait-proc-output iclj-cmd-tq
    (iclj--eval-file-contents filename))
  ;; run the tests after "load"
  (iclj-cmd-send 'run-tests nil nil ""))

(defun iclj-doc (input)
  "Describe identifier INPUT."
  (interactive (iclj-util-minibuffer-read 'sexp "Doc"))
  ;; send documentation operation
  (iclj-cmd-send 'doc nil nil input))

(defun iclj-find-doc (input)
  "Find INPUT documentation ."
  (interactive (iclj-util-minibuffer-read 'symbol "Find-doc"))
  ;; send find doc operation
  (iclj-cmd-send 'find-doc nil nil input))

(defun iclj-apropos (input)
  "Apropos INPUT."
  (interactive (iclj-util-minibuffer-read 'symbol "Apropos"))
  ;; send apropos command
  (iclj-cmd-send 'apropos nil nil input))

(defun iclj-all-ns ()
  "Pretty print all name spaces."
  (interactive)
  (iclj-cmd-send 'all-ns nil nil ""))

(defun iclj--ns-list ()
  "Cache namespace list."
  (iclj-tq-wait-proc-output iclj-cmd-tq
    (iclj-cmd-send 'ns-list nil t "")))

(defun iclj-ns-vars ()
  "List namespace symbols."
  (interactive)
  (iclj--ns-list)
  (iclj-cmd-send 'ns-vars nil nil (iclj-ns-read-namespace)))

(defun iclj-set-ns ()
  "Set current namespace."
  (interactive)
  (iclj--ns-list)
  (iclj-cmd-send 'set-ns nil t (iclj-ns-read-namespace)))

(defun iclj-source (input)
  "Show source from symbol INPUT."
  (interactive (iclj-util-minibuffer-read nil "Symbol"))
  ;; parse/send source command
  (iclj-cmd-send 'source nil nil input))

(defun iclj-meta (symbol)
  "Show meta data information for SYMBOL."
  (interactive (iclj-util-minibuffer-read nil "Symbol"))
  ;; parse/send meta command
  (iclj-cmd-send 'meta nil nil symbol))

(defun iclj-macroexpand ()
  "Macro expand symbolic expression at point."
  (interactive)
  (iclj-cmd-send 'macroexpand
                 nil
                 nil
                 (iclj-util-sexp-at-point)))

(defun iclj-macroexpand-1 ()
  "Macro expand (alternative) symbolic expression at point."
  (interactive)
  (iclj-cmd-send 'macroexpand-1
                 nil
                 nil
                 (iclj-util-sexp-at-point)))

(provide 'iclj-cmd)

;;; iclj-cmd.el ends here
