;;; iclj-comint.el --- summary -*- lexical-binding: t -*-
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

(require 'comint)

(defgroup iclj-comint nil
  "Clojure Scheme Utilities."
  :prefix "iclj-comint-"
  :group 'iclj-comint)

(defcustom iclj-comint-prompt-read-only t
  "If non-nil, the comint buffer will be read-only."
  :group 'iclj-comint
  :type 'boolean)

(defcustom iclj-comint-buffer-name "clojure-repl"
  "Inferior buffer default name."
  :group 'iclj-comint
  :type 'string)

(defcustom iclj-debug-buffer-name "*CLOJURE-DEBUG*"
  "Debug buffer name for the inferior process output."
  :group 'iclj-comint
  :type 'string)

(defcustom iclj-comint-prompt-regexp "^\\( *#_\\|[^=> \n]+\\)=> *"
  "Regexp to recognize prompt."
  :group 'iclj-comint
  :type 'regexp)

(defcustom iclj-comint-filter-regexp "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "Clojure comint input filter regex."
  :group 'iclj-comint
  :type 'regexp)

(defcustom iclj-comint-program (executable-find "clojure")
  "Clojure executable full path program."
  :group 'iclj-comint
  :type 'file
  :set `(lambda (symbol value)
          (set symbol (executable-find value))))

(defcustom iclj-comint-program-args '("-r")
  "Command-line arguments to pass to `iclj-comint-program'."
  :group 'iclj-comint
  :type 'list)

(defcustom iclj-comint-output-timeout 15
  "Comint accept output timeout in seconds."
  :group 'iclj-comint
  :type 'integer)

(defvar iclj-comint-start-file nil
  "The `make-comint' start file argument.")

(defvar iclj-comint-redirect-buffer-name "clojure-output"
  "Redirect output buffer name.")

(defvar iclj-comint-redirect-buffer nil
  "Redirect output buffer.")

(defvar iclj-comint-buffer nil
  "Comint process buffer.")

(defvar iclj-comint-output-cache '("")
  "Comint output filtered text list.")

(defvar iclj-comint-last-output ""
  "Process (cache) last output line.")

(defvar iclj-comint-proc-in-progress nil
  "Indicates if the comint filter function is still in progress.")

(defvar iclj-comint-resp-handler nil)

(defvar iclj-comint-from-buffer nil)

(defun iclj-comint-redirect-buffer ()
  "Get or create the \\{iclj-comint-redirect-buffer}."
  (if (buffer-live-p iclj-comint-redirect-buffer)
      iclj-comint-redirect-buffer
    (let ((buffer (get-buffer-create iclj-comint-redirect-buffer-name)))
      (with-current-buffer buffer
        ;; enable scheme-mode if available and chicken-mode?
        (and (require 'clojure-mode nil t)
             (fboundp 'clojure-mode)
             (clojure-mode)))
      ;; cache and return the buffer
      (setq iclj-comint-redirect-buffer buffer))))

(defun iclj-comint-redirect-buffer-content ()
  "Return the redirect-buffer content."
  (with-current-buffer (iclj-comint-redirect-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun iclj-comint-redirect-completed-p ()
  "Return if the PROC/BUFFER redirecting is over."
  ;; comint-redirect-completed is local in relation to the comint-buffer
  (with-current-buffer iclj-comint-buffer
    (null comint-redirect-completed)))

(defun iclj-comint-redirect-erase-buffer ()
  "Clean the redirect buffer."
  (with-current-buffer (iclj-comint-redirect-buffer)
    ;; remove read only protection (just in case)
    (setq buffer-read-only nil)
    ;; clean the buffer
    (erase-buffer)))

(defun iclj-comint-proc ()
  "Return current comint process."
  (let ((proc (get-buffer-process
               (and (buffer-live-p iclj-comint-buffer)
                    iclj-comint-buffer))))
    (if (process-live-p proc) proc
      (get-buffer-process (iclj-comint-run)))))

(defun iclj-comint-proc-sentinel (process event)
  "Sentinel function to handle (PROCESS EVENT) relation."
  (princ (format "Process: %s had the event '%s'" process event)))

(defun iclj-comint-proc-in-progress-timeout ()
  "Timeout, set the control variable to nil."
  (setq iclj-comint-proc-in-progress nil))

(defun iclj-comint-proc-wait ()
  "Wait for the comint output."
  (when iclj-comint-proc-in-progress
    ;; run with timer (timeout)
    (run-with-timer iclj-comint-output-timeout nil
                    'iclj-comint-proc-in-progress-timeout)
    ;; wait for the final prompt
    (while iclj-comint-proc-in-progress
      (sleep-for 0 10))))

(defmacro iclj-comint-with-redirect-output (&rest body)
  "Evaluate the BODY forms with the redirect output."
  ;; extract output from the buffer
  `(let ((output (iclj-comint-redirect-buffer-content)))
     ;; update output
     (setq output (if (string= output "") "nil" output))
     ;; evaluate body forms
     ,@body
     ;; clean display function
     (setq iclj-comint-resp-handler nil
           iclj-comint-from-buffer nil)))

(defun iclj-comint-redirect-dispatch-resp-handler ()
  "Dispatch the display handler callback."
  (when iclj-comint-resp-handler
    (funcall iclj-comint-resp-handler
             iclj-comint-from-buffer)))

(defun iclj-comint-cache-output ()
  "Return cached cache output."
  ;; concat the output and return it
  (eval `(concat ,@(reverse iclj-comint-output-cache))))

(defun iclj-comint-preoutput-filter (string)
  "Return the output STRING."
  ;; cache comint response
  (push string iclj-comint-output-cache)
  ;; verify filter in progress control variable
  (when (string-match-p iclj-comint-prompt-regexp string)
    (setq iclj-comint-proc-in-progress nil))
  ;; return the string to the comint buffer (implicit)
  string)

(defun iclj-comint-send (string)
  "Send STRING, using `comint-send-string' function."
  (let ((proc (iclj-comint-proc)))
    ;; check if the process is alive
    (if (not (process-live-p proc))
        (message "[CHICKEN]: error, process not found")
      ;; output list should always start empty
      ;; set progress control variable to true
      (setq iclj-comint-output-cache '("")
            iclj-comint-proc-in-progress t)
      ;; send string (or region) to the process
      (comint-send-string proc string)
      ;; always send a new line: <return> (implicit)
      (comint-send-string proc "\n"))))

(defun iclj-comint-redirect-input-to-process
    (send-func from-buffer echo &optional no-display &rest input)
  "Send INPUT FROM-BUFFER to process using the chosen SEND-FUNC.
If ECHO is non-nil, output in process buffer.
If NO-DISPLAY is non-nil, do not show the output buffer."
  (let* ((proc (iclj-comint-proc))
         (proc-buffer (process-buffer proc))
         (output-buffer (iclj-comint-redirect-buffer)))
    (if (not (buffer-live-p output-buffer))
        (message "[CHICKEN]: error, no redirect output buffer available")
      ;; erase redirect buffer
      (iclj-comint-redirect-erase-buffer)
      ;; change to the process buffer
      (with-current-buffer proc-buffer
        ;; set up for redirection
        (comint-redirect-setup output-buffer
                               iclj-comint-buffer        ; comint buffer
                               iclj-comint-prompt-regexp ; finished regexp
                               echo)                        ; echo input
        ;; set the filter
        (add-function :around (process-filter proc) #'comint-redirect-filter)
        ;; apply the right process send function
        (with-current-buffer from-buffer (apply send-func proc input))
        ;; always send new line <return>
        (process-send-string proc "\n")
        ;; show the output
        (or no-display (display-buffer (get-buffer-create output-buffer)))))))

(defun iclj-comint-input-sender (_ string)
  "Comint input sender STRING function."
  (iclj-comint-send string))

(defun iclj-comint-input-filter (string)
  "Don't save anything on the STRING matching `iclj-comint-filter-regexp'."
  (not (string-match-p iclj-comint-filter-regexp string)))

(defun iclj-comint-get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun iclj-comint-setup ()
  "Helper function to setup `comint-mode' related variables.
This function will be called by `chicken-mode-hook.'"
  ;; set comint related variables
  (setq comint-process-echoes t
        comint-input-ignoredups nil
        comint-use-prompt-regexp t))

(defun iclj-comint-quit ()
  "Quit Clojure comint, i.e, quit subjob and kill the buffer."
  (interactive)
  ;; quit the subjob, if necessary
  (and iclj-comint-buffer
       (with-current-buffer iclj-comint-buffer
         (comint-quit-subjob)))
  ;; kill the buffer
  (kill-buffer iclj-comint-buffer))

;;;###autoload
(defun iclj-comint-run ()
  "Run an inferior instance of Clojure REPL inside Emacs."
  (interactive)
  (let ((buffer (apply 'make-comint
                       iclj-comint-buffer-name
                       iclj-comint-program
                       iclj-comint-start-file
                       iclj-comint-program-args)))
    (unless buffer
      (error "[CHICKEN]: Error, start process %s: fails"
             iclj-comint-program))
    ;; check comint process
    (comint-check-proc buffer)
    ;; set process sentinel
    (set-process-sentinel (get-buffer-process buffer)
                          'iclj-comint-proc-sentinel)
    ;; start chicken comint mode
    (with-current-buffer buffer
      (iclj-comint-mode))
    ;; display buffer
    (display-buffer buffer 'display-buffer-pop-up-window)
    ;;; cache the process buffer (implicit: return it)
    (setq iclj-comint-buffer buffer)))

;; (declare-function 'iclj-op-load-file "iclj-op.el")

(defvar iclj-comint-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map (kbd "C-c C-l") #'iclj-op-load-file)
    (define-key map (kbd "C-c C-q") #'iclj-comint-quit)
    map)
  "Extended Comint Mode Map.")

(define-derived-mode iclj-comint-mode comint-mode "CLOJURE-REPL"
  "Major mode for `iclj-comint' REPL buffer.

Runs a Clojure interpreter with the help of `comint-mode',
use the buffer abstraction as the main I/O bridge between
Emacs and the inferior process.

The following commands are available:

\\{iclj-comint-mode-map}"
  :group 'iclj-comint
  ;; :keymap iclj-comint-mode-map
  ;; set comint variables
  (setq comint-prompt-regexp iclj-comint-prompt-regexp
        comint-prompt-read-only iclj-comint-prompt-read-only
        comint-input-sender (function iclj-comint-input-sender)
        comint-input-filter (function iclj-comint-input-filter)
        comint-get-old-input (function iclj-comint-get-old-input))
  ;; add comint preoutput filter function
  (add-hook 'comint-preoutput-filter-functions
            'iclj-comint-preoutput-filter nil t)
  ;; customize redirect verbose
  (customize-set-variable 'comint-redirect-verbose t)
  ;; add dispatch display handler hook
  (add-hook 'comint-redirect-hook
            #'iclj-comint-redirect-dispatch-resp-handler)
  ;; set local paragraph variables
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) iclj-comint-prompt-regexp))

;;;###autoload
(add-hook 'iclj-comint-mode-hook 'iclj-comint-setup)

(provide 'iclj-comint)

;;; iclj-comint.el ends here
