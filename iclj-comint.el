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
(require 'iclj-util)

(defgroup iclj-comint nil
  "Iclj comint utilities."
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

(defcustom iclj-comint-prompt-regexp "^\\( *#_\\|[^=> \n]+\\)=> *"
  "Regexp to recognize prompt."
  :group 'iclj-comint
  :type 'regexp)

(defcustom iclj-comint-filter-regexp "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "Clojure comint input filter regex."
  :group 'iclj-comint
  :type 'regexp)

(defcustom iclj-comint-program "clojure"
  "Clojure executable full path program."
  :group 'iclj-comint
  :type 'file
  :set `(lambda (symbol value)
          (set symbol (or (executable-find value) value))))

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

(defvar iclj-comint-buffer nil
  "Comint process buffer.")

(defvar iclj-comint-output-cache '("")
  "Comint output filtered text list.")

(defvar iclj-comint-proc-in-progress nil
  "Indicates if the comint filter function is still in progress.")

(defvar iclj-comint-redirect-handler nil
  "Redirect function handler (callback).")

(defvar iclj-comint-from-buffer nil
  "Previous current buffer.")

(defvar iclj-comint-host-history '()
  "Host history list.")

(defvar iclj-comint-default-port "5555"
  "Port default value.")

(defun iclj-comint-redirect-completed-p ()
  "Return if the PROC/BUFFER redirecting is over."
  ;; comint-redirect-completed is local in relation to the comint-buffer
  (with-current-buffer iclj-comint-buffer
    (null comint-redirect-completed)))

(defun iclj-comint-proc ()
  "Return current comint process."
  ;; get the process (first try)
  (let ((proc (get-buffer-process
               (and (buffer-live-p iclj-comint-buffer)
                    iclj-comint-buffer))))
    ;; if we have a live process return it
    ;; otherwise asks if/and/how should be created
    (if (process-live-p proc) proc
      (let* ((create-flag (yes-or-no-p "Create clojure repl comint process?"))
             (remote-flag (when create-flag
                            (yes-or-no-p "Remote?"))))
        (when create-flag
          (get-buffer-process
           (if (not remote-flag)
               (iclj-comint-run iclj-comint-program)
             (call-interactively 'iclj-comint-remote-run))))))))

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

(defmacro iclj-comint-with-redirect-output (buffer-or-name &rest body)
  "Evaluate the BODY after get redirect BUFFER-OR-NAME output."
  ;; extract output from the buffer
  `(let ((output (iclj-util-buffer-string ,buffer-or-name)))
     ;; update output
     (setq output (if (string= output "") "nil" output))
     ;; evaluate body forms
     ,@body
     ;; clean display function
     (setq iclj-comint-redirect-handler nil
           iclj-comint-from-buffer nil)))

(defun iclj-comint-redirect-dispatch-handler ()
  "Dispatch response function handler."
  ;; call the response handler
  (when iclj-comint-redirect-handler
    (funcall iclj-comint-redirect-handler
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

(defun iclj-comint-send (input)
  "Send INPUT string, using `comint-send-string' function."
  (let ((proc (iclj-comint-proc)))
    ;; check if the process is alive
    (if (not (process-live-p proc))
        (message "[ICLJ]: error, process not found")
      ;; output list should always start empty
      ;; set progress control variable to true
      (setq iclj-comint-output-cache '("")
            iclj-comint-proc-in-progress t)
      ;; send string to the process
      (comint-send-string proc input)
      ;; always send a new line: <return> (implicit)
      (comint-send-string proc "\n"))))

(defun iclj-comint-redirect-input-to-process
    (send-func from-buffer echo &optional no-display buffer-or-name &rest input)
  "Send INPUT FROM-BUFFER to process using the chosen SEND-FUNC.
ECHO non-nil means, show output in comint process buffer.
NO-DISPLAY non-nil means, don't display the default output buffer.
BUFFER-OR-NAME non-nil means, use it as the redirect output buffer (dedicated)."
  (let ((proc (iclj-comint-proc)))
    (if (not (process-live-p proc))
        ;; clean message and leave
        (message nil)
      ;; else:
      (let ((proc-buffer (process-buffer proc))
            (output-buffer (iclj-util-get-buffer-create buffer-or-name)))
        (if (not (buffer-live-p output-buffer))
            (message "[ICLJ]: error, no redirect output buffer available")
          ;; erase output redirect buffer
          (iclj-util-erase-buffer output-buffer)
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
            (or no-display
                (display-buffer
                 (get-buffer-create output-buffer)))))))))

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
  "Helper function to setup `comint-mode' related variables."
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

(defun iclj-comint-read-host-port ()
  "Read host and port."
  (let* ((host-history (or (car iclj-comint-host-history) ""))
         (host-prompt (format
                       (if (string= host-history "")
                           "Host: "
                         "Host[%s]: ")
                       host-history))
         (port-prompt (format "Port[%s]: " iclj-comint-default-port)))
    ;; read the values using the `minibuffer'
    (list (read-string host-prompt nil host-history iclj-comint-host-history)
          (read-string port-prompt nil nil iclj-comint-default-port))))

;;;###autoload
(defun iclj-comint-remote-run (host port)
  "Run an inferior instance of Clojure REPL Server (HOST PORT) inside Emacs."
  (interactive (iclj-comint-read-host-port))
  ;; save host history if necessary
  (unless (member host iclj-comint-host-history)
    (push host iclj-comint-host-history))
  ;; update host and port, if necessary
  (let ((port (if (string= port "") "5555" port)))
    (if (string= host "")
        (message "[Iclj]: Error, missing host value")
      ;; run comint with host/port as the program (open-stream)
      (iclj-comint-run (cons host port)))))

;;;###autoload
(defun iclj-comint-run (program &optional switches)
  "Run an inferior instance of Clojure REPL PROGRAM inside Emacs.
If SWITCHES are supplied, they are passed to PROGRAM.  With prefix argument
\\[universal-argument] prompt for SWITCHES as well as PROGRAM."
  ;; map function arguments
  (interactive
   (list (read-string "Run program: "
                      iclj-comint-program
                      nil
                      iclj-comint-program)
         (and (consp current-prefix-arg)
              (split-string-and-unquote (read-string "Switches: ")))))
  ;; make comint process buffer
  (let ((buffer (apply 'make-comint
                       iclj-comint-buffer-name
                       program
                       iclj-comint-start-file
                       (or switches iclj-comint-program-args))))
    ;; check if the buffer was properly created
    (unless buffer
      (error "[ICLJ]: Error, start process %s: fails" iclj-comint-program))
    ;; check comint process
    (comint-check-proc buffer)
    ;; set process sentinel
    (set-process-sentinel (get-buffer-process buffer)
                          'iclj-comint-proc-sentinel)
    ;; start clojure comint mode
    (with-current-buffer buffer
      (iclj-comint-mode))
    ;; display buffer
    (display-buffer buffer 'display-buffer-pop-up-window)
    ;; cache the process buffer (implicit: return it)
    (setq iclj-comint-buffer buffer)))

(defvar iclj-comint-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    ;; (define-key map (kbd "C-c C-l") #'iclj-op-load-file)
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
  (customize-set-variable 'comint-redirect-verbose nil)
  ;; add dispatch display handler hook
  (add-hook 'comint-redirect-hook
            #'iclj-comint-redirect-dispatch-handler)
  ;; set local paragraph variables
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) iclj-comint-prompt-regexp))

;;;###autoload
(add-hook 'iclj-comint-mode-hook 'iclj-comint-setup)

(provide 'iclj-comint)

;;; iclj-comint.el ends here

