;;; iclj-tq.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/iclj-tq.el
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

(require 'iclj-util nil t)

(defvar iclj-tq-subprompt-regexp " *#_=> *"
  "Regexp to recognize subprompts in the Inferior Clojure mode.")

(defvar iclj-tq-prompt-regexp "\\( *#_\\|[^=> \n]+\\)=> *"
  "Regexp to recognize both main prompt and subprompt for comint.")

(defvar iclj-tq nil
  "Default tq head cache.")

(defvar iclj-tq-eoc "\n--ICLJ-EOC-INDICATOR--\n"
  "Default end of command indicator.")

(defvar iclj-tq-stream-name
  " *iclj-repl-server-stream*"
  "Default stream name.")

(defun iclj-tq-queue (tq)
  "Fetch TQ queue.
Looks like (queue proc . eoc)."
  (car tq))

(defun iclj-tq-proc (tq)
  "Fetch TQ process."
  (cadr tq))

(defun iclj-tq-proc-eoc (tq)
  "Fetch TQ process end of command indicator."
  (cddr tq))

(defun iclj-tq-queue-head-input (tq)
  "Fetch TQ question.
The structure of `queue' is as follows:
\((input regexp closure . (handler . buffer))).
Input: string to send to the process."
  (caar (iclj-tq-queue tq)))

(defun iclj-tq-queue-head-waitp (tq)
  "Fetch TQ wait response predicate."
  (cadar (iclj-tq-queue tq)))

(defun iclj-tq-queue-head-handler (tq)
  "Fetch TQ handler.
Handler: function to call upon receiving a complete response from the process."
  (caddar (iclj-tq-queue tq)))

(defun iclj-tq-queue-head-orig-buffer (tq)
  "Fetch TQ origin buffer."
  (car (cdddar (iclj-tq-queue tq))))

(defun iclj-tq-queue-head-temp-buffer (tq)
  "Fetch TQ temporary buffer.
Buffer: process output buffer."
  (cdr (cdddar (iclj-tq-queue tq))))

(defun iclj-tq-queue-empty-p (tq)
  "Return non-nil if queue (TQ) is empty."
  (not (iclj-tq-queue tq)))

;;; Core functionality

(defun iclj-tq-filter-chunk (string)
  "Return filtered STRING, i.e, prompt is removed."
  (format "%s"
          (if (and string (stringp string))
              ;; remove prompt
              (replace-regexp-in-string iclj-tq-prompt-regexp
                                        ""
                                        string)
            "")))

(defun iclj-tq-call-handler (tq)
  "Call TQ function handler."
  (let ((func (iclj-tq-queue-head-handler tq)))
    ;; call function handler
    (funcall func
             (iclj-tq-queue-head-temp-buffer tq)
             (iclj-tq-queue-head-orig-buffer tq))))

(defun iclj-tq-filter (tq string)
  "Cache TQ output STRING."
  ;; analyse if output has the end of request indicator (prompt regexp)
  (let ((chunk (iclj-tq-filter-chunk string)))
    ;; skip if chunk is empty
    (unless (string-empty-p chunk)
      (let ((buffer (iclj-tq-queue-head-temp-buffer tq))
            (inhibit-read-only t))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            ;; go to point max before insertion
            (goto-char (point-max))
            ;; insert the output chunk the temporary output buffer
            (insert chunk))))
      ;; logs if EOC was found (debugging)
      (when (string-match-p (car (iclj-tq-proc-eoc tq)) chunk)
        (iclj-tq-log "process: end of command!")
        ;; call the handler if it's waiting
        (when (iclj-tq-queue-head-waitp tq)
          (iclj-tq-call-handler tq))
        ;; remove queue head (operation)
        (iclj-tq-queue-pop tq)))))

;;;###autoload
(defun iclj-tq--make (process eoc)
  "Make and return a transaction queue that communicates with PROCESS.
PROCESS should be a sub process capable of sending and receiving
streams of bytes.  It may be a local process, or it may be connected
to a tcp server on another machine. EOC (end of command) is used to indicate the
end of command."
  (let ((tq `(nil ,process ,eoc)))
    (set-process-filter process
                        (lambda (_p s)
                          (iclj-tq-filter tq s)))
    tq))

(defun iclj-tq-queue-add (tq
                          input
                          waitp
                          handler
                          orig-buffer
                          temp-buffer)
  "Add queue element: (INPUT WAIT-RESP-P HANDLER ORIG-BUFFER TEMP-BUFFER)
to the TQ head."
  (setcar tq
          (nconc (iclj-tq-queue tq)
                 `((,input
                    ,waitp
                    ,handler
                    ,orig-buffer .
                    ,temp-buffer))))
  t)

(defun iclj-tq-proc-send-input (tq)
  "Send TQ input using the correct process-send function."
  (let ((process (iclj-tq-proc tq))
        (input (iclj-tq-queue-head-input tq))
        (waitp (iclj-tq-queue-head-waitp tq)))
    ;; send region or string to the process
    (process-send-string process (concat input "\n"))
    ;; when waitp is non-nil do not wait for the response,
    ;; call the handler ASAP
    (when (not waitp)
      (iclj-tq-call-handler tq))))

(defun iclj-tq-queue-head-kill-temp-buffer (tq)
  "Kill temporary output buffer if TQ queue head waitp is non-nil."
  (when (iclj-tq-queue-head-waitp tq)
    (kill-buffer (iclj-tq-queue-head-temp-buffer tq))))

(defun iclj-tq-queue-pop (tq)
  "Pop TQ queue element."
  (iclj-tq-queue-head-kill-temp-buffer tq)
  ;; update TQ car
  (setcar tq (cdr (car tq)))
  ;; send next queue operation (if any)
  (or (iclj-tq-queue-empty-p tq)
      (iclj-tq-proc-send-input tq)))

(defun iclj-tq-enqueue (tq
                        input
                        waitp
                        handler
                        orig-buffer
                        &optional delay-question)
  "Add a transaction to transaction queue TQ.
This sends the INPUT string to the process that TQ communicates with.

If WAITP is non-nil we call the HANDLER function ASAP, otherwise we
wait for the response end of command indicator.

The HANDLER is called passing two arguments: the response and
the ORIG-BUFFER.

If DELAY-QUESTION is non-nil, delay sending this question until
the process has finished replying to any previous questions.
This produces more reliable results with some processes."
  (let ((sendp (or (not delay-question)
                   (not (iclj-tq-queue tq)))))
    ;; add queue to the transmission queue
    (iclj-tq-queue-add tq
                       input
                       waitp
                       handler
                       orig-buffer
                       (iclj-util-get-buffer-create
                        (generate-new-buffer-name "*iclj-proc-output*")))
    ;; send queue to process
    (when sendp
      (iclj-tq-proc-send-input tq))))

(defun iclj-tq-proc-delete (tq)
  "Shut down transaction queue TQ, terminating the process."
  (delete-process (iclj-tq-proc tq)))

(defun iclj-tq-open (host port eoc)
  "Open transaction queue using the HOST/PORT to create the process.
EOC is the end of command indicator (arbitrary)."
  (setq iclj-tq
        (let* ((proc (open-network-stream iclj-tq-stream-name
                                          nil
                                          host
                                          port
                                          :return-list nil
                                          :type 'network
                                          :nogreeting t
                                          :nowait nil))
               (tq (iclj-tq--make proc eoc)))
          ;; set our process filter
          (set-process-filter proc
                              `(lambda (p s) (iclj-tq-filter ',tq s)))
          ;; return transaction queue
          tq)))

(defvar iclj-tq-host-history '()
  "Host history list.")

(defvar iclj-tq-default-port "5555"
  "Default port number.")

(defun iclj-tq-read-port (&optional default-port)
  "Read port, when DEFAULT-PORT is non-nil suggest it."
  (let* ((fmt (if default-port "Port[%s]: " "Port: %s"))
         (port (read-string (format fmt (or default-port ""))
                            nil
                            nil
                            default-port)))
    ;; return the choose port or the default one
    (if (string= port "")
        default-port
      port)))

(defun iclj-tq-read-host ()
  "Read host and port."
  (let* ((host-history (or (car-safe iclj-tq-host-history) ""))
         (host-prompt (format
                       (if (string= host-history "")
                           "Host: "
                         "Host[%s]: ")
                       host-history)))
    ;; read the values using the `minibuffer'
    (read-string host-prompt
                 nil
                 host-history
                 iclj-tq-host-history)))

(defun iclj-tq-log (msg)
  "Show log MSG to the user and return nil."
  (progn (message "%s" (concat "[ICLJ-TQ]: " msg))
         (message nil)
         nil))

;;;###autoload
(defun iclj-tq-make (host port)
  "Make TQ Clojure repl server stream using the (HOST PORT) parameters."
  (interactive (list
                (iclj-tq-read-host)
                (iclj-tq-read-port iclj-tq-default-port)))
  ;; maybe cache host to be used again
  (unless (member host iclj-tq-host-history)
    (push host iclj-tq-host-history))
  ;; update host and port, if necessary
  (cond ((string= host "")
         (iclj-tq-log "error: missing host value!"))
        ;; default: tries open host/port stream and cache it
        (t (condition-case err
               (iclj-tq-open host port iclj-tq-eoc)
             ;; handle errors
             (error (iclj-tq-log (concat "error: " (cadr err))))
             ;; handle success
             (:success (iclj-tq-log "success: TQ created!"))))))

;;;###autoload
(defun iclj-tq-erase ()
  "Erase current transmission queue."
  (interactive)
  ;; erase if necessary
  (setq iclj-tq (progn (and iclj-tq (iclj-tq-proc-delete iclj-tq) nil)))
  ;; just a hint to the user
  (iclj-tq-log "TQ erased!"))

(defun iclj-tq-send (input waitp handler &optional orig-buffer)
  "This sends the INPUT string to the process.

If WAITP is non-nil we call the HANDLER function ASAP, otherwise we
wait for the response end of command indicator.

The HANDLER is called passing two arguments: the response and
the ORIG-BUFFER."
  ;; make transaction queue if necessary
  (unless iclj-tq (call-interactively 'iclj-tq-make))
  ;; and enqueue the operation
  (when iclj-tq
    (iclj-tq-enqueue iclj-tq
                     input
                     ;; indicates if we should wait for the end of command
                     ;; indicator: if non-nil call the handler after the EOC
                     ;; insertion, otherwise call it ASAP
                     waitp
                     ;; handler: function that will handler the operation
                     handler
                     ;; origin buffer (optional)
                     orig-buffer
                     ;; delay send
                     t)))

(provide 'iclj-tq)

;;; iclj-tq.el ends here
