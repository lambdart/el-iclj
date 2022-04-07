;;; iclj-op-table.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/iclj-op-vars.el
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

(defvar iclj-op-eldoc-fmt
  "(let [m (clojure.core/meta #'%s)]
     (flatten
        (list (str (get m :name))
              (str (first (get m :arglists)))
              (get m :doc))))"
  "Eldoc operation format.")

(defvar iclj-ns-list-fmt
  "(clojure.core/->>
     (clojure.core/all-ns)
     (clojure.core/map clojure.core/ns-name)
     (clojure.core/map name))"
  "Return list all of available namespaces.")

(defvar iclj-trace-ns-fmt
  "(use 'clojure.tools.trace)
(clojure.tools.trace/trace-ns %s)"
  "Trace chosen namescape format.")

(defvar iclj-op-all-ns-fmt
  "(clojure.pprint/pprint
     (clojure.core/->>
       (clojure.core/all-ns)
       (clojure.core/map clojure.core/ns-name)
       (clojure.core/map name)))"
  "List all available namespaces.")

(defvar iclj-op-table
  `((input          . (:fmt "%s"))
    (eval           . (:fmt "%s"))
    (eval-last-sexp . (:cb iclj-eval-handler :fmt "%s" :waitp nil))
    (doc            . (:fmt "(clojure.repl/doc %s)"))
    (find-doc       . (:fmt "(clojure.repl/find-doc %S)"))
    (run-tests      . (:fmt "(clojure.test/run-tests)"))
    (eldoc          . (:cb iclj-eldoc-handler :fmt ,iclj-op-eldoc-fmt))
    (apropos        . (:cb iclj-apropos-handler
                           :fmt "(sort (clojure.repl/apropos %S))"
                           :buf ""
                           :waitp t))
    (source         . (:fmt "(clojure.repl/source %s)"))

    (meta           . (:fmt "(clojure.pprint/pprint (clojure.core/meta #'%s))"))
    (macroexpand    . (:fmt "(clojure.pprint/pprint (clojure.core/macroexpand '%s))"))
    (macroexpand-1  . (:fmt "(clojure.pprint/pprint (clojure.core/macroexpand-1 '%s))"))
    (all-ns         . (:fmt ,iclj-op-all-ns-fmt))
    (ns-vars        . (:fmt "(clojure.repl/dir %s)"))
    (ns-list        . (:fmt ,iclj-ns-list-fmt :cb iclj-ns-list-handler :waitp t))
    (set-ns         . (:fmt "(clojure.core/in-ns '%s)"))
    (trace-ns       . (:fmt ,iclj-trace-ns-fmt)))
  "Operation associative list: (OP-KEY . (OP-PLIST))
OP-KEY, the operation key selector.
OP-PLIST, response handler, operation format string and
dedicated output buffer.")

(defun iclj-op-table-get-plist (op-key)
  "Get the property list from the operation OP-KEY table."
  (cdr (assoc op-key iclj-op-table))) ; select operation

(defun iclj-op-table-get-property (op-key property)
  "Get property list element from the operation table.
OP-KEY, operation key property list selector.
PROPERTY, possible values are: :cb, :fmt and :buf."
  (plist-get (iclj-op-table-get-plist op-key) property))

(provide 'iclj-op-table)

;;; iclj-op-table.el ends here

