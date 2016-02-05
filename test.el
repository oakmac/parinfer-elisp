#!/usr/bin/emacs --script

;; TODO: explain the purpose of this file

(load-file "parinferlib.el")

(defun println (txt)
  (princ (concat txt "\n")))

(defun squiggly-line ()
  (println "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))

(princ "\n\n\n")
(squiggly-line)
(println "Running Parinfer Tests")
(squiggly-line)


(defconst in-text-1 "(defn foo\n  [arg\n  ret")
(defconst out-text-1 "(defn foo\n  [arg]\n  ret)")

(print (parinferlib-indent-mode in-text-1 nil nil nil))

(princ "\n\n\n")
