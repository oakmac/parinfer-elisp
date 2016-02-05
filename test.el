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

(print (parinferlib-indent-mode nil nil nil nil))

(princ "\n\n\n")
