#!/usr/bin/emacs --script

;; TODO: document this file

(require 'json)
(load-file "parinferlib.el")

(setq debug-on-error t)

;;------------------------------------------------------------------------------
;; Util functions
;;------------------------------------------------------------------------------

(defun get-string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun println (txt)
  (princ (concat txt "\n")))

(defun squiggly-line ()
  (println "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))

(defun string-join (l)
  (mapconcat 'identity l "\n"))

;;------------------------------------------------------------------------------
;; Load test files
;;------------------------------------------------------------------------------

(defconst indent-mode-tests
  (let ((test-str (get-string-from-file "tests/indent-mode.json"))
        (json-object-type 'plist))
    (json-read-from-string test-str)))

(defconst paren-mode-tests
  (let ((test-str (get-string-from-file "tests/paren-mode.json"))
        (json-object-type 'plist))
    (json-read-from-string test-str)))

;;------------------------------------------------------------------------------
;; Run the tests
;;------------------------------------------------------------------------------

(princ "\n\n")
(squiggly-line)
(println "Running Parinfer Tests")
(squiggly-line)

(defun run-test (mode test)
  (let* ((in (plist-get test :in))
         (out (plist-get test :out))
         (test-id (number-to-string (plist-get in :fileLineNo)))
         (in-text (string-join (plist-get in :lines)))
         (expected-text (string-join (plist-get out :lines)))
         (cursor (plist-get in :cursor))
         (cursor-x (plist-get cursor :cursorX))
         (cursor-line (plist-get cursor :cursorLine))
         (cursor-dx (plist-get cursor :cursorDx))

         (result-1 (if (equal :indent mode)
                     (parinferlib-indent-mode in-text cursor-x cursor-line cursor-dx)
                     (parinferlib-paren-mode in-text cursor-x cursor-line cursor-dx)))
         (out-text-1 (nth 1 result-1))

         (result-2 (if (equal :indent mode)
                     (parinferlib-indent-mode out-text-1 cursor-x cursor-line cursor-dx)
                     (parinferlib-paren-mode out-text-1 cursor-x cursor-line cursor-dx)))
         (out-text-2 (nth 1 result-2)))
    ;; in/out text equality
    (when (not (equal out-text-1 expected-text))
      ;; TODO: figure out how to print to stderr
      (print (concat "In/Out text failure: test id " test-id)))

    ;; idempotence
    (when (not (equal out-text-2 expected-text))
      (print (concat "Idempotence failure: test id " test-id)))))

    ;; cross-mode preservation
    ;; TODO: write this

(mapc (lambda (test) (run-test :indent test)) indent-mode-tests)
(mapc (lambda (test) (run-test :paren test)) paren-mode-tests)


; (defconst in-text-1 "(defn foo\n  [arg\n  ret")
; (defconst out-text-1 "(defn foo\n  [arg]\n  ret)")
; (defconst paren-test-1 "(let [foo 1]\nfoo)")
; (print (parinferlib-paren-mode paren-test-1 nil nil nil))
; (print (parinferlib-indent-mode in-text-1 nil nil nil))
; (parinferlib-indent-mode in-text-1 nil nil nil)

(princ "\n\n")
