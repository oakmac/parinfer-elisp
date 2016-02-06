#!/usr/bin/env emacs --script

;; TODO: document this file

(require 'json)
(load-file "parinferlib.el")

;; NOTE: useful when debugging a failed test
;; (setq debug-on-error t)

;;------------------------------------------------------------------------------
;; Util functions
;;------------------------------------------------------------------------------

(defun get-string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun print-err (msg)
  (message msg))

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
;; Test runner
;;------------------------------------------------------------------------------

(defvar num-tests-failed 0)
(defvar num-tests-ran 0)

(defun run-test (mode test)
  (let* ((mode-string (if (equal :indent mode) "Indent Mode" "Paren Mode"))
         (in (plist-get test :in))
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
         (out-text-1 (plist-get result-1 :text))

         (result-2 (if (equal :indent mode)
                     (parinferlib-indent-mode out-text-1 cursor-x cursor-line cursor-dx)
                     (parinferlib-paren-mode out-text-1 cursor-x cursor-line cursor-dx)))
         (out-text-2 (plist-get result-2 :text))
         (failed? nil))
    ;; in/out text equality
    (when (not (equal out-text-1 expected-text))
      (setq failed? t)
      (print-err (concat mode-string " In/Out text failure: test id " test-id)))

    ;; idempotence
    (when (not (equal out-text-2 expected-text))
      (setq failed? t)
      (print-err (concat mode-string " Idempotence failure: test id " test-id)))

    ;; cross-mode preservation
    (when (and (not cursor-x)
               (not cursor-line)
               (not cursor-dx))
      (let* ((result-3 (if (equal :indent mode)
                         (parinferlib-paren-mode out-text-1 nil nil nil)
                         (parinferlib-indent-mode out-text-1 nil nil nil)))
             (out-text-3 (plist-get result-3 :text)))
        (when (not (equal out-text-3 expected-text))
          (setq failed? t)
          (print-err (concat mode-string " cross-mode preservation: test id " test-id)))))

    ;; increment the test counts
    (setq num-tests-ran (1+ num-tests-ran))
    (when failed?
      (setq num-tests-failed (1+ num-tests-failed)))))

;;------------------------------------------------------------------------------
;; Run the tests and print the result
;;------------------------------------------------------------------------------

(princ "\n\n")
(squiggly-line)
(println "Running Parinfer Tests...")
(squiggly-line)

(mapc (lambda (test) (run-test :indent test)) indent-mode-tests)
(mapc (lambda (test) (run-test :paren test)) paren-mode-tests)

(squiggly-line)
(let ((done-msg (if (equal 0 num-tests-failed) "SUCCESS! " "Done. ")))
  (println (concat done-msg
                   "Ran " (number-to-string num-tests-ran) " tests. "
                   (number-to-string num-tests-failed) " failures.")))
(squiggly-line)
(princ "\n\n")
