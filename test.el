;; This file is part of the parinfer-elisp project:
;; https://github.com/oakmac/parinfer-elisp
;;
;; You can run this file on the command line:
;; emacs --script test.el
;;
;; It will run the Parinfer test suite and output the results to the console.
;; The script will exit with "1" on any test failure. Exit "0" otherwise.

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

(defun convert-result-tabstops (tabstops)
  "Converts tabStops from the library result into a list."
  (mapcar
    (lambda (ts) (list (plist-get ts :ch)
                       (plist-get ts :line-no)
                       (plist-get ts :x)))
    tabstops))

(defun convert-test-tabstops (tabstops)
  "Converts tabStops from the test JSON into a list."
  (mapcar
    (lambda (ts) (list (plist-get ts :ch)
                       (plist-get ts :lineNo)
                       (plist-get ts :x)))
    tabstops))

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
         (out-cursor (plist-get out :cursor))
         (out-error (plist-get out :error))
         (out-tabstops (plist-get out :tabStops))
         (test-id (number-to-string (plist-get in :fileLineNo)))
         (in-text (string-join (plist-get in :lines)))
         (expected-text (string-join (plist-get out :lines)))
         (in-cursor (plist-get in :cursor))
         (cursor-x (plist-get in-cursor :cursorX))
         (cursor-line (plist-get in-cursor :cursorLine))
         (cursor-dx (plist-get in-cursor :cursorDx))
         (preview-cursor-scope (plist-get in-cursor :previewCursorScope))
         (options (list :cursor-x cursor-x
                        :cursor-line cursor-line
                        :cursor-dx cursor-dx
                        :preview-cursor-scope preview-cursor-scope))
         (test-idempotence? (and (not out-error)
                                 (not out-tabstops)
                                 (not cursor-dx)))
         (test-cross-mode? (and (not out-error)
                                (not out-tabstops)
                                (not in-cursor)))
         (result-1 (if (equal :indent mode)
                     (parinferlib-indent-mode in-text options)
                     (parinferlib-paren-mode in-text options)))
         (out-text-1 (plist-get result-1 :text))
         (result-2 (if (equal :indent mode)
                     (parinferlib-indent-mode out-text-1 options)
                     (parinferlib-paren-mode out-text-1 options)))
         (out-text-2 (plist-get result-2 :text))
         (failed? nil))
    ;; in/out text equality
    (when (not (equal out-text-1 expected-text))
      (setq failed? t)
      (print-err (concat mode-string " In/Out text failure: test id " test-id)))

    ;; check cursor-x
    (when (and in-cursor
               (not (equal (plist-get result-1 :cursor-x)
                           (plist-get out-cursor :cursorX))))
      (setq failed? t)
      (print-err (concat mode-string " cursorX In/Out failure: test id " test-id)))

    ;; TODO: check error output

    ;; check tab stops
    (when out-tabstops
      (let ((result-tabstops (convert-result-tabstops (plist-get result-1 :tab-stops)))
            (out-tabstops2 (convert-test-tabstops out-tabstops)))
        (when (not (equal result-tabstops out-tabstops2))
          (setq failed? t)
          (print-err (concat mode-string " Tab Stops failure: test id " test-id)))))

    ;; idempotence
    (when test-idempotence?
      (when (not (equal out-text-2 expected-text))
        (setq failed? t)
        (print-err (concat mode-string " Idempotence failure: test id " test-id))))

    ;; cross-mode preservation
    (when test-cross-mode?
      (let* ((result-3 (if (equal :indent mode)
                         (parinferlib-paren-mode out-text-1 options)
                         (parinferlib-indent-mode out-text-1 options)))
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

;; exit with "1" on failure
;; NOTE: this is necessary for travis-ci
(when (not (equal num-tests-failed 0))
  (kill-emacs 1))
