;;; parinferlib.el --- a Parinfer implementation in Emacs Lisp
;; v0.1.0
;; https://github.com/oakmac/parinferlib-emacs
;;
;; More information about Parinfer can be found here:
;; http://shaunlebron.github.io/parinfer/
;;
;; Copyright (c) 2016, Chris Oakman and other contributors
;; Released under the ISC license
;; https://github.com/oakmac/parinferlib-emacs/blob/master/LICENSE.md

;;; TODO: figure out the best approach for the header comment here
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging-Basics.html

;;------------------------------------------------------------------------------
;; Constants / Predicates
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Result Structure
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Errors
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; String Operations
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Line Operations
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Util
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Character functions
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Cursor functions
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Paren Trail functions
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Indentation functions
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Indentation functions
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; High-level processing functions
;;------------------------------------------------------------------------------

(defun parinferlib--process-char (result ch)
  nil)

(defun parinferlib--process-line (result line)
  nil)

(defun parinferlib--process-error (result err)
  nil)

(defun parinferlib--process-text (text options mode)
  nil)

;;------------------------------------------------------------------------------
;; Public API
;;------------------------------------------------------------------------------

(defun parinferlib-indent-mode (text cursor-x cursor-line cursor-dx)
  "Indent Mode public function."
  (message "Hello from parinferlib!!! How's the weather there?"))

(defun parinferlib-paren-mode (text cursor-x cursor-line cursor-dx)
  "Paren Mode public function"
  nil)

(provide 'parinferlib)

;;; parinfer-lib.el ends here
