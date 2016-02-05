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

(defun parinferlib--create-initial-result (text mode cursor-x cursor-line cursor-dx)
  (let ((result (make-hash-table)))
    (puthash :mode mode result)

    (puthash :origText text result)
    (puthash :origLines "TODO: figure out how to split-lines" result)

    (puthash :lines [] result)
    (puthash :lineNo -1 result)
    (puthash :ch "" result)
    (puthash :x 0 result)

    (puthash :parenStack [] result)

    (puthash :parenTrailLineNo nil result)
    (puthash :parenTrailStartX nil result)
    (puthash :parenTrailEndX nil result)
    (puthash :parenTrailOpeners [] result)

    (puthash :cursorX cursor-x result)
    (puthash :cursorLine cursor-line result)
    (puthash :cursorDx cursor-dx result)

    (puthash :isInCode t result)
    (puthash :isEscaping nil result)
    (puthash :isInStr nil result)
    (puthash :isInComment nil result)
    (puthash :commentX nil result)

    (puthash :quoteDanger nil result)
    (puthash :trackingIndent nil result)
    (puthash :skipChar nil result)
    (puthash :success nil result)

    (puthash :maxIndent nil result)
    (puthash :indentDelta 0 result)

    (puthash :errorName nil result)
    (puthash :errorMessage nil result)
    (puthash :errorLineNo nil result)
    (puthash :errorX nil result)

    (puthash :errorPosCache nil result) ;; TODO: this might need to be a hash-map?

    result))

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
  (parinferlib--create-initial-result text :indent cursor-x cursor-line cursor-dx))

(defun parinferlib-paren-mode (text cursor-x cursor-line cursor-dx)
  "Paren Mode public function"
  (parinferlib--create-initial-result text :paren cursor-x cursor-line cursor-dx))

(provide 'parinferlib)

;;; parinfer-lib.el ends here
