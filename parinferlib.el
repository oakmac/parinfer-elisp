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

;; TODO: prefix these with parinferlib-- ?
(defconst BACKSLASH "\\")
(defconst BLANK_SPACE " ")
(defconst DOUBLE_SPACE "  ")
(defconst DOUBLE_QUOTE "\"")
(defconst NEWLINE "\n")
(defconst SEMICOLON ";")
(defconst TAB "\t")

(defun parinferlib--open-paren? (ch)
  (or (string= "(" ch)
      (string= "{" ch)
      (string= "[" ch)))

(defun parinferlib--close-paren? (ch)
  (or (string= ")" ch)
      (string= "}" ch)
      (string= "]" ch)))

;;------------------------------------------------------------------------------
;; Result Structure
;;------------------------------------------------------------------------------

(defun parinferlib--create-initial-result (text mode cursor-x cursor-line cursor-dx)
  (let ((result (make-hash-table)))
    (puthash :mode mode result)

    (puthash :origText text result)
    (puthash :origLines (vconcat (split-string text NEWLINE)) result)

    (puthash :lines [] result)
    (puthash :lineNo -1 result)
    (puthash :ch "" result)
    (puthash :x 0 result)

    (puthash :parenStack '() result)

    (puthash :parenTrailLineNo nil result)
    (puthash :parenTrailStartX nil result)
    (puthash :parenTrailEndX nil result)
    (puthash :parenTrailOpeners '() result)

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

;; TODO: write this section

;;------------------------------------------------------------------------------
;; String Operations
;;------------------------------------------------------------------------------

(defun parinferlib--insert-within-string (orig idx insert)
  (concat (substring orig 0 idx)
          insert
          (substring orig idx)))

(defun parinferlib--replace-within-string (orig start end replace)
  (concat (substring orig 0 start)
          replace
          (substring orig end)))

(defun parinferlib--remove-within-string (orig start end)
  (concat (substring orig 0 start)
          (substring orig end)))

;;------------------------------------------------------------------------------
;; Line Operations
;;------------------------------------------------------------------------------

(defun parinferlib--insert-within-line (result lineNo idx insert)
  (let (lines (gethash :lines result)
        line (aref lines lineNo)
        new-line (parinferlib--insert-within-string line idx insert)
        new-lines (aset lines lineNo new-line))
    (puthash :lines new-lines result)))

(defun parinferlib--replace-within-line (result lineNo start end replace)
  (let (lines (gethash :lines result)
        line (aref lines lineNo)
        new-line (parinferlib--replace-within-string line start end replace)
        new-lines (aset lines lineNo new-line))
    (puthash :lines new-lines result)))

(defun parinferlib--remove-within-line (result lineNo start end)
  (let (lines (gethash :lines result)
        line (aref lines lineNo)
        new-line (parinferlib--remove-within-string line start end)
        new-lines (aset lines lineNo new-line))
    (puthash :lines new-lines result)))

(defun parinferlib--init-line (result line)
  (let (current-line-no (gethash :lineNo result)
        lines (gethash :lines result)
        new-lines (vconcat lines [line]))
    (puthash :x 0 result)
    (puthash :lineNo (1+ current-line-no) result)
    (puthash :lines new-lines result)

    ;; reset line-specific state
    (puthash :commentX nil result)
    (puthash :indentDelta 0 result)))

;;------------------------------------------------------------------------------
;; Util
;;------------------------------------------------------------------------------

;; TODO: write this section

;;------------------------------------------------------------------------------
;; Character functions
;;------------------------------------------------------------------------------

;; TODO: write this section

;;------------------------------------------------------------------------------
;; Cursor functions
;;------------------------------------------------------------------------------

;; TODO: write this section

;;------------------------------------------------------------------------------
;; Paren Trail functions
;;------------------------------------------------------------------------------

;; TODO: write this section

;;------------------------------------------------------------------------------
;; Indentation functions
;;------------------------------------------------------------------------------

;; TODO: write this section

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
