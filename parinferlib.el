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

;; TODO: write about the differences between this implementation and parinfer.js
;; - using vectors for lines
;; - using lists for stacks
;; - using vector for stack elements

;; A Stack Element is a Vector of 4 items (alphabetically ordered)
;; idx : item
;;   0 : ch
;;   1 : indentDelta
;;   2 : lineNo
;;   3 : x

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

(defconst CH_IDX 0)
(defconst INDENT_DELTA_IDX 1)
(defconst LINE_NO_IDX 2)
(defconst X_IDX 3)

(defconst PARENS (make-hash-table :test 'equal))
(puthash "{" "}" PARENS)
(puthash "}" "{" PARENS)
(puthash "[" "]" PARENS)
(puthash "]" "[" PARENS)
(puthash "(" ")" PARENS)
(puthash ")" "(" PARENS)

(defun parinferlib--zero? (x)
  (eq 0 x))

(defun parinferlib--empty? (stack)
  (parinferlib--zero? (lenth stack)))

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
  (let ((lines-vector (vconcat (split-string text NEWLINE)))
        (result (make-hash-table)))
    (puthash :mode mode result)

    (puthash :origText text result)
    (puthash :origLines lines-vector result)

    (puthash :lines (make-vector (length lines-vector) nil) result)
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
  (let* ((lines (gethash :lines result))
         (line (aref lines lineNo))
         (new-line (parinferlib--insert-within-string line idx insert))
         (new-lines (aset lines lineNo new-line)))
    (puthash :lines new-lines result)
    nil))

(defun parinferlib--replace-within-line (result lineNo start end replace)
  (let* ((lines (gethash :lines result))
         (line (aref lines lineNo))
         (new-line (parinferlib--replace-within-string line start end replace))
         (new-lines (aset lines lineNo new-line)))
    (puthash :lines new-lines result)
    nil))

(defun parinferlib--remove-within-line (result lineNo start end)
  (let* ((lines (gethash :lines result))
         (line (aref lines lineNo))
         (new-line (parinferlib--remove-within-string line start end))
         (new-lines (aset lines lineNo new-line)))
    (puthash :lines new-lines result)
    nil))

(defun parinferlib--init-line (result line)
  (let* ((current-line-no (gethash :lineNo result))
         (new-line-no (1+ current-line-no))
         (lines (gethash :lines result))
         (new-lines (aset lines new-line-no line)))
    (puthash :x 0 result)
    (puthash :lineNo new-line-no result)
    (puthash :lines new-lines result)

    ;; reset line-specific state
    (puthash :commentX nil result)
    (puthash :indentDelta 0 result)
    nil))

;; if the current character has changed, commit it's change to the current line
(defun parinferlib--commit-char (result orig-ch)
  (let* ((line-no (gethash :lineNo result))
         (x (gethash :x result))
         (ch (gethash :ch result))
         (ch-length (length ch))
         (orig-ch-length (length orig-ch)))
    (when (not (string= orig-ch ch))
      (parinferlib--replace-within-line result line-no x (+ x orig-ch-length) ch))
    (puthash :x (+ x ch-length) result)
    nil))

;;------------------------------------------------------------------------------
;; Util
;;------------------------------------------------------------------------------

(defun parinferlib--clamp (val min-n max-n)
  (when min-n
    (setq val (if (> min-n val) min-n val)))
  (when max-n
    (setq val (if (< max-n val) max-n val)))
  val)

;; TODO: figure out what data structure to use for the stack
;;       probably a list?
(defun parinferlib--peek (a)
  nil)

;;------------------------------------------------------------------------------
;; Character functions
;;------------------------------------------------------------------------------

(defun parinferlib--valid-close-paren? (paren-stack ch)
  (if (parinferlib--empty? paren-stack)
    nil
    (let* ((top-of-stack (car paren-stack))
           (top-of-stack-ch (aref top-of-stack CH_IDX)))
      (string= top-of-stack-ch (gethash ch PARENS)))))

(defun parinferlib--on-open-paren (result)
  (when (gethash :isInCode result)
    (let* ((new-stack-el (vector (gethash :ch result)
                                 (gethash :indentDelta result)
                                 (gethash :lineNo result)
                                 (gethash :x result)))
           (paren-stack (gethash :parenStack result))
           (new-paren-stack (cons new-stack-el paren-stack)))
      (puthash :parenStack new-paren-stack result)))
  nil)

(defun parinferlib--on-matched-close-paren (result)
  (let* ((paren-stack (gethash :parenStack result))
         (opener (pop paren-stack))
         (opener-x (aref opener X_IDX))
         (result-x (gethash :x result))
         (openers (gethash :parenTrailOpeners result))
         (new-openers (cons opener openers)))
    (puthash :parenTrailEndX (+1 result-x) result)
    (puthash :parenTrailOpeners new-openers result)
    (puthash :maxIndent opener-x result)
    ;; the first element of paren-stack was removed when we called "pop" earlier
    (puthash :parenStack paren-stack result))
  nil)

(defun parinferlib--on-unmatched-close-paren (result)
  (puthash :ch "" result)
  nil)

(defun parinferlib--on-close-paren (result)
  (when (gethash :isInCode result)
    (if (parinferlib--valid-close-paren? (gethash :parenStack result) (gethash :ch result))
      (parinferlib--on-matched-close-paren result)
      (parinferlib--on-unmatched-close-paren result)))
  nil)

(defun parinferlib--on-tab (result)
  (when (gethash :isInCode result)
    (puthash :ch DOUBLE_SPACE result))
  nil)

(defun parinferlib--on-semicolon (result)
  (when (gethash :isInCode result)
    (puthash :isInComment t result)
    (puthash :commentX (gethash :x result) result))
  nil)

(defun parinferlib--on-newline (result)
  (puthash :isInComment nil result)
  (puthash :ch "" result)
  nil)

(defun parinferlib--on-quote (result)
  ;; TODO: write me; figure out throw
  nil)

(defun parinferlib--on-backslash (result)
  (puthash :isEscaping t result)
  nil)

(defun parinferlib--after-backslash (result)
  (puthash :isEscaping nil result)
  (when (string= (gethash :ch result) NEWLINE)
    ;; TODO: figure out throw here
    (parinferlib--on-newline result))
  nil)

(defun parinferlib--on-char (result)
  (let ((ch (gethash :ch result)))
    (cond ((gethash :isEscaping result)   (parinferlib--after-backslash result))
          ((parinferlib--open-paren? ch)  (parinferlib--on-open-paren result))
          ((parinferlib--close-paren? ch) (parinferlib--on-close-paren result))
          ((string= ch DOUBLE_QUOTE) (parinferlib--on-quote result))
          ((string= ch SEMICOLON)    (parinferlib--on-semicolon result))
          ((string= ch BACKSLASH)    (parinferlib--on-backslash result))
          ((string= ch TAB)          (parinferlib--on-tab result))
          ((string= ch NEWLINE)      (parinferlib--on-newline result))))
  (let ((in-comment? (gethash :isInComment result))
        (in-string? (gethash :isInStr result)))
    (puthash :isInCode (and (not in-comment?) (not in-string?))) result)
  nil)

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
  (let ((result (parinferlib--create-initial-result text :indent cursor-x cursor-line cursor-dx)))
    (parinferlib--on-open-paren result)
    (parinferlib--on-open-paren result)
    (parinferlib--on-open-paren result)
    (parinferlib--on-open-paren result)
    result))

(defun parinferlib-paren-mode (text cursor-x cursor-line cursor-dx)
  "Paren Mode public function"
  (parinferlib--create-initial-result text :paren cursor-x cursor-line cursor-dx))

(provide 'parinferlib)

;;; parinfer-lib.el ends here
