;;; parinferlib.el --- A Parinfer implementation in Emacs Lisp  -*- lexical-binding: t; -*-
;;
;; Author: Chris Oakman
;; Homepage: https://github.com/oakmac/parinfer-elisp
;; Version: 1.1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: parinfer, extensions
;;
;;; Commentary:
;;
;; More information about Parinfer can be found here:
;; http://shaunlebron.github.io/parinfer/
;;
;; Copyright (c) 2016, Chris Oakman
;; Released under the ISC license
;; https://github.com/oakmac/parinfer-elisp/blob/master/LICENSE.md

;; NOTE: everything is namespaced under `parinferlib` with the assumption that
;;       Emacs extensions might use `parinfer`

;;; Code:

;;------------------------------------------------------------------------------
;; Constants / Predicates
;;------------------------------------------------------------------------------

(defconst parinferlib--BACKSLASH "\\")
(defconst parinferlib--BLANK_SPACE " ")
(defconst parinferlib--DOUBLE_SPACE "  ")
(defconst parinferlib--DOUBLE_QUOTE "\"")
(defconst parinferlib--NEWLINE "\n")
(defconst parinferlib--LINE_ENDING_REGEX "\r?\n")
(defconst parinferlib--SEMICOLON ";")
(defconst parinferlib--TAB "\t")

;; A Stack Element is a Vector of 4 items (alphabetically ordered)
;; idx : item
;;   0 : ch
;;   1 : indentDelta
;;   2 : lineNo
;;   3 : x
;; A Stack Element has four fields:
;; - ch
;; - indent-delta
;; - line-no
;; - x

(defsubst parinferlib--stack-elem (ch indent-delta line-no x)
  "Create a Stack Element.
A Stack Element has four fields: CH, INDENT-DELTA, LINE-NO, and X."
  (declare (side-effect-free t))
  (vector ch indent-delta line-no x))
(defsubst parinferlib--stack-elem-ch (elem)
  "Access slot \"ch\" of `parinferlib--stack-elem' struct ELEM."
  (declare (side-effect-free t))
  (aref elem 0))
(defsubst parinferlib--stack-elem-indent-delta (elem)
  "Access slot \"indent-delta\" of `parinferlib--stack-elem' struct ELEM."
  (declare (side-effect-free t))
  (aref elem 1))
(defsubst parinferlib--stack-elem-line-no (elem)
  "Access slot \"line-no\" of `parinferlib--stack-elem' struct ELEM."
  (declare (side-effect-free t))
  (aref elem 2))
(defsubst parinferlib--stack-elem-x (elem)
  "Access slot \"x\" of `parinferlib--stack-elem' struct ELEM."
  (declare (side-effect-free t))
  (aref elem 3))

;; determines if a line only contains a Paren Trail (possibly w/ a comment)
(defconst parinferlib--STANDALONE_PAREN_TRAIL "^[][:space:])}]*\\(;.*\\)?$")

(defconst parinferlib--PARENS (make-hash-table :test 'equal))
(puthash "{" "}" parinferlib--PARENS)
(puthash "}" "{" parinferlib--PARENS)
(puthash "[" "]" parinferlib--PARENS)
(puthash "]" "[" parinferlib--PARENS)
(puthash "(" ")" parinferlib--PARENS)
(puthash ")" "(" parinferlib--PARENS)

(defun parinferlib--open-paren? (ch)
  "Is CH an open paren?"
  (member ch '("(" "{" "[")))

(defun parinferlib--close-paren? (ch)
  "Is CH a close paren?"
  (member ch '(")" "}" "]")))

;;------------------------------------------------------------------------------
;; State
;;------------------------------------------------------------------------------

;; State is always reset by the entry points calling process-text, so
;; we don't need to use buffer local variables.
(defvar parinferlib--mode nil)
(defvar parinferlib--origText nil)
(defvar parinferlib--origLines nil)
(defvar parinferlib--origCursorX nil)
(defvar parinferlib--lines nil)
(defvar parinferlib--lineNo nil)
(defvar parinferlib--ch nil)
(defvar parinferlib--x nil)
(defvar parinferlib--parenStack nil)
(defvar parinferlib--tabStops nil)
(defvar parinferlib--parenTrailLineNo nil)
(defvar parinferlib--parenTrailStartX nil)
(defvar parinferlib--parenTrailEndX nil)
(defvar parinferlib--parenTrailOpeners nil)
(defvar parinferlib--cursorX nil)
(defvar parinferlib--cursorLine nil)
(defvar parinferlib--cursorDx nil)
(defvar parinferlib--previewCursorScope nil)
(defvar parinferlib--canPreviewCursorScope nil)
(defvar parinferlib--isInCode nil)
(defvar parinferlib--isEscaping nil)
(defvar parinferlib--isInStr nil)
(defvar parinferlib--isInComment nil)
(defvar parinferlib--commentX nil)
(defvar parinferlib--quoteDanger nil)
(defvar parinferlib--trackingIndent nil)
(defvar parinferlib--skipChar nil)
(defvar parinferlib--success nil)
(defvar parinferlib--maxIndent nil)
(defvar parinferlib--indentDelta nil)
(defvar parinferlib--error nil)
(defvar parinferlib--errorPosCache nil
  "A plist of potential error positions.")
(defvar parinferlib--cursorX nil)
(defvar parinferlib--origCursorX nil)
(defvar parinferlib--cursorLine nil)
(defvar parinferlib--cursorDx nil)
(defvar parinferlib--previewCursorScope nil)

(defun parinferlib--initialize (text mode options)
  "Reset state according to TEXT, MODE, and OPTIONS."
  (let ((lines-vector (vconcat (split-string text parinferlib--LINE_ENDING_REGEX))))
    (setq parinferlib--mode mode
          parinferlib--origText text
          parinferlib--origLines lines-vector
          parinferlib--origCursorX nil
          parinferlib--lines (make-vector (length lines-vector) nil)
          parinferlib--lineNo -1
          parinferlib--ch ""
          parinferlib--x 0
          parinferlib--parenStack nil
          parinferlib--tabStops nil
          parinferlib--parenTrailLineNo nil
          parinferlib--parenTrailStartX nil
          parinferlib--parenTrailEndX nil
          parinferlib--parenTrailOpeners nil
          parinferlib--cursorX nil
          parinferlib--cursorLine nil
          parinferlib--cursorDx nil
          parinferlib--previewCursorScope nil
          parinferlib--canPreviewCursorScope nil
          parinferlib--isInCode t
          parinferlib--isEscaping nil
          parinferlib--isInStr nil
          parinferlib--isInComment nil
          parinferlib--commentX nil
          parinferlib--quoteDanger nil
          parinferlib--trackingIndent nil
          parinferlib--skipChar nil
          parinferlib--success nil
          parinferlib--maxIndent nil
          parinferlib--indentDelta 0
          parinferlib--error nil
          parinferlib--errorPosCache nil)

    ;; merge options if they are valid
    (when (integerp (plist-get options :cursor-x))
      (setq parinferlib--cursorX (plist-get options :cursor-x))
      (setq parinferlib--origCursorX (plist-get options :cursor-x)))
    (when (integerp (plist-get options :cursor-line))
      (setq parinferlib--cursorLine (plist-get options :cursor-line)))
    (when (integerp (plist-get options :cursor-dx))
      (setq parinferlib--cursorDx (plist-get options :cursor-dx)))
    (when (booleanp (plist-get options :preview-cursor-scope))
      (setq parinferlib--previewCursorScope (plist-get options :preview-cursor-scope)))))

;;------------------------------------------------------------------------------
;; Errors
;;------------------------------------------------------------------------------

(defconst parinferlib--err-messages
  '((quote-danger . "Quotes must balanced inside comment blocks.")
    (eol-backslash . "Line cannot end in a hanging backslash.")
    (unclosed-quote . "String is missing a closing quote.")
    (unclosed-paren . "Unmatched open-paren."))
  "Alist mapping error symbols to error messages.")

(defun parinferlib--cache-error-pos (error-name line-no x)
  (let* ((error-cache parinferlib--errorPosCache)
         (position (list :line-no line-no :x x))
         (updated-error-cache (plist-put error-cache error-name position)))
    (setq parinferlib--errorPosCache updated-error-cache)))

(defun parinferlib--create-error (error line-no x)
  "Create an error object.

ERROR: one of the keys in `parinferlib--err-messages'.
LINE-NO: current line number.
X: current position."
  (let* ((error-cache parinferlib--errorPosCache)
         (error-msg (cdr (assq error parinferlib--err-messages)))
         (error-position (plist-get error-cache error)))
    (when (not line-no)
      (setq line-no (plist-get error-position :line-no)))
    (when (not x)
      (setq x (plist-get error-position :x)))

    ;; return a plist of the error
    (list :name (symbol-name error)
          :message error-msg
          :line-no line-no
          :x x)))

;;------------------------------------------------------------------------------
;; String Operations
;;------------------------------------------------------------------------------

(defun parinferlib--replace-within-string (orig start end replace)
  (let* ((orig-length (length orig))
         (head (substring orig 0 start))
         (tail (if (>= end orig-length)
                 ""
                 (substring orig end))))
    (concat head replace tail)))

;;------------------------------------------------------------------------------
;; Line Operations
;;------------------------------------------------------------------------------

(defun parinferlib--cursor-affected? (start end)
  (let ((cursor-x parinferlib--cursorX))
    (if (and (equal cursor-x start)
             (equal cursor-x end))
        (zerop cursor-x)
      (>= cursor-x end))))

(defun parinferlib--shift-cursor-on-edit (line-no start end replace)
  (let* ((old-length (- end start))
         (new-length (length replace))
         (dx (- new-length old-length))
         (cursor-line parinferlib--cursorLine)
         (cursor-x parinferlib--cursorX))
    (when (and dx
               (not (zerop dx))
               (equal cursor-line line-no)
               cursor-x
               (parinferlib--cursor-affected? start end))
      (setq parinferlib--cursorX (+ cursor-x dx)))))

(defun parinferlib--replace-within-line (line-no start end replace)
  (let* ((lines parinferlib--lines)
         (line (aref lines line-no))
         (new-line (parinferlib--replace-within-string line start end replace)))
    (aset lines line-no new-line)
    (parinferlib--shift-cursor-on-edit line-no start end replace)))

(defun parinferlib--insert-within-line (line-no idx insert)
  (parinferlib--replace-within-line line-no idx idx insert))

(defun parinferlib--init-line (line)
  (let* ((current-line-no parinferlib--lineNo)
         (new-line-no (1+ current-line-no))
         (lines parinferlib--lines))
    (aset lines new-line-no line)

    (setq parinferlib--x 0)
    (setq parinferlib--lineNo new-line-no)

    ;; reset line-specific state
    (setq parinferlib--commentX nil)
    (setq parinferlib--indentDelta 0)))

(defun parinferlib--commit-char (orig-ch)
  "if the current character has changed, commit its change to the current line"
  (let* ((line-no parinferlib--lineNo)
         (x parinferlib--x)
         (ch parinferlib--ch)
         (ch-length (length ch))
         (orig-ch-length (length orig-ch)))
    (when (not (string= orig-ch ch))
      (parinferlib--replace-within-line line-no x (+ x orig-ch-length) ch))
    (setq parinferlib--x (+ x ch-length))))

;;------------------------------------------------------------------------------
;; Util
;;------------------------------------------------------------------------------

(defun parinferlib--clamp (val-n min-n max-n)
  (when min-n
    (setq val-n (if (> min-n val-n) min-n val-n)))
  (when max-n
    (setq val-n (if (< max-n val-n) max-n val-n)))
  val-n)

;;------------------------------------------------------------------------------
;; Character functions
;;------------------------------------------------------------------------------

(defun parinferlib--valid-close-paren? (paren-stack ch)
  (when paren-stack
    (let* ((top-of-stack (car paren-stack))
           (top-of-stack-ch (parinferlib--stack-elem-ch top-of-stack)))
      (string= top-of-stack-ch (gethash ch parinferlib--PARENS)))))

(defun parinferlib--on-open-paren ()
  (when parinferlib--isInCode
    (let* ((new-stack-el (parinferlib--stack-elem
                          parinferlib--ch
                          parinferlib--indentDelta
                          parinferlib--lineNo
                          parinferlib--x))
           (paren-stack parinferlib--parenStack)
           (new-paren-stack (cons new-stack-el paren-stack)))
      (setq parinferlib--parenStack new-paren-stack))))

(defun parinferlib--on-matched-close-paren ()
  (let* ((paren-stack parinferlib--parenStack)
         (opener (pop paren-stack))
         (opener-x (parinferlib--stack-elem-x opener))
         (result-x parinferlib--x)
         (openers parinferlib--parenTrailOpeners)
         (new-openers (cons opener openers)))
    (setq parinferlib--parenTrailEndX (1+ result-x))
    (setq parinferlib--parenTrailOpeners new-openers)
    (setq parinferlib--maxIndent opener-x)
    ;; the first element of paren-stack was removed when we called "pop" earlier
    (setq parinferlib--parenStack paren-stack)))

(defun parinferlib--on-unmatched-close-paren ()
  (setq parinferlib--ch ""))

(defun parinferlib--on-close-paren ()
  (when parinferlib--isInCode
    (if (parinferlib--valid-close-paren? parinferlib--parenStack parinferlib--ch)
        (parinferlib--on-matched-close-paren)
      (parinferlib--on-unmatched-close-paren))))

(defun parinferlib--on-tab ()
  (when parinferlib--isInCode
    (setq parinferlib--ch parinferlib--DOUBLE_SPACE)))

(defun parinferlib--on-semicolon ()
  (when parinferlib--isInCode
    (setq parinferlib--isInComment t)
    (setq parinferlib--commentX parinferlib--x)))

(defun parinferlib--on-newline ()
  (setq parinferlib--isInComment nil)
  (setq parinferlib--ch ""))

(defun parinferlib--on-quote ()
  (cond (parinferlib--isInStr
         (setq parinferlib--isInStr nil))

        (parinferlib--isInComment
         (let ((quote-danger parinferlib--quoteDanger))
           (setq parinferlib--quoteDanger (not quote-danger))
           (when parinferlib--quoteDanger
             (let ((line-no parinferlib--lineNo)
                   (x parinferlib--x))
               (parinferlib--cache-error-pos 'quote-danger line-no x)))))

        (t
         (let ((line-no parinferlib--lineNo)
               (x parinferlib--x))
           (setq parinferlib--isInStr t)
           (parinferlib--cache-error-pos 'unclosed-quote line-no x)))))

(defun parinferlib--on-backslash ()
  (setq parinferlib--isEscaping t))

(defun parinferlib--after-backslash ()
  (setq parinferlib--isEscaping nil)
  (when (string= parinferlib--ch parinferlib--NEWLINE)
    (when parinferlib--isInCode
      (let ((line-no parinferlib--lineNo)
            (x parinferlib--x))
        (throw 'parinferlib-error (parinferlib--create-error 'eol-backslash line-no (1- x)))))
    (parinferlib--on-newline)))

(defun parinferlib--on-char (ch)
  "Do stuff depending on CH and current state.
CH is the character we're processing."
  (cond (parinferlib--isEscaping                   (parinferlib--after-backslash))
        ((parinferlib--open-paren? ch)             (parinferlib--on-open-paren))
        ((parinferlib--close-paren? ch)            (parinferlib--on-close-paren))
        ((string= ch parinferlib--DOUBLE_QUOTE)    (parinferlib--on-quote))
        ((string= ch parinferlib--SEMICOLON)       (parinferlib--on-semicolon))
        ((string= ch parinferlib--BACKSLASH)       (parinferlib--on-backslash))
        ((string= ch parinferlib--TAB)             (parinferlib--on-tab))
        ((string= ch parinferlib--NEWLINE)         (parinferlib--on-newline)))
  (let ((in-comment? parinferlib--isInComment)
        (in-string? parinferlib--isInStr))
    (setq parinferlib--isInCode (and (not in-comment?) (not in-string?)))))

;;------------------------------------------------------------------------------
;; Cursor functions
;;------------------------------------------------------------------------------

(defun parinferlib--cursor-on-left? ()
  (let* ((line-no parinferlib--lineNo)
         (cursor-line parinferlib--cursorLine)
         (cursor-x parinferlib--cursorX)
         (result-x parinferlib--x))
    (and (equal line-no cursor-line)
         cursor-x
         result-x
         (<= cursor-x result-x))))

(defun parinferlib--cursor-on-right? (x)
  (let* ((line-no parinferlib--lineNo)
         (cursor-line parinferlib--cursorLine)
         (cursor-x parinferlib--cursorX))
    (and (equal line-no cursor-line)
         cursor-x
         x
         (> cursor-x x))))

(defun parinferlib--cursor-in-comment? ()
  (parinferlib--cursor-on-right? parinferlib--commentX))

(defun parinferlib--handle-cursor-delta ()
  (let* ((cursor-dx parinferlib--cursorDx)
         (cursor-line parinferlib--cursorLine)
         (line-no parinferlib--lineNo)
         (cursor-x parinferlib--cursorX)
         (x parinferlib--x)
         (indent-delta parinferlib--indentDelta)
         (has-delta? (and cursor-dx
                          (equal cursor-line line-no)
                          (equal cursor-x x))))
    (when has-delta?
      (setq parinferlib--indentDelta (+ indent-delta cursor-dx)))))

;;------------------------------------------------------------------------------
;; Paren Trail functions
;;------------------------------------------------------------------------------

(defun parinferlib--reset-paren-trail (line-no x)
  (setq parinferlib--parenTrailLineNo line-no
        parinferlib--parenTrailStartX x
        parinferlib--parenTrailEndX x
        parinferlib--parenTrailOpeners nil
        parinferlib--maxIndent nil))

(defun parinferlib--update-paren-trail-bounds ()
  (let* ((lines parinferlib--lines)
         (line-no parinferlib--lineNo)
         (line (aref lines line-no))
         (x parinferlib--x)
         (prev-ch (if (> x 0)
                    (string (aref line (1- x)))
                    nil))
         (ch parinferlib--ch)
         (should-reset? (and parinferlib--isInCode
                             (or (not (parinferlib--close-paren? ch))
                                 (string= prev-ch parinferlib--BACKSLASH))
                             (not (string= ch ""))
                             (or (not (string= ch parinferlib--BLANK_SPACE))
                                 (string= prev-ch parinferlib--BACKSLASH))
                             (not (string= ch parinferlib--DOUBLE_SPACE)))))
    (when should-reset?
      (parinferlib--reset-paren-trail line-no (1+ x)))))

(defun parinferlib--clamp-paren-trail-to-cursor ()
  (let* ((start-x parinferlib--parenTrailStartX)
         (end-x parinferlib--parenTrailEndX)
         (cursor-clamping? (and (parinferlib--cursor-on-right? start-x)
                                (not (parinferlib--cursor-in-comment?)))))
    (when cursor-clamping?
      (let* ((cursor-x parinferlib--cursorX)
             (new-start-x (max start-x cursor-x))
             (new-end-x (max end-x cursor-x))
             (line-no parinferlib--lineNo)
             (lines parinferlib--lines)
             (line (aref lines line-no))
             (remove-count 0)
             (i start-x))
        (while (< i new-start-x)
          (when (parinferlib--close-paren? (string (aref line i)))
            (setq remove-count (1+ remove-count)))
          (setq i (1+ i)))
        (when (> remove-count 0)
          (let* ((openers parinferlib--parenTrailOpeners)
                 (new-openers (nbutlast openers remove-count)))
            (setq parinferlib--parenTrailOpeners new-openers)))
        (setq parinferlib--parenTrailStartX new-start-x
              parinferlib--parenTrailEndX new-end-x)))))

(defun parinferlib--pop-paren-trail ()
  (let ((start-x parinferlib--parenTrailStartX)
        (end-x parinferlib--parenTrailEndX))
    (when (not (equal start-x end-x))
      (let ((openers parinferlib--parenTrailOpeners)
            (paren-stack parinferlib--parenStack))
        (while openers
          (setq paren-stack (cons (pop openers) paren-stack)))
        (setq parinferlib--parenTrailOpeners openers
              parinferlib--parenStack paren-stack)))))

(defun parinferlib--correct-paren-trail (indent-x)
  (let ((parens "")
        (paren-stack parinferlib--parenStack)
        (break? nil))
    (while (and (> (length paren-stack) 0)
                (not break?))
      (let* ((opener (car paren-stack))
             (opener-x (parinferlib--stack-elem-x opener))
             (opener-ch (parinferlib--stack-elem-ch opener)))
        (if (>= opener-x indent-x)
            (progn (pop paren-stack)
                   (setq parens (concat parens (gethash opener-ch parinferlib--PARENS))))
          (setq break? t))))
    (setq parinferlib--parenStack paren-stack)
    (let ((paren-trail-line-no parinferlib--parenTrailLineNo)
          (paren-trail-start-x parinferlib--parenTrailStartX)
          (paren-trail-end-x parinferlib--parenTrailEndX))
      (parinferlib--replace-within-line paren-trail-line-no paren-trail-start-x paren-trail-end-x parens))))

(defun parinferlib--clean-paren-trail ()
  (let* ((start-x parinferlib--parenTrailStartX)
         (end-x parinferlib--parenTrailEndX)
         (line-no parinferlib--lineNo)
         (paren-trail-line-no parinferlib--parenTrailLineNo)
         (exit-early? (or (equal start-x end-x)
                          (not (equal line-no paren-trail-line-no)))))
    (when (not exit-early?)
      (let* ((lines parinferlib--lines)
             (line (aref lines line-no))
             (new-trail "")
             (space-count 0)
             (i start-x))
        (while (< i end-x)
          (let ((ch (string (aref line i))))
            (if (parinferlib--close-paren? ch)
              (setq new-trail (concat new-trail ch))
              (setq space-count (1+ space-count))))
          (setq i (1+ i)))
        (when (> space-count 0)
          (parinferlib--replace-within-line line-no start-x end-x new-trail)
          (let* ((paren-trail-end-x parinferlib--parenTrailEndX)
                 (new-pt-end-x (- paren-trail-end-x space-count)))
            (setq parinferlib--parenTrailEndX new-pt-end-x)))))))

(defun parinferlib--append-paren-trail ()
  (let* ((paren-stack parinferlib--parenStack)
         (opener (pop paren-stack))
         (opener-ch (parinferlib--stack-elem-ch opener))
         (opener-x (parinferlib--stack-elem-x opener))
         (close-ch (gethash opener-ch parinferlib--PARENS))
         (paren-trail-line-no parinferlib--parenTrailLineNo)
         (end-x parinferlib--parenTrailEndX))
    (setq parinferlib--parenStack paren-stack)
    (setq parinferlib--maxIndent opener-x)
    (parinferlib--insert-within-line paren-trail-line-no end-x close-ch)
    (setq parinferlib--parenTrailEndX (1+ end-x))))

(defun parinferlib--invalidate-paren-trail ()
  (setq parinferlib--parenTrailLineNo nil)
  (setq parinferlib--parenTrailStartX nil)
  (setq parinferlib--parenTrailEndX nil)
  (setq parinferlib--parenTrailOpeners '()))

(defun parinferlib--finish-new-paren-trail ()
  (let* ((in-str? parinferlib--isInStr)
         (mode parinferlib--mode)
         (line-no parinferlib--lineNo)
         (cursor-line parinferlib--cursorLine))
    (cond
     (in-str?
      (parinferlib--invalidate-paren-trail))

     ((equal mode :indent)
      (progn
        (parinferlib--clamp-paren-trail-to-cursor)
        (parinferlib--pop-paren-trail)))


     ((and (equal mode :paren)
           (not (equal line-no cursor-line)))
      (parinferlib--clean-paren-trail)))))

;;------------------------------------------------------------------------------
;; Indentation functions
;;------------------------------------------------------------------------------

(defun parinferlib--correct-indent ()
  (let* ((orig-indent parinferlib--x)
         (new-indent orig-indent)
         (min-indent 0)
         (max-indent parinferlib--maxIndent)
         (paren-stack parinferlib--parenStack)
         (opener (car paren-stack)))
    (when opener
      (let* ((opener-x (parinferlib--stack-elem-x opener))
             (opener-indent-delta (parinferlib--stack-elem-indent-delta opener)))
        (setq min-indent (1+ opener-x))
        (setq new-indent (+ new-indent opener-indent-delta))))
    (setq new-indent (parinferlib--clamp new-indent min-indent max-indent))
    (when (not (equal new-indent orig-indent))
      (let* ((indent-str (make-string new-indent (aref parinferlib--BLANK_SPACE 0)))
             (line-no parinferlib--lineNo)
             (indent-delta parinferlib--indentDelta)
             (new-indent-delta (+ indent-delta (- new-indent orig-indent))))
        (parinferlib--replace-within-line line-no 0 orig-indent indent-str)
        (setq parinferlib--x new-indent)
        (setq parinferlib--indentDelta new-indent-delta)))))

(defun parinferlib--try-preview-cursor-scope ()
  (when parinferlib--canPreviewCursorScope
    (let ((cursor-x parinferlib--cursorX)
          (cursor-line parinferlib--cursorLine)
          (result-x parinferlib--x))
      (when (> cursor-x result-x)
        (parinferlib--correct-paren-trail cursor-x)
        (parinferlib--reset-paren-trail cursor-line cursor-x))
      (setq parinferlib--canPreviewCursorScope nil))))

(defun parinferlib--on-indent ()
  (setq parinferlib--trackingIndent nil)
  (when parinferlib--quoteDanger
    (throw 'parinferlib-error (parinferlib--create-error 'quote-danger nil nil)))
  (let ((mode parinferlib--mode)
        (x parinferlib--x))
    (when (equal mode :indent)
      (parinferlib--try-preview-cursor-scope)
      (parinferlib--correct-paren-trail x))
    (when (equal mode :paren)
      (parinferlib--correct-indent))))

(defun parinferlib--on-leading-close-paren ()
  (setq parinferlib--skipChar t)
  (setq parinferlib--trackingIndent t)
  (when (equal :paren parinferlib--mode)
    (let* ((paren-stack parinferlib--parenStack)
           (ch parinferlib--ch))
      (when (parinferlib--valid-close-paren? paren-stack ch)
        (if (parinferlib--cursor-on-left?)
            (progn (setq parinferlib--skipChar nil)
                   (parinferlib--on-indent))
          (parinferlib--append-paren-trail))))))

(defun parinferlib--check-indent ()
  (let ((ch parinferlib--ch))
    (cond ((parinferlib--close-paren? ch)
           (parinferlib--on-leading-close-paren))

          ((string= ch parinferlib--SEMICOLON)
           (setq parinferlib--trackingIndent nil))

          ((not (or (string= ch parinferlib--NEWLINE)
                    (string= ch parinferlib--BLANK_SPACE)
                    (string= ch parinferlib--TAB)))
           (parinferlib--on-indent)))))

(defun parinferlib--init-preview-cursor-scope ()
  (let* ((preview-cursor-scope parinferlib--previewCursorScope)
         (cursor-line parinferlib--cursorLine)
         (line-no parinferlib--lineNo)
         (lines parinferlib--lines)
         (line (aref lines line-no))
         (semicolon-x (string-match ";" line))
         (cursor-x parinferlib--cursorX))
    (when (and preview-cursor-scope
               (equal cursor-line line-no))
      (setq parinferlib--canPreviewCursorScope
            (and parinferlib--trackingIndent
                 (string-match parinferlib--STANDALONE_PAREN_TRAIL line)
                 (or (not semicolon-x)
                     (<= cursor-x semicolon-x)))))))

(defun parinferlib--init-indent ()
  (let ((mode parinferlib--mode)
        (in-str? parinferlib--isInStr))
    (when (equal :indent mode)
      ;; length of list > 0 means the same as the list not being null
      (setq parinferlib--trackingIndent (and parinferlib--parenStack
                                             (not in-str?)))
      (parinferlib--init-preview-cursor-scope))
    (when (equal :paren mode)
      (setq parinferlib--trackingIndent (not in-str?)))))

(defun parinferlib--set-tab-stops ()
  (let ((cursor-line parinferlib--cursorLine)
        (line-no parinferlib--lineNo)
        (mode parinferlib--mode))
    (when (and (equal cursor-line line-no)
               (equal :indent mode))
      (let ((current-stops parinferlib--tabStops)
            (new-stops '()))
        (dolist (stackel parinferlib--parenStack)
          (let ((new-stop (list :ch (parinferlib--stack-elem-ch stackel)
                                :line-no (parinferlib--stack-elem-line-no stackel)
                                :x (parinferlib--stack-elem-x stackel))))
            (setq new-stops (push new-stop new-stops))))
        (setq parinferlib--tabStops (append current-stops new-stops))))))

;;------------------------------------------------------------------------------
;; High-level processing functions
;;------------------------------------------------------------------------------

(defun parinferlib--process-char (ch)
  (let* ((orig-ch ch)
         (mode parinferlib--mode))
    (setq parinferlib--ch ch)
    (setq parinferlib--skipChar nil)

    (when (equal :paren mode)
      (parinferlib--handle-cursor-delta))

    (when parinferlib--trackingIndent
      (parinferlib--check-indent))

    (if parinferlib--skipChar
        (setq parinferlib--ch "")
      (parinferlib--on-char ch)
      (parinferlib--update-paren-trail-bounds))

    (parinferlib--commit-char orig-ch)))

(defun parinferlib--process-line (line)
  (parinferlib--init-line line)
  (parinferlib--init-indent)
  (parinferlib--set-tab-stops)
  (let* ((i 0)
         (chars (concat line parinferlib--NEWLINE))
         (chars-length (length chars)))
    (while (< i chars-length)
      (parinferlib--process-char (string (aref chars i)))
      (setq i (1+ i))))

  (when (equal parinferlib--lineNo
               parinferlib--parenTrailLineNo)
    (parinferlib--finish-new-paren-trail)))

(defun parinferlib--finalize-result ()
  (when parinferlib--quoteDanger
    (throw 'parinferlib-error
           (parinferlib--create-error 'quote-danger nil nil)))
  (when parinferlib--isInStr
    (throw 'parinferlib-error
           (parinferlib--create-error 'unclosed-quote nil nil)))
  (let* ((paren-stack parinferlib--parenStack)
         (mode parinferlib--mode))
    (when paren-stack
      (when (equal mode :paren)
        (let* ((paren-stack parinferlib--parenStack)
               (opener (car paren-stack))
               (opener-line-no (parinferlib--stack-elem-line-no opener))
               (opener-x (parinferlib--stack-elem-x opener)))
          (throw 'parinferlib-error
                 (parinferlib--create-error 'unclosed-paren opener-line-no opener-x))))
      (when (equal mode :indent)
        (setq parinferlib--x 0)
        (parinferlib--on-indent))))
  (setq parinferlib--success t))

(defun parinferlib--process-error (err)
  (setq parinferlib--success nil)
  (setq parinferlib--error err))

(defun parinferlib--process-text (text mode options)
  "Process TEXT in MODE with OPTIONS."
  (parinferlib--initialize text mode options)
  (let* ((orig-lines parinferlib--origLines)
         (lines-length (length orig-lines))
         (i 0)
         (err (catch 'parinferlib-error
                (while (< i lines-length)
                  (parinferlib--process-line (aref orig-lines i))
                  (setq i (1+ i)))
                (parinferlib--finalize-result)
                nil)))
    (when err
      (parinferlib--process-error err))))

(defun parinferlib--get-changed-lines ()
  (let* ((orig-lines parinferlib--origLines)
         (lines parinferlib--lines)
         (lines-length (min (length orig-lines) (length lines)))
         (changed-lines nil))
    (dotimes (i lines-length)
      (let ((line (aref lines i))
            (orig-line (aref orig-lines i)))
        (unless (string= line orig-line)
          (push (list :line-no i :line line) changed-lines))))
    changed-lines))

(defun parinferlib--public-result ()
  "Return a plist for the Public API."
  (if parinferlib--success
      (let* ((lines parinferlib--lines)
             (result-text (mapconcat 'identity lines parinferlib--NEWLINE))
             (cursor-x parinferlib--cursorX)
             (tab-stops parinferlib--tabStops))
        (list :success t
              :cursor-x cursor-x
              :text result-text
              :changed-lines (parinferlib--get-changed-lines)
              :tab-stops tab-stops))
    (let ((orig-text parinferlib--origText)
          (public-error parinferlib--error)
          (orig-cursor-x parinferlib--origCursorX))
      (list :success nil
            :text orig-text
            :cursor-x orig-cursor-x
            :error public-error))))

;;------------------------------------------------------------------------------
;; Public API
;;------------------------------------------------------------------------------

(defun parinferlib-indent-mode (text &optional options)
  "Indent Mode public function.

TEXT should be a string to process with Parinfer.
OPTIONS should be a plist; see README.md for all options."
  (parinferlib--process-text text :indent options)
  (parinferlib--public-result))

(defun parinferlib-paren-mode (text &optional options)
  "Paren Mode public function.

TEXT should be a string to process with Parinfer.
OPTIONS should be a plist; see README.md for all options."
  (parinferlib--process-text text :paren options)
  (parinferlib--public-result))

(provide 'parinferlib)

;;; parinferlib.el ends here
