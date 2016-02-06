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

;; A Stack Element is a Vector of 4 items (alphabetically ordered)
;; idx : item
;;   0 : ch
;;   1 : indentDelta
;;   2 : lineNo
;;   3 : x
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
  (parinferlib--zero? (length stack)))

(defun parinferlib--not-empty? (stack)
  (not (parinferlib--empty? stack)))

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

    (puthash :errorPosCache (make-hash-table) result)

    result))

;;------------------------------------------------------------------------------
;; Errors
;;------------------------------------------------------------------------------

;; TODO: namespace these?
(defconst ERROR_QUOTE_DANGER "quote-danger")
(defconst ERROR_EOL_BACKSLASH "eol-backslash")
(defconst ERROR_UNCLOSED_QUOTE "unclosed-quote")
(defconst ERROR_UNCLOSED_PAREN "unclosed-paren")
(defconst ERROR_UNHANDLED "unhandled")

(defconst error-messages (make-hash-table))
(puthash ERROR_QUOTE_DANGER   "Quotes must balanced inside comment blocks." error-messages)
(puthash ERROR_EOL_BACKSLASH  "Line cannot end in a hanging backslash." error-messages)
(puthash ERROR_UNCLOSED_QUOTE "String is missing a closing quote." error-messages)
(puthash ERROR_UNCLOSED_PAREN "Unmatched open-paren." error-messages)

(defun parinferlib--cache-error-pos (result error-name line-no x)
  (let ((cache (gethash :errorPosCache result)))
    (puthash error-name [line-no x] cache)
    (puthash :errorPosCache cache result)))

;; TODO: error function here

;;------------------------------------------------------------------------------
;; String Operations
;;------------------------------------------------------------------------------

(defun parinferlib--insert-within-string (orig idx insert)
  (concat (substring orig 0 idx)
          insert
          (substring orig idx)))

(defun parinferlib--replace-within-string (orig start end replace)
  (let* ((orig-length (length orig))
         (head (substring orig 0 start))
         (tail (if (>= end orig-length)
                 ""
                 (substring orig end))))
    (concat head replace tail)))

(defun parinferlib--remove-within-string (orig start end)
  (concat (substring orig 0 start)
          (substring orig end)))

;;------------------------------------------------------------------------------
;; Line Operations
;;------------------------------------------------------------------------------

(defun parinferlib--insert-within-line (result line-no idx insert)

  ; (print "insert-within-line")

  (let* ((lines (gethash :lines result))
         (line (aref lines line-no))
         (new-line (parinferlib--insert-within-string line idx insert)))
    (aset lines line-no new-line)
    (puthash :lines lines result)
    nil))

(defun parinferlib--replace-within-line (result line-no start end replace)


  ; (print "replace-within-line")

  (let* ((lines (gethash :lines result))
         (line (aref lines line-no))
         (new-line (parinferlib--replace-within-string line start end replace)))
    (aset lines line-no new-line)
    (puthash :lines lines result)
    nil))

(defun parinferlib--remove-within-line (result line-no start end)

  ; (print "remove-within-line")

  (let* ((lines (gethash :lines result))
         (line (aref lines line-no))
         (new-line (parinferlib--remove-within-string line start end)))
    (aset lines line-no new-line)
    (puthash :lines lines result)
    nil))

(defun parinferlib--init-line (result line)
  (let* ((current-line-no (gethash :lineNo result))
         (new-line-no (1+ current-line-no))
         (lines (gethash :lines result)))
    (aset lines new-line-no line)

    (puthash :x 0 result)
    (puthash :lineNo new-line-no result)
    (puthash :lines lines result)

    ;; reset line-specific state
    (puthash :commentX nil result)
    (puthash :indentDelta 0 result)
    nil))

;; if the current character has changed, commit it's change to the current line
(defun parinferlib--commit-char (result orig-ch)

  ; (print orig-ch)
  ; (print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

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

(defun parinferlib--clamp (val-n min-n max-n)
  (when min-n
    (setq val-n (if (> min-n val-n) min-n val-n)))
  (when max-n
    (setq val-n (if (< max-n val-n) max-n val-n)))
  val-n)

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
    (puthash :parenTrailEndX (1+ result-x) result)
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
  (cond ((gethash :isInStr result)
         (puthash :isInStr t result))

        ((gethash :isInComment result)
         (let ((quote-danger (gethash :quoteDanger result)))
           (puthash :quoteDanger (not quote-danger) result)
           (when (gethash :quoteDanger result)
             (let ((line-no (gethash :lineNo result))
                   (x (gethash :x result))))
             (parinferlib--cache-error-pos result ERROR_QUOTE_DANGER line-no x))))

        (t
         (let ((line-no (gethash :lineNo result))
               (x (gethash :x result)))
          (puthash :isInStr t result)
          (parinferlib--cache-error-pos result ERROR_UNCLOSED_QUOTE line-no x))))
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
    (puthash :isInCode (and (not in-comment?) (not in-string?)) result))
  nil)

;;------------------------------------------------------------------------------
;; Cursor functions
;;------------------------------------------------------------------------------

(defun parinferlib--cursor-on-left? (result)
  (let* ((line-no (gethash :lineNo result))
         (cursor-line (gethash :cursorLine result))
         (cursor-x (gethash :cursorX result))
         (result-x (gethash :x result)))
    (and (equal line-no cursor-line)
         cursor-x
         (<= cursor-x result-x))))

(defun parinferlib--cursor-on-right? (result x)
  (let* ((line-no (gethash :lineNo result))
         (cursor-line (gethash :cursorLine result))
         (cursor-x (gethash :cursorX result)))
    (and (equal line-no cursor-line)
         cursor-x
         x
         (> cursor-x x))))

(defun parinferlib--cursor-in-comment? (result)
  (parinferlib--cursor-on-right? result (gethash :commentX result)))

(defun parinferlib--handle-cursor-delta (result)
  (let* ((cursor-dx (gethash :cursorDx result))
         (cursor-line (gethash :cursorLine result))
         (line-no (gethash :lineNo result))
         (cursor-x (gethash :cursorX result))
         (x (gethash :x result))
         (indent-delta (gethash :indentDelta result))
         (has-delta? (and cursor-dx
                          (equal cursor-line line-no)
                          (equal cursor-x x))))
    (when has-delta?
      (puthash :indentDelta (+ indent-delta cursor-dx) result)))
  nil)

;;------------------------------------------------------------------------------
;; Paren Trail functions
;;------------------------------------------------------------------------------

(defun parinferlib--update-paren-trail-bounds (result)
  (let* ((lines (gethash :lines result))
         (line-no (gethash :lineNo result))
         (line (aref lines line-no))
         (x (gethash :x result))
         (prev-ch (if (> x 0)
                    (string (aref line (1- x)))
                    nil))
         (ch (gethash :ch result))
         (should-reset? (and (gethash :isInCode result)
                             (not (parinferlib--close-paren? ch))
                             (not (string= ch ""))
                             (or (not (string= ch BLANK_SPACE))
                                 (string= prev-ch BACKSLASH))
                             (not (string= ch DOUBLE_SPACE)))))
    (when should-reset?
      (puthash :parenTrailLineNo line-no result)
      (puthash :parenTrailStartX (1+ x) result)
      (puthash :parenTrailEndX (1+ x) result)
      (puthash :parenTrailOpeners '() result)
      (puthash :maxIndent nil result)))
  nil)

(defun parinferlib--clamp-paren-trail-to-cursor (result)
  (let* ((start-x (gethash :parenTrailStartX result))
         (end-x (gethash :parenTrailEndX result))
         (cursor-clamping? (and (parinferlib--cursor-on-right? result start-x)
                                (not (parinferlib--cursor-in-comment? result)))))
    (when cursor-clamping?
      (let* ((cursor-x (gethash :cursorX result))
             (new-start-x (max start-x cursor-x))
             (new-end-x (max end-x cursor-x))
             (line-no (gethash :lineNo result))
             (lines (gethash :lines result))
             (line (aref lines line-no))
             (remove-count 0)
             (i start-x))
        (while (< i new-start-x)
          (when (parinferlib--close-paren? (string (aref line i)))
            (setq remove-count (1+ remove-count)))
          (setq i (1+ i)))
        () ;; TODO: figure out splice
        (puthash :parenTrailStartX new-start-x result)
        (puthash :parenTrailEndX new-end-x result))))
  nil)

(defun parinferlib--remove-paren-trail (result)
  (let ((start-x (gethash :parenTrailStartX result))
        (end-x (gethash :parenTrailEndX result)))
    (when (not (equal start-x end-x))
      (let ((openers (gethash :parenTrailOpeners result))
            (paren-stack (gethash :parenStack result)))

        ;; (print (gethash :parenTrailOpeners result))
        ;; (print (gethash :parenStack result))
        ;; (print "------------------")

        (while (parinferlib--not-empty? openers)
          (setq paren-stack (cons (pop openers) paren-stack)))
        (puthash :parenTrailOpeners openers result)
        (puthash :parenStack paren-stack result)

        ;; (print (gethash :parenTrailOpeners result))
        ;; (print (gethash :parenStack result))
        ;; (print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

        (parinferlib--remove-within-line result (gethash :lineNo result) start-x end-x))))
  nil)

(defun parinferlib--correct-paren-trail (result indent-x)
  ;; (print "correct paren trail")
  (let ((parens "")
        (paren-stack (gethash :parenStack result))
        (break? nil))
    (while (and (> (length paren-stack) 0)
                (not break?))
      (let* ((opener (car paren-stack))
             (opener-x (aref opener X_IDX))
             (opener-ch (aref opener CH_IDX)))
        (if (>= opener-x indent-x)
          (progn (pop paren-stack)
                 (setq parens (concat parens (gethash opener-ch PARENS))))
          (setq break? t))))
    (puthash :parenStack paren-stack result)
    (let ((paren-trail-line-no (gethash :parenTrailLineNo result))
          (paren-trail-start-x (gethash :parenTrailStartX result)))
      (parinferlib--insert-within-line result paren-trail-line-no paren-trail-start-x parens)))
  nil)

(defun parinferlib--clean-paren-trail (result)
  (let* ((start-x (gethash :parenTrailStartX result))
         (end-x (gethash :parenTrailEndX result))
         (line-no (gethash :lineNo result))
         (paren-trail-line-no (gethash :parenTrailLineNo result))
         (exit-early? (or (equal start-x end-x)
                          (not (equal line-no paren-trail-line-no)))))
    (when (not exit-early?)
      (let* ((lines (gethash :lines result))
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
          (parinferlib--replace-within-line result line-no start-x end-x new-trail)
          (let* ((paren-trail-end-x (gethash :parenTrailEndX result))
                 (new-pt-end-x (- paren-trail-end-x space-count)))
            (puthash :parenTrailEndX new-pt-end-x result))))))
  nil)

(defun parinferlib--append-paren-trail (result)
  ;; (print "append paren trail")
  (let* ((paren-stack (gethash :parenStack result))
         (opener (pop paren-stack))
         (opener-ch (aref opener CH_IDX))
         (opener-x (aref opener X_IDX))
         (close-ch (gethash opener-ch PARENS))
         (paren-trail-line-no (gethash :parenTrailLineNo result))
         (end-x (gethash :parenTrailEndX result)))
    (puthash :parenStack paren-stack result)
    (puthash :maxIndent opener-x result)
    (parinferlib--insert-within-line result paren-trail-line-no end-x close-ch)
    (puthash :parenTrailEndX (1+ end-x) result))
  nil)

(defun parinferlib--finish-new-paren-trail (result)
  (let* ((mode (gethash :mode result))
         (line-no (gethash :lineNo result))
         (cursor-line (gethash :cursorLine result)))
    (when (equal mode :indent)
      (parinferlib--clamp-paren-trail-to-cursor result)
      (parinferlib--remove-paren-trail result))
    (when (and (equal mode :paren)
               (not (equal line-no cursor-line)))
      (parinferlib--clean-paren-trail result)))
  nil)

;;------------------------------------------------------------------------------
;; Indentation functions
;;------------------------------------------------------------------------------

(defun parinferlib--correct-indent (result)
  (let* ((orig-indent (gethash :x result))
         (new-indent orig-indent)
         (min-indent 0)
         (max-indent (gethash :maxIndent result))
         (paren-stack (gethash :parenStack result))
         (opener (car paren-stack)))
    (when opener
      (let* ((opener-x (aref opener X_IDX))
             (opener-indent-delta (aref opener INDENT_DELTA_IDX)))
        (setq min-indent (1+ opener-x))
        (setq new-indent (+ new-indent opener-indent-delta))))
    (setq new-indent (parinferlib--clamp new-indent min-indent max-indent))
    (when (not (equal new-indent orig-indent))
      (let* ((indent-str (make-string new-indent (aref BLANK_SPACE 0)))
             (line-no (gethash :lineNo result))
             (indent-delta (gethash :indentDelta result))
             (new-indent-delta (+ indent-delta (- new-indent orig-indent))))
        (parinferlib--replace-within-line result line-no 0 orig-indent indent-str)
        (puthash :x new-indent result)
        (puthash :indentDelta new-indent-delta result))))
  nil)

(defun parinferlib--on-proper-indent (result)
  ;; (print "on proper indent")
  (puthash :trackingIndent nil result)
  (when (gethash :quoteDanger result)
    ()) ;; TODO: figure out throw here
  (let ((mode (gethash :mode result))
        (x (gethash :x result)))
    (when (equal mode :indent)
      (parinferlib--correct-paren-trail result x))
    (when (equal mode :paren)
      (parinferlib--correct-indent result)))
  nil)

(defun parinferlib--on-leading-close-paren (result)
  (puthash :skipChar t result)
  (puthash :trackingIndent t result)
  (when (equal :paren (gethash :mode result))
    (let* ((paren-stack (gethash :parenStack result))
           (ch (gethash :ch result)))
      (when (parinferlib--valid-close-paren? paren-stack ch)
        (if (parinferlib--cursor-on-left? result)
          (progn (puthash :skipChar nil result)
                 (parinferlib--on-proper-indent result))
          (parinferlib--append-paren-trail result)))))
  nil)

(defun parinferlib--on-indent (result)

  ;; (print "on indent")

  (let ((ch (gethash :ch result)))
    ;;(print (concat "yy" ch "yy"))
    (cond ((parinferlib--close-paren? ch)
           (parinferlib--on-leading-close-paren result))

          ((string= ch SEMICOLON)
           (puthash :trackingIndent nil result))

          ((not (string= ch NEWLINE))
           (parinferlib--on-proper-indent result))))
  nil)

;;------------------------------------------------------------------------------
;; High-level processing functions
;;------------------------------------------------------------------------------

(defun parinferlib--process-char (result ch)
  (let* ((orig-ch ch)
         (mode (gethash :mode result)))
    (puthash :ch ch result)
    (puthash :skipChar nil result)

    (when (equal :paren mode)
      (parinferlib--handle-cursor-delta result))

    (when (and (gethash :trackingIndent result)
               (not (string= ch BLANK_SPACE))
               (not (string= ch TAB)))
      (parinferlib--on-indent result))

    (if (gethash :skipChar result)
      (puthash :ch "" result)
      (progn (parinferlib--on-char result)
             (parinferlib--update-paren-trail-bounds result)))

    (parinferlib--commit-char result orig-ch))
  nil)

(defun parinferlib--process-line (result line)
  (parinferlib--init-line result line)

  (let* ((mode (gethash :mode result)))
    (when (equal mode :indent)
      (let* ((paren-stack (gethash :parenStack result))
             (in-str? (gethash :isInStr result))
             (tracking-indent? (and (parinferlib--not-empty? paren-stack)
                                    (not in-str?))))
        (puthash :trackingIndent tracking-indent? result)))
    (when (equal mode :paren)
      (puthash :trackingIndent (not (gethash :isInStr result)) result)))

  (let* ((i 0)
         (chars (concat line NEWLINE))
         (chars-length (length chars)))

    ;; (print chars)
    ;; (print "^^^^^^ chars to be processed")

    (while (< i chars-length)
      (parinferlib--process-char result (string (aref chars i)))

      ;;(print (concat "xx" (string (aref chars i)) "xx"))
      ;;(print (gethash :parenStack result))
      ;;(print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

      (setq i (1+ i))))

  (when (equal (gethash :lineNo result)
               (gethash :parenTrailLineNo result))
    (parinferlib--finish-new-paren-trail result))
  nil)

(defun parinferlib--finalize-result (result)
  (when (gethash :quoteDanger result)
    ()) ;; TODO: figure out throw here
  (when (gethash :isInStr result)
    ()) ;; TODO: figure out throw here
  (let* ((paren-stack (gethash :parenStack result))
         (mode (gethash :mode result)))
    (when (parinferlib--not-empty? paren-stack)
      (when (equal mode :paren)
        ()) ;; TODO: figure out throw here
      (when (equal mode :indent)
        (parinferlib--correct-paren-trail result 0))))
  (puthash :success t result)
  nil)

(defun parinferlib--process-error (result err)
  nil)

(defun parinferlib--process-text (text mode cursor-x cursor-line cursor-dx)
  (let* ((result (parinferlib--create-initial-result text mode cursor-x cursor-line cursor-dx))
         (orig-lines (gethash :origLines result))
         (lines-length (length orig-lines))
         (i 0))
    ;; TODO: figure out try / catch
    (while (< i lines-length)
      (parinferlib--process-line result (aref orig-lines i))
      (setq i (1+ i)))
    (parinferlib--finalize-result result)
    result))

(defun parinferlib--public-result (result)
  (if (gethash :success result)
    (let* ((lines (gethash :lines result))
           (result-text (mapconcat 'identity lines NEWLINE)))
      (list t result-text))
    (let ((orig-text (gethash :origText result))
          (public-error "TODO: create error alist here"))
      (list nil orig-text public-error))))

;;------------------------------------------------------------------------------
;; Public API
;;------------------------------------------------------------------------------

(defun parinferlib-indent-mode (text cursor-x cursor-line cursor-dx)
  "Indent Mode public function."
  (let ((result (parinferlib--process-text text :indent cursor-x cursor-line cursor-dx)))
    ;(print result)
    (parinferlib--public-result result)))

(defun parinferlib-paren-mode (text cursor-x cursor-line cursor-dx)
  "Paren Mode public function"
  (let ((result (parinferlib--process-text text :paren cursor-x cursor-line cursor-dx)))
    (parinferlib--public-result result)))

(provide 'parinferlib)

;;; parinfer-lib.el ends here
