(require 'benchmark)
(add-to-list 'load-path (expand-file-name "."))
(load "parinferlib")

;; from: http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun print-times (mode times)
  "Print the output of benchmark-run."
  (message "  %s: %fs (w/ %d garbage collects in %fs)"
    mode
    (nth 0 times)
    (nth 1 times)
    (nth 2 times)))

(let* ((filename "tests/really_long_file")
       (text (get-string-from-file filename))
       (indent-times (benchmark-run (parinferlib-indent-mode text)))
       (paren-times (benchmark-run (parinferlib-paren-mode text))))
  (message "\nTime to process '%s'" filename)
  (print-times "indent-mode" indent-times)
  (print-times " paren-mode" paren-times))
