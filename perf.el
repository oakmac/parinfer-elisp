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
    (/ (nth 0 times) 5.0)
    (/ (nth 1 times) 5.0)
    (/ (nth 2 times) 5.0)))

(let* ((filename "tests/really_long_file")
       (text (get-string-from-file filename))
       (indent-times (benchmark-run 5 (parinferlib-indent-mode text)))
       (paren-times (benchmark-run 5 (parinferlib-paren-mode text))))
  (message "\nAverage time to process '%s' (across 5 runs)" filename)
  (print-times "indent-mode" indent-times)
  (print-times " paren-mode" paren-times))
