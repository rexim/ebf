(eval-when-compile
  (require 'ebf))

(defun plus (x y)
  (let ((input (list x y))
        (output nil))
    (ebf (lambda () (pop input))
         (lambda (x) (setq output x))
         \,>\,<
         [->>+<<]
         >
         [->+<]
         >.)
    output))

(provide 'plus)
