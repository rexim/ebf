
(ert-deftest ebf-hello-world-test ()
  (should (string= "Hello World!\n"
                   (let ((result nil))
                     (ebf nil #'(lambda (x) (setq result (cons x result)))
                          ++++++++++\[>+++++++>++++++++++>+++>+<<<<-\]>++
                          .>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.
                          ------.--------.>+.>.)
                     (apply #'string (reverse result))))))
