
(ert-deftest ebf-hello-world-test ()
  (should (string= "Hello World!\n"
                   (let ((result nil))
                     (ebf nil #'(lambda (x) (setq result (cons x result)))
                          ++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++
                          .>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.
                          ------.--------.>+.>.)
                     (apply #'string (reverse result))))))

(defun rot-13 (text)
  (let ((result nil))
    (ebf (ebf-string-input text)
         (lambda (x) (push x result))
         -\,+
         [-
          [>>++++
           [>++++++++<-]
           <+<-
           [>+>+>-
            [>>>]
            <
            [[>+<-]>>+>]
            <<<<<-]]
          >>>[-]+
          >--
          [-[<->+++[-]]]<
          [++++++++++++<
           [>-
            [>+>>]
            >[+[<+>-]>+>>]
            <<<<<-]
           >>[<+>-]
           >[-[-<<[-]>>]<<[<<->>-]>>]
           <<[<<+>>-]]
          <[-]
          <.[-]
          <-\,+])
    (apply #'string (reverse result))))

(ert-deftest ebf-rot-13-test ()
  (should (string= (rot-13 "hello") "uryyb"))
  (should (string= (rot-13 "uryyb") "hello")))
