(eval-when-compile
  (require 'ebf))

(defun rot13 (text)
  (let ((input (mapcar #'identity text))
        (result nil))
    (ebf (lambda () (if (not input) -1 (pop input)))
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

(provide 'rot13)
