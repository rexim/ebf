;;; ebf.el --- brainfuck language transpiler to Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2014 Alexey Kutepov a.k.a rexim

;; Author: Alexey Kutepov <reximkut@gmail.com>
;; Maintainer: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/rexim/ebf

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;; 
;; Brainfuck language tanspiler to Emacs Lisp in a form of a macro

;;; Code:

(require 'cl-lib)
(require 'dash)

(defun ebf--compile-instruction (instruction input-callback output-callback)
  (cl-case instruction
    (?> '(cl-incf pointer))
    (?< '(cl-decf pointer))
    (?+ '(cl-incf (aref memory pointer)))
    (?- '(cl-decf (aref memory pointer)))
    (?. `(funcall ,output-callback (aref memory pointer)))
    (?, `(aset memory pointer (funcall ,input-callback)))))

(defun ebf--compile-instructions (instructions input-callback output-callback)
  (if instructions
      (cl-case (car instructions)
        (?\[ (-let (((compiled . rest)
                     (ebf--compile-instructions (cdr instructions)
                                                input-callback
                                                output-callback)))
               (-let (((compiled2 . rest2)
                       (ebf--compile-instructions rest input-callback output-callback)))
                 (cons (cons `(while (not (zerop (aref memory pointer)))
                                ,@compiled)
                             compiled2) rest2))))
        (?\] (cons nil (cdr instructions)))
        (otherwise (-let (((compiled . rest)
                           (ebf--compile-instructions
                            (cdr instructions)
                            input-callback
                            output-callback)))
                     (cons (cons (ebf--compile-instruction (car instructions)
                                                           input-callback
                                                           output-callback)
                                 compiled) rest))))
    (cons nil nil)))

(defmacro ebf (input-callback output-callback &rest instructions)
  "Brainfuck language transpiler macro.
INPUT-CALLBACK is called on dot instruction and should have zero
arguments and return a number. OUTPUT-CALLBACK is called on
comman instruction and should have one argument of an integer
type. INSTRUCTIONS is a list of symbols which names are sequences
of brainfuck instructions."
  `(let ((memory (make-vector 100 0))
         (pointer 0))
     ,@(car (ebf--compile-instructions
             (mapcar #'identity
                     (apply #'concat
                            (mapcar #'symbol-name instructions)))
             input-callback
             output-callback))))

;;; ebf.el ends here
