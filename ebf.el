;;; ebf.el --- brainfuck language transpiler to Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2014 Alexey Kutepov a.k.a rexim

;; Author: Alexey Kutepov <reximkut@gmail.com>
;; Maintainer: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/rexim/ebf
;; Version: 0.0.1
;; Package-Requires: ((dash "2.11.0") (dash-functional "1.2.0") (cl-lib "0.5"))

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

(defvar ebf--input-callback nil)
(defvar ebf--output-callback nil)
(defvar ebf--memory-symbol nil)
(defvar ebf--pointer-symbol nil)

(defun ebf--compile-one-instruction (instruction)
  (cl-case instruction
    (?> `(cl-incf ,ebf--pointer-symbol))
    (?< `(cl-decf ,ebf--pointer-symbol))
    (?+ `(cl-incf (aref ,ebf--memory-symbol ,ebf--pointer-symbol)))
    (?- `(cl-decf (aref ,ebf--memory-symbol ,ebf--pointer-symbol)))
    (?. `(funcall ,ebf--output-callback (aref ,ebf--memory-symbol ,ebf--pointer-symbol)))
    (?, `(aset ,ebf--memory-symbol ,ebf--pointer-symbol (funcall ,ebf--input-callback)))))

(defun ebf--compile-chunk-of-instructions (chunk-of-instructions)
  (cond ((symbolp chunk-of-instructions)
         (->> chunk-of-instructions
              (symbol-name)
              (-map #'ebf--compile-one-instruction)))
        ((vectorp chunk-of-instructions)
         (list `(while (not (zerop (aref ,ebf--memory-symbol
                                         ,ebf--pointer-symbol)))
                  ,@(ebf--compile-instructions
                     chunk-of-instructions))))))

(defun ebf--compile-instructions (instructions)
  (->> instructions
       (-map #'ebf--compile-chunk-of-instructions)
       (apply #'append)))

(defun ebf-string-input (s)
  "Constructs an ebf input callback for feeding stirng as an input.
The result of this function can by passed as INPUT-CALLBACK to
the ebf macro. The end of the input is indicated by -1."
  (let ((chars (mapcar #'identity s)))
    (lambda ()
      (if (not chars) -1 (pop chars)))))

(defmacro ebf (input-callback output-callback &rest instructions)
  "Brainfuck language transpiler macro.
INPUT-CALLBACK is called on dot instruction and should have zero
arguments and return a number. OUTPUT-CALLBACK is called on
comman instruction and should have one argument of an integer
type. INSTRUCTIONS is a list of symbols which names are sequences
of brainfuck instructions."
  (let ((ebf--input-callback input-callback)
        (ebf--output-callback output-callback)
        (ebf--memory-symbol (cl-gensym "MEMORY"))
        (ebf--pointer-symbol (cl-gensym "POINTER")))
    `(let ((,ebf--memory-symbol (make-vector 100 0))
           (,ebf--pointer-symbol 0))
       ,@(ebf--compile-instructions instructions))))

;;; ebf.el ends here
