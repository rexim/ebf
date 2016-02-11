;;; ebf.el --- brainfuck language transpiler to Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2016 Alexey Kutepov a.k.a rexim

;; Author: Alexey Kutepov <reximkut@gmail.com>
;; Maintainer: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/rexim/ebf
;; Version: 1.0.0
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
(require 'dash-functional)

(defconst ebf-initial-memory-size 100
  "Initial size of the ebf memory buffer")

(defvar ebf--input-callback-symbol nil)
(defvar ebf--output-callback-symbol nil)
(defvar ebf--memory-symbol nil)
(defvar ebf--pointer-symbol nil)

(defun ebf--repeat-action (action times)
  (if (= 1 times)
      `(,action)
    `((dotimes (,(cl-gensym "I") ,times)
         ,action))))

(defun ebf--compile-rle-group (rle-group)
  (let ((instruction (car rle-group))
        (size (cdr rle-group)))
    (cl-case instruction
      (?> `((cl-incf ,ebf--pointer-symbol ,size)
            (while (<= (length ,ebf--memory-symbol) ,ebf--pointer-symbol)
              (let ((memory-length (length ,ebf--memory-symbol)))
                (setq ,ebf--memory-symbol
                      (vconcat
                       ,ebf--memory-symbol
                       (make-vector (max 1 (/ memory-length 2))
                                    0)))))))
      (?< `((cl-decf ,ebf--pointer-symbol ,size)))
      (?+ `((cl-incf (aref ,ebf--memory-symbol ,ebf--pointer-symbol) ,size)))
      (?- `((cl-decf (aref ,ebf--memory-symbol ,ebf--pointer-symbol) ,size)))
      (?. (ebf--repeat-action `(funcall ,ebf--output-callback-symbol
                                        (aref ,ebf--memory-symbol
                                              ,ebf--pointer-symbol))
                              size))
      (?, (ebf--repeat-action `(aset ,ebf--memory-symbol
                                     ,ebf--pointer-symbol
                                     (funcall ,ebf--input-callback-symbol))
                              size)))))

(defun ebf--rle-group-chunk-of-instructions (chunk-of-instructions)
  (->> chunk-of-instructions
       (mapcar #'identity)
       (-partition-by #'identity)
       (-map (-lambda (xs) (cons (car xs) (length xs))))))

(defun ebf--compile-chunk-of-instructions (chunk-of-instructions)
  (cond ((stringp chunk-of-instructions)
         (->> chunk-of-instructions
              (ebf--rle-group-chunk-of-instructions)
              (-mapcat #'ebf--compile-rle-group)))
        ((listp chunk-of-instructions)
         (if (or (equal chunk-of-instructions '("+"))
                 (equal chunk-of-instructions '("-")))
             (list `(aset ,ebf--memory-symbol
                          ,ebf--pointer-symbol
                          0))
           (list `(while (not (zerop (aref ,ebf--memory-symbol
                                           ,ebf--pointer-symbol)))
                    ,@(ebf--compile-instructions
                       chunk-of-instructions)))))))

(defun ebf--compile-instructions (instructions)
  (->> instructions
       (-map #'ebf--compile-chunk-of-instructions)
       (apply #'append)))

(defun ebf--merge-chunks (chunks)
  (if (-all-p #'symbolp chunks)
      (->> chunks
           (-map #'symbol-name)
           (apply #'concat)
           (list))
    (-map (-compose
           #'ebf--normalize-instructions
           (-partial #'mapcar #'identity))
          chunks)))

(defun ebf--normalize-instructions (instructions)
  (->> instructions
       (-partition-by #'symbolp)
       (-map #'ebf--merge-chunks)
       (apply #'append)))

(defun ebf--verify-one-instruction (instruction)
  (when (not (-find (-partial #'= instruction)
                    (list ?- ?+ ?> ?< ?. ?,)))
    (error "`%c' is not a valid ebf brainfuck instruction"
           instruction)))

(defun ebf--verify-instructions (instructions)
  (dolist (chunk-of-instructions instructions)
    (cond ((symbolp chunk-of-instructions)
           (-each (mapcar #'identity (symbol-name chunk-of-instructions))
             #'ebf--verify-one-instruction))

          ((vectorp chunk-of-instructions)
           (->> chunk-of-instructions
                (mapcar #'identity)
                (ebf--verify-instructions)))

          (t (error (concat "%s is neither symbol nor vector of symbols. "
                            "Please check the ebf macro documentation.")
                    chunk-of-instructions)))))

(defmacro ebf (input-callback output-callback &rest instructions)
  "Brainfuck language transpiler macro.
INPUT-CALLBACK is called on comma instruction and should have
zero arguments and return a number.

OUTPUT-CALLBACK is called on dot instruction and should have one
argument of an integer type.

INSTRUCTIONS is a list of symbols and vectors of symbols. Vectors
are accepted so we don't need to escape square brackets of our
brainfuck program. Symbols' names should be sequences of valid
brainfuck instructions excluding square brackets.

Evaluation of the macro expansion causes the brainfuck program
execution."
  (ebf--verify-instructions instructions)
  (let ((ebf--input-callback-symbol (cl-gensym "INPUT"))
        (ebf--output-callback-symbol (cl-gensym "OUTPUT"))
        (ebf--memory-symbol (cl-gensym "MEMORY"))
        (ebf--pointer-symbol (cl-gensym "POINTER")))
    `(let ((,ebf--memory-symbol (make-vector ,ebf-initial-memory-size 0))
           (,ebf--pointer-symbol 0)
           (,ebf--input-callback-symbol ,input-callback)
           (,ebf--output-callback-symbol ,output-callback))
       ,@(->> instructions
              (ebf--normalize-instructions)
              (ebf--compile-instructions)))))

(provide 'ebf)

;;; ebf.el ends here
