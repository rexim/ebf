;;; ebf--translator.el --- brainfuck language transpiler to Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2016 Alexey Kutepov <reximkut@gmail.com>

;; Author: Alexey Kutepov <reximkut@gmail.com>
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

;; Routines for bf language translation to Emacs Lisp

;;; Code:

(require 'ebf--optimizer)

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

(provide 'ebf--translator)

;;; ebf--translator.el ends here
