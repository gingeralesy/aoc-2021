#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defparameter *day18-input* (local-file #P"day18.txt" :error T))

(defstruct d18-stack
  (head NIL :type (or null list)))

(defun d18-data ()
  (with-open-file (stream *day18-input* :if-does-not-exist :error)
    (labels ((stack-peek (stack) (car (d18-stack-head stack)))
             (stack-pop (stack) (pop (d18-stack-head stack)))
             (tokenify (string)
               (make-d18-stack :head (loop for ch across string
                                           unless (char= #\, ch) collect ch)))
             (parse (stack)
               (unless (stack-peek stack) (error "Empty stack."))
               (let ((token (stack-pop stack)))
                 (case token
                   (#\[
                    (prog1 (cons (parse stack) (parse stack))
                      (unless (char= #\] (stack-pop stack)) (error "Expected ']'."))))
                   (#\] (error "Unexpected ']'."))
                   (T (parse-integer (format NIL "~c" token)))))))
      (loop for line = (read-line stream NIL)
            while line
            unless (char= #\[ (char line 0)) do (error "Expected '[': ~a" line)
            collect (parse (tokenify line))))))
