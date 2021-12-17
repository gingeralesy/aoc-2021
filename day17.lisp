#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defparameter *day17-input* (local-file #P"day17.txt" :error T))

(defun d17-data ()
  "Returns the input values as x-range and y-range."
  (with-open-file (stream *day17-input* :if-does-not-exist :error)
    (multiple-value-bind (result groups)
        (cl-ppcre:scan-to-strings
         "^target area: x=(-?\\d+)..(-?\\d+), y=(-?\\d+)..(-?\\d+)$"
         (read-line stream NIL))
      (when (and result (= 4 (length groups)))
        (values (cons (parse-integer (aref groups 0))
                      (parse-integer (aref groups 1)))
                (cons (parse-integer (aref groups 2))
                      (parse-integer (aref groups 3))))))))
