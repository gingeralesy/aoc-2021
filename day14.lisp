#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defparameter *day14-input* (local-file #P"day14.txt" :error T))

(defun d14-data ()
  (with-open-file (stream *day14-input* :if-does-not-exist :error)
    (values
     (loop for ch across (read-line stream NIL) collecting ch)
     (loop with scanner = (cl-ppcre:create-scanner "^(\\w+) -> (\\w+)$")
           for line = (read-line stream NIL)
           while line
           when (cl-ppcre:scan scanner line)
           collect (cl-ppcre:register-groups-bind (from to)
                       (scanner line)
                     (unless (and from to) (error "Invalid pair insertion rule: ~a" line))
                     (cons (cons (char from 0) (char from 1))
                           (char to 0)))))))
