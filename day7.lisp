#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defparameter *day7-input* (local-file #P"day7.txt" :error T))

(defun d7-data ()
  "Returns the input values as a list, the largest input value, and the number of input values."
  (with-open-file (stream *day7-input* :if-does-not-exist :error)
    (let ((scanner (cl-ppcre:create-scanner "\\d+"))
          (count 0)
          (max 0)
          (values (queue-make)))
      (cl-ppcre:do-matches-as-strings (match scanner (read-line stream))
        (let ((value (parse-integer match)))
          (queue-push value values)
          (when (< max value) (setf max value)))
        (incf count))
      (values (queue-as-list values) max count))))
