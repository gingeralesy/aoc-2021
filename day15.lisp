#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defparameter *day15-input* (local-file #P"day15.txt" :error T))

(defun d15-data ()
  (with-open-file (stream *day15-input* :if-does-not-exist :error)
    (loop with width = 0
          for line = (read-line stream NIL)
          while line
          counting line into height
          collect (loop for value across line
                        when (= 1 height) do (incf width)
                        collecting (parse-integer (format NIL "~c" value)))
          into values
          finally (return
                    (values
                     (make-array (list height width) :element-type '(unsigned-byte 4)
                                                     :initial-contents values)
                     width height)))))
