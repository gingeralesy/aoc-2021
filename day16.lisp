#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defparameter *day16-input* (local-file #P"day16.txt" :error T))

(defun d16-data ()
  (with-open-file (stream *day16-input* :if-does-not-exist :error)
    (loop with line = (read-line stream NIL)
          for ch across line
          counting ch into count
          collect (parse-integer (format NIL "~c" ch) :radix 16) into nibbles
          finally (return
                    (values (make-array count :element-type '(unsigned-byte 4)
                                              :initial-contents nibbles)
                            count)))))
