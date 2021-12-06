#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defun local-file (filename &key error)
  (let ((file (asdf:system-relative-pathname :aoc-2021 filename)))
    (when (and error (not (probe-file file)))
      (error "Missing file: ~a" filename))
    file))
