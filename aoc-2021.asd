#|
 This file is a part of aoc-2021
 (c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
 Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:cl-user)
(asdf:defsystem aoc-2021
  :version "0.0.0"
  :license "zlib"
  :author "Janne Pakarinen <gingeralesy@gmail.com>"
  :maintainer "Janne Pakarinen <gingeralesy@gmail.com>"
  :description "Advent of Code 2021 - https://adventofcode.com/"
  :serial T
  :components ((:file "package")
               (:file "util")
               (:file "day1"))
  :depends-on (:alexandria
               :asdf))
