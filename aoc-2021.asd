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
               (:file "day1")
               (:file "day2")
               (:file "day3")
               (:file "day4")
               (:file "day5")
               (:file "day6")
               (:file "day7")
               (:file "day8")
               (:file "day9")
               (:file "day10")
               (:file "day11"))
  :depends-on (:alexandria
               :asdf
               :cl-ppcre))
