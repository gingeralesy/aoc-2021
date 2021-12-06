#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defparameter *day5-input* (local-file #P"day5.txt" :error T))

(defun d5-data ()
  (with-open-file (stream *day5-input* :if-does-not-exist :error)
    (loop with scanner = (cl-ppcre:create-scanner "^(\\d+),(\\d+) -> (\\d+),(\\d+)$")
          with max-x = 0
          with max-y = 0
          for line = (read-line stream NIL)
          while line
          collect (cl-ppcre:register-groups-bind (x1 y1 x2 y2)
                      (scanner line)
                    (unless (and x1 y1 x2 y2) (error "Invalid data: ~a" line))
                    (let ((x1 (parse-integer x1))
                          (y1 (parse-integer y1))
                          (x2 (parse-integer x2))
                          (y2 (parse-integer y2)))
                      (setf max-x (max max-x x1 x2))
                      (setf max-y (max max-y y1 y2))
                      (list (cons x1 y1) (cons x2 y2))))
          into coordinates
          finally (return (values coordinates (1+ max-x) (1+ max-y))))))

#|
--- Day 5: Hydrothermal Venture ---

You come across a field of hydrothermal vents on the ocean floor! These vents constantly produce
large, opaque clouds, so it would be best to avoid them if possible.

They tend to form in lines; the submarine helpfully produces a list of nearby lines of vents (your
puzzle input) for you to review. For example:

0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2

Each line of vents is given as a line segment in the format x1,y1 -> x2,y2 where x1,y1 are the
coordinates of one end the line segment and x2,y2 are the coordinates of the other end. These line
segments include the points at both ends. In other words:

- An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
- An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.

For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2.

So, the horizontal and vertical lines from the above list would produce the following diagram:

.......1..
..1....1..
..1....1..
.......1..
.112111211
..........
..........
..........
..........
222111....

In this diagram, the top left corner is 0,0 and the bottom right corner is 9,9. Each position is
shown as the number of lines which cover that point or . if no line covers that point. The top-left
pair of 1s, for example, comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping
lines 0,9 -> 5,9 and 0,9 -> 2,9.

To avoid the most dangerous areas, you need to determine the number of points where at least two
lines overlap. In the above example, this is anywhere in the diagram with a 2 or larger - a total of
5 points.

Consider only horizontal and vertical lines. At how many points do at least two lines overlap?
|#

(defun d5p1 ()
  (multiple-value-bind (coordinates width height)
      (d5-data)
    (let ((diagram (make-array (list width height) :element-type 'integer :initial-element 0)))
      (loop for ((x1 . y1) (x2 . y2)) in coordinates
            when (or (= x1 x2) (= y1 y2))
            do (if (= y1 y2)
                   (loop for x from (min x1 x2) to (max x1 x2)
                         do (incf (aref diagram y1 x)))
                   (loop for y from (min y1 y2) to (max y1 y2)
                         do (incf (aref diagram y x1)))))
      (loop with danger = 0
            for y from 0 below height
            do (loop for x from 0 below width
                     when (< 1 (aref diagram y x))
                     do (incf danger))
            finally (return danger)))))

;; Answer: 5306
