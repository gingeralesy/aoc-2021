#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defparameter *day13-input* (local-file #P"day13.txt" :error T))

(defun d13-data ()
  "Parses the input and returns a list of coordinates, locations of the folds, and
the width and height of the coordinate grid."
  (with-open-file (stream *day13-input* :if-does-not-exist :error)
    (loop with coordinate-scanner = (cl-ppcre:create-scanner "^(\\d+),(\\d+)$")
          with fold-scanner = (cl-ppcre:create-scanner "^fold along (\\w)=(\\d+)$")
          with coordinates = (queue-make)
          with folds = (queue-make)
          with get-coordinates-p = T
          with max-x = 0
          with max-y = 0
          for line = (read-line stream NIL)
          while line
          when (= 0 (length line)) do (setf get-coordinates-p NIL)
          when get-coordinates-p
          do (cl-ppcre:register-groups-bind (x y)
                 (coordinate-scanner line)
               (unless (and x y) (error "Invalid coordinates: ~a" line))
               (let ((x (parse-integer x))
                     (y (parse-integer y)))
                 (when (< max-x x) (setf max-x x))
                 (when (< max-y y) (setf max-y y))
                 (queue-push (cons x y) coordinates)))
          unless (or get-coordinates-p (= 0 (length line)))
          do (cl-ppcre:register-groups-bind (coordinate value)
                 (fold-scanner line)
               (unless (and coordinate value) (error "Invalid fold: ~a" line))
               (let ((coordinate (intern (string-upcase coordinate) :keyword))
                     (value (parse-integer value)))
                 (queue-push (cons coordinate value) folds)))
          finally (return
                    (values
                     (queue-as-list coordinates)
                     (queue-as-list folds)
                     (1+ max-x) (1+ max-y))))))

#|
--- Day 13: Transparent Origami ---

You reach another volcanically active part of the cave. It would be nice if you could do some kind
of thermal imaging so you could tell ahead of time which caves are too hot to safely enter.

Fortunately, the submarine seems to be equipped with a thermal camera! When you activate it, you are
greeted with:

| Congratulations on your purchase! To activate this infrared thermal imaging camera system, please
| enter the code found on page 1 of the manual.

Apparently, the Elves have never used this feature. To your surprise, you manage to find the manual;
as you go to open it, page 1 falls out. It's a large sheet of transparent paper! The transparent
paper is marked with random dots and includes instructions on how to fold it up (your puzzle input).
For example:

6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5

The first section is a list of dots on the transparent paper. 0,0 represents the top-left
coordinate. The first value, x, increases to the right. The second value, y, increases downward. So,
the coordinate 3,0 is to the right of 0,0, and the coordinate 0,7 is below 0,0. The coordinates in
this example form the following pattern, where # is a dot on the paper and . is an empty, unmarked
position:

...#..#..#.
....#......
...........
#..........
...#....#.#
...........
...........
...........
...........
...........
.#....#.##.
....#......
......#...#
#..........
#.#........

Then, there is a list of fold instructions. Each instruction indicates a line on the transparent
paper and wants you to fold the paper up (for horizontal y=... lines) or left (for vertical x=...
lines). In this example, the first fold instruction is fold along y=7, which designates the line
formed by all of the positions where y is 7 (marked here with -):

...#..#..#.
....#......
...........
#..........
...#....#.#
...........
...........
-----------
...........
...........
.#....#.##.
....#......
......#...#
#..........
#.#........

Because this is a horizontal line, fold the bottom half up. Some of the dots might end up
overlapping after the fold is complete, but dots will never appear exactly on a fold line. The
result of doing this fold looks like this:

#.##..#..#.
#...#......
......#...#
#...#......
.#.#..#.###
...........
...........

Now, only 17 dots are visible.

Notice, for example, the two dots in the bottom left corner before the transparent paper is folded;
after the fold is complete, those dots appear in the top left corner (at 0,0 and 0,1). Because the
paper is transparent, the dot just below them in the result (at 0,3) remains visible, as it can be
seen through the transparent paper.

Also notice that some dots can end up overlapping; in this case, the dots merge together and become
a single dot.

The second fold instruction is fold along x=5, which indicates this line:

#.##.I#..#.
#...#I.....
.....I#...#
#...#I.....
.#.#.I#.###
.....I.....
.....I.....

Because this is a vertical line, fold left:

#####
#...#
#...#
#...#
#####
.....
.....

The instructions made a square!

The transparent paper is pretty big, so for now, focus on just completing the first fold. After the
first fold in the example above, 17 dots are visible - dots that end up overlapping after the fold
is completed count as a single dot.

How many dots are visible after completing just the first fold instruction on your transparent
paper?
|#

(defun d13p1 ()
  (multiple-value-bind (dots folds width height)
      (d13-data)
    (let ((sheet (make-array (list height width) :element-type 'boolean :initial-element NIL)))
      (loop for (x . y) in dots do (setf (aref sheet y x) T))
      (loop for n from 0 below 1
            for (fold-side . fold-at) in folds
            do (cond
                 ((eql :x fold-side)
                  (loop for x from 1 below (- width fold-at)
                        do (loop for y from 0 below height
                                 when (aref sheet y (+ fold-at x))
                                 do (setf (aref sheet y (- fold-at x)) T)))
                  (setf width fold-at))
                 ((eql :y fold-side)
                  (loop for y from 1 below (- height fold-at)
                        do (loop for x from 0 below width
                                 when (aref sheet (+ fold-at y) x)
                                 do (setf (aref sheet (- fold-at y) x) T)))
                  (setf height fold-at))
                 (T (error "Invalid fold: ~a = ~a" fold-side fold-at))))
      (loop for y from 0 below height
            sum (loop for x from 0 below width count (aref sheet y x))))))

;; Answer: 790

#|
--- Part Two ---

Finish folding the transparent paper according to the instructions. The manual says the code is
always eight capital letters.

What code do you use to activate the infrared thermal imaging camera system?
|#

(defun d13p2 ()
  (multiple-value-bind (dots folds width height)
      (d13-data)
    (let ((sheet (make-array (list height width) :element-type 'boolean :initial-element NIL)))
      (loop for (x . y) in dots do (setf (aref sheet y x) T))
      (loop for (fold-side . fold-at) in folds
            do (cond
                 ((eql :x fold-side)
                  (loop for x from 1 below (- width fold-at)
                        do (loop for y from 0 below height
                                 when (aref sheet y (+ fold-at x))
                                 do (setf (aref sheet y (- fold-at x)) T)))
                  (setf width fold-at))
                 ((eql :y fold-side)
                  (loop for y from 1 below (- height fold-at)
                        do (loop for x from 0 below width
                                 when (aref sheet (+ fold-at y) x)
                                 do (setf (aref sheet (- fold-at y) x) T)))
                  (setf height fold-at))
                 (T (error "Invalid fold: ~a = ~a" fold-side fold-at))))
      (dotimes (y height)
        (dotimes (x width)
          (format T "~c" (if (aref sheet y x) #\# #\.)))
        (format T "~%")))))

;; Answer: PGHZBFJC
