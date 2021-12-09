#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defparameter *day9-input* (local-file #P"day9.txt" :error T))

(defun d9-data ()
  (with-open-file (stream *day9-input* :if-does-not-exist :error)
    (loop with queue = (queue-make)
          with width = 0
          for line = (read-line stream NIL)
          while line
          counting line into height
          do (loop for ch across line
                   counting ch into count
                   do (queue-push (parse-integer (format NIL "~c" ch)) queue)
                   finally (setf width count))
          finally (return
                    (values
                     (loop with heightmap = (make-array (list height width)
                                                        :element-type '(unsigned-byte 4)
                                                        :initial-element 0)
                           for node in (queue-as-list queue)
                           for count from 0
                           do (setf (aref heightmap (floor count width) (mod count width)) node)
                           finally (return heightmap))
                     width
                     height)))))

#|
--- Day 9: Smoke Basin ---

These caves seem to be lava tubes. Parts are even still volcanically active; small hydrothermal
vents release smoke into the caves that slowly settles like rain.

If you can model how the smoke flows through the caves, you might be able to avoid it and be that
much safer. The submarine generates a heightmap of the floor of the nearby caves for you (your
puzzle input).

Smoke flows to the lowest point of the area it's in. For example, consider the following heightmap:

2199943210
3987894921
9856789892
8767896789
9899965678

Each number corresponds to the height of a particular location, where 9 is the highest and 0 is the
lowest a location can be.

Your first goal is to find the low points - the locations that are lower than any of its adjacent
locations. Most locations have four adjacent locations (up, down, left, and right); locations on the
edge or corner of the map have three or two adjacent locations, respectively. (Diagonal locations do
not count as adjacent.)

In the above example, there are four low points, all highlighted: two are in the first row (a 1 and
a 0), one is in the third row (a 5), and one is in the bottom row (also a 5). All other locations on
the heightmap have some lower adjacent location, and so are not low points.

The risk level of a low point is 1 plus its height. In the above example, the risk levels of the low
points are 2, 1, 6, and 6. The sum of the risk levels of all low points in the heightmap is
therefore 15.

Find all of the low points on your heightmap. What is the sum of the risk levels of all low points
on your heightmap?
|#

(defun d9p1 ()
  (multiple-value-bind (heightmap width height)
      (d9-data)
    (loop for y from 0 below height
          sum (loop for x from 0 below width
                    for current = (aref heightmap y x)
                    for left = (if (< 0 x) (aref heightmap y (1- x)) 10)
                    for right = (if (< x (1- width)) (aref heightmap y (1+ x)) 10)
                    for up = (if (< 0 y) (aref heightmap (1- y) x) 10)
                    for down = (if (< y (1- height)) (aref heightmap (1+ y) x) 10)
                    when (and (< current left) (< current right) (< current up) (< current down))
                    sum (1+  current)))))

;; Answer: 462

#|
--- Part Two ---

Next, you need to find the largest basins so you know what areas are most important to avoid.

A basin is all locations that eventually flow downward to a single low point. Therefore, every low
point has a basin, although some basins are very small. Locations of height 9 do not count as being
in any basin, and all other locations will always be part of exactly one basin.

The size of a basin is the number of locations within the basin, including the low point. The
example above has four basins.

The top-left basin, size 3:

2199943210
3987894921
9856789892
8767896789
9899965678

The top-right basin, size 9:

2199943210
3987894921
9856789892
8767896789
9899965678

The middle basin, size 14:

2199943210
3987894921
9856789892
8767896789
9899965678

The bottom-right basin, size 9:

2199943210
3987894921
9856789892
8767896789
9899965678

Find the three largest basins and multiply their sizes together. In the above example, this is
9 * 14 * 9 = 1134.

What do you get if you multiply together the sizes of the three largest basins?
|#

(defun d9p2 ()
  (multiple-value-bind (heightmap width height)
      (d9-data)
    (labels ((node-at (x y)
               (if (and (<= 0 x) (< x width) (<= 0 y) (< y height))
                   (aref heightmap y x)
                   10))
             (sides (x y)
               (list (node-at (1- x) y)
                     (node-at (1+ x) y)
                     (node-at x (1- y))
                     (node-at x (1+ y))))
             (basin (x y &optional visited)
               (let ((current (node-at x y)))
                 (when (and (< current 9) (or (null visited) (not (aref visited y x))))
                   (let ((visited (or visited
                                      (make-array (list height width)
                                                  :element-type 'boolean
                                                  :initial-element NIL))))
                     (destructuring-bind (left right up down)
                         (sides x y)
                       (setf (aref visited y x) T)
                       (nconc (list current)
                              (when (< left 9) (basin (1- x) y visited))
                              (when (< right 9) (basin (1+ x) y visited))
                              (when (< up 9) (basin x (1- y) visited))
                              (when (< down 9) (basin x (1+ y) visited)))))))))
      (let ((sizes
              (sort
               (loop for y from 0 below height
                     append (loop for x from 0 below width
                                  for current = (node-at x y)
                                  when (< current (loop for side in (sides x y) minimize side))
                                  collect (length (basin x y))))
               #'>)))
        (apply #'* (subseq sizes 0 3))))))

;; Answer: 1397760
