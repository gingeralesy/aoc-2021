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

#|
--- Day 15: Chiton ---

You've almost reached the exit of the cave, but the walls are getting closer together. Your
submarine can barely still fit, though; the main problem is that the walls of the cave are covered
in chitons, and it would be best not to bump any of them.

The cavern is large, but has a very low ceiling, restricting your motion to two dimensions. The
shape of the cavern resembles a square; a quick scan of chiton density produces a map of risk level
throughout the cave (your puzzle input). For example:

1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581

You start in the top left position, your destination is the bottom right position, and you cannot
move diagonally. The number at each position is its risk level; to determine the total risk of an
entire path, add up the risk levels of each position you enter (that is, don't count the risk level
of your starting position unless you enter it; leaving it adds no risk to your total).

The total risk of this path of the example is 40 (the starting position is never entered, so its
risk is not counted).

What is the lowest total risk of any path from the top left to the bottom right?
|#

(defparameter *d15-print-symbols* " .~:=$%&@#█")

(defstruct d15-node
  (pos (cons 0 0) :type cons :read-only T) ;; The (x . y) coordinate.
  (route NIL) ;; The previous node of the cheapest path.
  (g-value #xffffffff :type integer) ;; Cost of travel thus far.
  (f-value #xffffffff :type integer)) ;; Approximated cost to end (g-value + heuristics).

(defun d15-print (map width height)
  (loop for row from 0 below height
        do (loop for col from 0 below width
                 for node = (aref map row col)
                 do (format T "~c" (char *d15-print-symbols* node)))
        do (format T "~%")))

(defun d15p1 (&optional (start '(0 . 0)) end)
  "Finds the route through a map with A* algorithm."
  (multiple-value-bind (map width height)
      (d15-data)
    (let ((end (or end (cons (1- width) (1- height))))
          (nodes (make-array (list height width) :initial-element NIL))
          (open (queue-make)))
      (dotimes (y height)
        (dotimes (x width)
          (setf (aref nodes y x) (make-d15-node :pos (cons x y) :route NIL))))
      (labels ((node (x y) (aref nodes y x))
               (coord= (a b) (and (= (car a) (car b)) (= (cdr a) (cdr b))))
               (node= (a b) (coord= (d15-node-pos a) (d15-node-pos b)))
               (heuristic (from to) (+ (abs (- (car to) (car from))) (abs (- (cdr to) (cdr from)))))
               (neighbours (prev)
                 (loop with pos = (d15-node-pos prev)
                       for delta in '((0 . -1) (-1 . 0) (0 . 1) (1 . 0))
                       for x1 = (+ (car pos) (car delta))
                       for y1 = (+ (cdr pos) (cdr delta))
                       for node = (unless (or (< x1 0) (< y1 0) (<= width x1) (<= height y1))
                                    (let* ((node (node x1 y1))
                                           (g-value (+ (d15-node-g-value prev) (aref map y1 x1)))
                                           (f-value (+ g-value (heuristic (cons x1 y1) end))))
                                      (when (< g-value (d15-node-g-value node))
                                        (setf (d15-node-g-value node) g-value)
                                        (setf (d15-node-f-value node) f-value)
                                        (setf (d15-node-route node) prev)
                                        node)))
                       when node collect node))
               (count-path (from)
                 (loop for prev = from then (d15-node-route prev)
                       while (and prev (d15-node-route prev))
                       for pos = (d15-node-pos prev)
                       sum (aref map (cdr pos) (car pos)))))
        (let ((start (node (car start) (cdr start))))
          (setf (d15-node-g-value start) 0)
          (setf (d15-node-f-value start) (heuristic (d15-node-pos start) end))
          (queue-push start open))
        (loop with end = (node (car end) (cdr end))
              until (queue-empty-p open)
              for current = (pqueue-pop open #'d15-node-f-value)
              do (when (node= current end)
                   (return-from d15p1 (count-path current)))
              do (loop for neighbour in (neighbours current)
                       unless (queue-find neighbour open #'identity #'node=)
                       do (queue-push neighbour open)))))))

;; Answer: 527
