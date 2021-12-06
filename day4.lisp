#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defparameter *day4-input* (local-file #P"day4.txt" :error T))

(defun d4-data ()
  "This is not optimal, but it works. It reads the values into lists and then builds the boards.
TODO: Use queues to avoid reversing everything.
TODO: Also get rid of board-numbers. Just build the boards as you read."
  (with-open-file (stream *day4-input* :if-does-not-exist :error)
    (let ((draw-re (cl-ppcre:create-scanner "\\d+"))
          (draw-numbers)
          (board-numbers)
          (boards))
      ;; Get the draw numbers.
      (cl-ppcre:do-matches-as-strings (match draw-re (read-line stream NIL))
        (push (parse-integer match) draw-numbers))
      ;; Get the board numbers.
      ;; We just read them all into a single list and, because they're iterated backwards,
      ;; we then push them into boards made of lists and it turns out in order.
      (loop for line = (read-line stream NIL)
            while line
            when (< 0 (length line))
            do (cl-ppcre:do-matches-as-strings (match draw-re line)
                 (push (parse-integer match) board-numbers)))
      (loop while board-numbers
            for board = NIL
            do (dotimes (n 5)
                 (let (row)
                   (dotimes (k 5)
                     (push (pop board-numbers) row))
                   (push row board)))
            do (push board boards))
      (values (nreverse draw-numbers)
              (mapcar
               #'(lambda (board)
                   (make-array '(5 5) :element-type 'integer :initial-contents board))
               boards)))))

#|
--- Day 4: Giant Squid ---
You're already almost 1.5km (almost a mile) below the surface of the ocean, already so deep that you
can't see any sunlight. What you can see, however, is a giant squid that has attached itself to the
outside of your submarine.

Maybe it wants to play bingo?

Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. Numbers are chosen at
random, and the chosen number is marked on all boards on which it appears. (Numbers may not appear
on all boards.) If all numbers in any row or any column of a board are marked, that board wins.
(Diagonals don't count.)

The submarine has a bingo subsystem to help passengers (currently, you and the giant squid) pass the
time. It automatically generates a random order in which to draw numbers and a random set of boards
(your puzzle input). For example:

7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7

After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no winners, but the boards
are marked as follows (shown here adjacent to each other to save space):

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

After the next six numbers are drawn (17, 23, 2, 0, 14, and 21), there are still no winners:

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

Finally, 24 is drawn:

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

At this point, the third board wins because it has at least one complete row or column of marked
numbers (in this case, the entire top row is marked: 14 21 17 24 4).

The score of the winning board can now be calculated. Start by finding the sum of all unmarked
numbers on that board; in this case, the sum is 188. Then, multiply that sum by the number that was
just called when the board won, 24, to get the final score, 188 * 24 = 4512.

To guarantee victory against the giant squid, figure out which board will win first. What will your
final score be if you choose that board?
|#

(defun d4-mark-tile (board checkboard number)
  "Marks the tile with the number as checked on the checkboard."
  (loop with found = NIL
        until found
        for y from 0 below 5
        do (loop until found
                 for x from 0 below 5
                 do (setf found (= number (aref board y x)))
                 finally (when found (setf (aref checkboard y x) T)))))

(defun d4-bingo-p (checkboard)
  "Checks if we've found a bingo on the checkboard."
  (loop with row-valid = T
        with col-valid = T
        for n from 0 below 5
        do (setf row-valid T
                 col-valid T)
        until (loop for k from 0 below 5
                    while (or row-valid col-valid)
                    do (when row-valid (setf row-valid (aref checkboard n k)))
                    do (when col-valid (setf col-valid (aref checkboard k n)))
                    finally (return (or row-valid col-valid)))
        finally (return (or row-valid col-valid))))

(defun d4-bingo (board draw-numbers)
  "Finds the number of draws get a bingo on the given board."
  (let ((checkboard (make-array '(5 5) :element-type 'boolean :initial-element NIL)))
    (loop for draw in draw-numbers
          for n from 0
          do (d4-mark-tile board checkboard draw)
          until (d4-bingo-p checkboard)
          finally (return (1+ n)))))

(defun points (board steps draw-numbers)
  "Calculates the points of the board."
  (let ((sum 0))
    (dotimes (n 5)
      (dotimes (k 5)
        (let ((number (aref board n k)))
          (unless (find number draw-numbers :end steps :test #'=)
            (incf sum number)))))
    (* sum (nth (1- steps) draw-numbers))))

(defun d4p1 ()
  (multiple-value-bind (draw-numbers boards)
      (d4-data)
    (loop with min = 1000
          with best = NIL
          for board in boards
          for n from 0
          for steps = (d4-bingo board draw-numbers)
          do (when (< steps min)
               (setf min steps)
               (setf best board))
          finally (return (values (points best min draw-numbers) best min)))))

;; Answer: 63424

#|
--- Part Two ---

On the other hand, it might be wise to try a different strategy: let the giant squid win.

You aren't sure how many bingo boards a giant squid could play at once, so rather than waste time
counting its arms, the safe thing to do is to figure out which board will win last and choose that
one. That way, no matter which boards it picks, it will win for sure.

In the above example, the second board is the last to win, which happens after 13 is eventually
called and its middle column is completely marked. If you were to keep playing until this point, the
second board would have a sum of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.

Figure out which board will win last. Once it wins, what would its final score be?
|#

(defun d4p2 ()
  (multiple-value-bind (draw-numbers boards)
      (d4-data)
    (loop with max = 0
          with worst = NIL
          for board in boards
          for n from 0
          for steps = (d4-bingo board draw-numbers)
          do (when (< max steps)
               (setf max steps)
               (setf worst board))
          finally (return (values (points worst max draw-numbers) worst max)))))

;; Answer: 23541
