(in-package #:aoc-2021)

;; https://adventofcode.com/2021/day/21

(defparameter *day21-input* (local-file #P"day21.txt" :error T))

(defstruct (d21-state (:constructor %make-d21-state (space-1 space-2)))
  (space-1 1 :type (integer 0 9))
  (space-2 1 :type (integer 0 9))
  (score-1 0 :type (unsigned-byte 32))
  (score-2 0 :type (unsigned-byte 32))
  (roll-count 0 :type (unsigned-byte 32)))

(declaim (inline d21-data))
(defun d21-data ()
  (declare (optimize (speed 3)))
  (with-open-file (stream *day21-input* :if-does-not-exist :error)
    (%make-d21-state (1- (the (integer 1 10) (parse-integer (read-line stream) :start 28)))
                     (1- (the (integer 1 10) (parse-integer (read-line stream) :start 28))))))

(declaim (ftype (function (d21-state (unsigned-byte 32) (function () (unsigned-byte 8))) d21-state)
                d21-play))
(defun d21-play (state until roll-f)
  (declare (type d21-state state))
  (declare (type (unsigned-byte 32) until))
  (declare (type (function () (unsigned-byte 8)) roll-f))
  (declare (optimize (speed 3)))
  (with-slots (space-1 space-2 score-1 score-2 roll-count) state
    (declare (type (integer 0 9) space-1 space-2))
    (declare (type (unsigned-byte 32) score-1 score-2 roll-count))
    (loop for move-1 = (+ (funcall roll-f) (funcall roll-f) (funcall roll-f))
          for move-2 = (+ (funcall roll-f) (funcall roll-f) (funcall roll-f))
          do (incf roll-count 3)
          do (setf space-1 (mod (+ space-1 move-1) 10))
          do (incf score-1 (1+ space-1))
          while (< score-1 until)
          do (incf roll-count 3)
          do (setf space-2 (mod (+ space-2 move-2) 10))
          do (incf score-2 (1+ space-2))
          while (< score-2 until)
          finally (return state))))

(defun d21p1 ()
  (declare (optimize (speed 3)))
  (let ((die 0)
        (state (d21-data)))
    (declare (type (unsigned-byte 8) die))
    (d21-play state 1000
              #'(lambda ()
                  (declare (optimize (speed 3)))
                  (let ((value die))
                    (setf die (mod (1+ die) 100))
                    (1+ value))))
    (with-slots (score-1 score-2 roll-count) state
      (values (the (unsigned-byte 32) (* (min score-1 score-2) roll-count)) state))))

;; Answer: 926610
