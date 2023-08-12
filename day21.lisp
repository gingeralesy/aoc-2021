(in-package #:aoc-2021)

;; https://adventofcode.com/2021/day/21

(defparameter *day21-input* (local-file #P"day21.txt" :error T))

(defstruct (d21p1-state (:constructor %make-d21p1-state (space-1 space-2)))
  (space-1 1 :type (integer 0 9))
  (space-2 1 :type (integer 0 9))
  (score-1 0 :type (unsigned-byte 32))
  (score-2 0 :type (unsigned-byte 32))
  (roll-count 0 :type (unsigned-byte 32)))

(declaim (inline d21p1-data))
(defun d21p1-data ()
  (declare (optimize (speed 3)))
  (with-open-file (stream *day21-input* :if-does-not-exist :error)
    (%make-d21p1-state (1- (the (integer 1 10) (parse-integer (read-line stream) :start 28)))
                       (1- (the (integer 1 10) (parse-integer (read-line stream) :start 28))))))

(declaim (ftype (function (d21p1-state (unsigned-byte 32) (function () (unsigned-byte 8))) d21p1-state)
                d21p1-play))
(defun d21p1-play (state until roll-f)
  (declare (type d21p1-state state))
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
        (state (d21p1-data)))
    (declare (type (unsigned-byte 8) die))
    (d21p1-play state 1000
                #'(lambda ()
                    (declare (optimize (speed 3)))
                    (let ((value die))
                      (setf die (mod (1+ die) 100))
                      (1+ value))))
    (with-slots (score-1 score-2 roll-count) state
      (values (the (unsigned-byte 32) (* (min score-1 score-2) roll-count)) state))))

;; Answer: 926610

(defstruct (d21p2-state (:constructor %make-d21p2-state (turn space-1 space-2 score-1 score-2)))
  (turn 0 :type bit)
  (space-1 1 :type (integer 0 9))
  (space-2 1 :type (integer 0 9))
  (score-1 0 :type (unsigned-byte 8))
  (score-2 0 :type (unsigned-byte 8))
  (multiplier 1 :type (unsigned-byte 32))
  (wins-1 0 :type (unsigned-byte 64))
  (wins-2 0 :type (unsigned-byte 64))
  (splits (make-array 7 :element-type '(or null d21p2-state) :initial-element NIL)
   :type (simple-array (or null d21p2-state) (7))))

(declaim (inline d21p2-data))
(declaim (ftype (function () d21p2-state) d21p2-data))
(defun d21p2-data ()
  (declare (optimize (speed 3)))
  (with-open-file (stream *day21-input* :if-does-not-exist :error)
    (%make-d21p2-state 0
                       (1- (the (integer 1 10) (parse-integer (read-line stream) :start 28)))
                       (1- (the (integer 1 10) (parse-integer (read-line stream) :start 28)))
                       0 0)))

(declaim (ftype (function (d21p2-state) d21p2-state) d21p2-roll))
(defun d21p2-roll (state)
  (declare (type d21p2-state state))
  (declare (optimize (speed 3)))
  (with-slots (turn space-1 space-2 score-1 score-2 splits) state
    (declare (type bit turn))
    (let ((cur-space (if (zerop turn) space-1 space-2))
          (cur-score (if (zerop turn) score-1 score-2)))
      (loop for i from 0 below 7
            for roll from 3 upto 9
            for multiplier in '(1 3 6 7 6 3 1)
            for new-space = (mod (+ roll cur-space) 10)
            for new-score = (+ cur-score (1+ new-space))
            for child = (if (zerop turn)
                            (%make-d21p2-state 1 new-space space-2 new-score score-2)
                            (%make-d21p2-state 0 space-1 new-space score-1 new-score))
            do (setf (d21p2-state-multiplier child) multiplier)
            do (setf (aref splits i) child)
            finally (return state)))))

(declaim (ftype (function (d21p2-state (unsigned-byte 8)) d21p2-state) d21p2-play))
(defun d21p2-play (state until)
  (declare (type d21p2-state state))
  (declare (type (unsigned-byte 8) until))
  (declare (optimize (speed 3)))
  (with-slots (turn score-1 score-2 wins-1 wins-2 splits) state
    (cond
      ((<= until score-1) (incf wins-1))
      ((<= until score-2) (incf wins-2))
      ((and (< score-1 until) (< score-2 until))
       (d21p2-roll state)
       (dotimes (i 7)
         (let* ((child (aref splits i))
                (multiplier (d21p2-state-multiplier child)))
           (d21p2-play child until)
           (incf wins-1 (logand #xffffffffffffffff (* multiplier (d21p2-state-wins-1 child))))
           (incf wins-2 (logand #xffffffffffffffff (* multiplier (d21p2-state-wins-2 child))))
           (setf (aref splits i) NIL))))))
  state)

(defun d21p2 ()
  (declare (optimize (speed 3)))
  (let ((state (d21p2-data)))
    (d21p2-play state 21)))

;; Answer: 146854918035875
