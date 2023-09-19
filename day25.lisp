(in-package #:aoc-2021)

;; https://adventofcode.com/2021/day/25

(defparameter *day25-input* (local-file #P"day25.txt" :error T))

(defparameter *day25-input-re* (cl-ppcre:create-scanner "[v>.]+"))

(defstruct (d25-floor (:constructor %make-d25-floor (width height eastward southward blockmap tmp)))
  (width 0 :type (unsigned-byte 8))
  (height 0 :type (unsigned-byte 8))
  (eastward NIL :type (simple-array bit (* *)))
  (southward NIL :type (simple-array bit (* *)))
  (blockmap NIL :type (simple-array bit (* *)))
  (tmp NIL :type (simple-array bit (* *))))

(defmethod d25-floor-to-string ((floor d25-floor))
  (declare (optimize (speed 3)))
  (with-slots (width height eastward southward) floor
    (declare (type (unsigned-byte 8) width height))
    (declare (type (simple-array bit (* *)) eastward southward))
    (let ((string (make-string (1- (* height (1+ width)))
                               :element-type 'base-char :initial-element #\Newline)))
      (dotimes (y height string)
        (dotimes (x width)
          (setf (char string (+ x (* y (1+ width))))
                (cond
                  ((= 1 (bit eastward y x)) #\>)
                  ((= 1 (bit southward y x)) #\v)
                  (T #\.))))))))

(defmethod d25-floor-step ((floor d25-floor))
  (declare (optimize (speed 3)))
  (with-slots (width height eastward southward blockmap tmp) floor
    (declare (type (unsigned-byte 8) width height))
    (declare (type (simple-array bit (* *)) eastward southward blockmap tmp))
    (let ((changed-p NIL))
      (bit-xor tmp tmp T) ;; Clear.
      (bit-ior eastward southward blockmap)
      (loop for y from 0 below height
            do (loop for x from 0 below width
                     for to-x = (mod (1+ x) width)
                     do (when (and (= 0 (bit blockmap y to-x))
                                   (= 1 (bit eastward y x)))
                          (setf changed-p T)
                          (setf (bit tmp y to-x) 1)
                          (setf (bit eastward y x) 0))))
      (bit-ior eastward tmp T)
      (bit-ior eastward southward blockmap)
      (bit-xor tmp tmp T)
      (loop for y from 0 below height
            for to-y = (mod (1+ y) height)
            do (loop for x from 0 below width
                     do (when (and (= 0 (bit blockmap to-y x))
                                   (= 1 (bit southward y x)))
                          (setf changed-p T)
                          (setf (bit tmp to-y x) 1)
                          (setf (bit southward y x) 0))))
      (bit-ior southward tmp T)
      (values floor changed-p))))

(defun d25-data ()
  (declare (optimize (speed 3)))
  (with-open-file (stream *day25-input* :if-does-not-exist :error)
    (loop for line of-type input-line = (read-line stream NIL :eof)
          until (eql line :eof)
          for match of-type (simple-array character (*)) =
          (or (cl-ppcre:scan-to-strings *day25-input-re* line)
              (error "Invalid input: ~a" line))
          for width = (length match) then (if (= width (length match))
                                              width (error "Varying line widths."))
          for eastward = (queue-make)
          for southward = (queue-make)
          counting match into height
          do (dotimes (i width)
               (ecase (char match i)
                 (#\>
                  (queue-push 1 eastward)
                  (queue-push 0 southward))
                 (#\v
                  (queue-push 0 eastward)
                  (queue-push 1 southward))
                 (#\.
                  (queue-push 0 eastward)
                  (queue-push 0 southward))))
          collect (queue-as-list eastward) into east-rows
          collect (queue-as-list southward) into south-rows
          finally (return (%make-d25-floor width height
                                           (make-array `(,height ,width)
                                                       :element-type 'bit
                                                       :initial-contents east-rows)
                                           (make-array `(,height ,width)
                                                       :element-type 'bit
                                                       :initial-contents south-rows)
                                           (make-array `(,height ,width) :element-type 'bit)
                                           (make-array `(,height ,width) :element-type 'bit))))))

(defun d25p1 ()
  (declare (optimize (speed 3)))
  (loop with floor = (d25-data)
        for i of-type (unsigned-byte 32) from 1
        while (nth-value 1 (d25-floor-step floor))
        finally (return i)))

;; Answer 337
