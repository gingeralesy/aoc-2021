#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defparameter *clean-re* (cl-ppcre:create-scanner "^(.*\\S)?\\s*$"))

(defun local-file (filename &key error)
  (let ((file (asdf:system-relative-pathname :aoc-2021 filename)))
    (when (and error (not (probe-file file)))
      (error "Missing file: ~a" filename))
    file))

(defun clean (line)
  (if (stringp line)
      (multiple-value-bind (match groups)
          (cl-ppcre:scan-to-strings *clean-re* line)
        (or (when match (aref groups 0)) ""))
      line))

(declaim (inline u32))
(defun u32 (value)
  (declare (type (unsigned-byte 64) value))
  (logand #xffffffff value))

(declaim (inline queue-make))
(defun queue-make ()
  (cons NIL NIL)) ;; (HEAD . TAIL)

(declaim (inline queue-length))
(defun queue-length (queue)
  (declare (optimize (speed 3)))
  (length (the list (car queue))))

(declaim (inline queue-empty-p))
(defun queue-empty-p (queue)
  (declare (optimize (speed 3)))
  (null (the list (car queue))))

(defun queue-push (obj queue)
  (declare (type list queue))
  (declare (optimize (speed 3)))
  (if (cdr queue)
      (setf (cddr queue) (cons obj NIL)
            (cdr queue) (cddr queue))
      (setf (cdr queue) (cons obj NIL)
            (car queue) (cdr queue)))
  queue)

(defun pqueue-push (obj queue &optional (key #'identity) (test #'<))
  "Use this to ensure ordering."
  (cond
    ((queue-empty-p queue)
     (queue-push obj queue))
    ((funcall test (funcall key obj) (funcall key (caar queue)))
     (let ((new (cons obj (car queue))))
       (setf (car queue) new)))
    (T
     (loop with obj-key = (funcall key obj)
           for prev = (car queue) then next
           for next = (cdr prev)
           for prev-key = (and prev (funcall key (car prev))) then next-key
           for next-key = (and next (funcall key (car next)))
           until (or (null next) (and (funcall test prev-key obj-key)
                                      (funcall test obj-key next-key)))
           finally (let ((new (cons obj next)))
                     (setf (cdr prev) new)
                     (when (null next)
                       (setf (cdr queue) new))))))
  queue)

(defun queue-pop (queue)
  (declare (type list queue))
  (declare (optimize (speed 3)))
  (when (car queue)
    (let ((obj (caar queue)))
      (setf (car queue) (cdar queue))
      (unless (car queue)
        (setf (cdr queue) NIL))
      obj)))

(defun pqueue-pop (queue &optional (key #'identity) (test #'<))
  "Use this if you need ordering but can't trust the queue."
  (when (car queue)
    (loop with min-prev = NIL
          with min = (car queue)
          for cur = (car queue) then next
          for next = (cdr cur) then (cdr next)
          while next
          do (when (funcall test (funcall key (car next)) (funcall key (car min)))
               (setf min-prev cur)
               (setf min next))
          finally (progn
                    (if min-prev
                        (setf (cdr min-prev) (cdr min))
                        (setf (car queue) (cdr min)))
                    (unless (cdr min)
                      (setf (cdr queue) min-prev))
                    (return (car min))))))

(declaim (inline queue-as-list))
(defun queue-as-list (queue &optional copy)
  (declare (optimize (speed 3)))
  (if copy (copy-list (the list (car queue))) (the list (car queue))))

(defun queue-copy (queue)
  (declare (type list queue))
  (declare (optimize (speed 3)))
  (let ((copy (queue-make)))
    (loop for item in (queue-as-list queue)
          do (queue-push item copy)
          finally (return copy))))

(defun queue-find (item queue &optional (key #'identity) (test #'eql))
  (declare (type list queue))
  (declare (type function key test))
  (declare (optimize (speed 3)))
  (find item (the list (car queue)) :key key :test test))

(defstruct (aqueue (:constructor %make-aqueue ()))
  (count 0 :type (unsigned-byte 32))
  (size #x100 :type (unsigned-byte 32))
  (start 0 :type (unsigned-byte 32))
  (array (make-array #x100) :type (simple-array T (*))))

(defmethod aqueue-expand ((queue aqueue))
  (declare (optimize (speed 3)))
  (with-slots (count size start array) queue
    (declare (type (unsigned-byte 32) count size start))
    (declare (type (simple-array T (*)) array))
    (unless (< count (1- size))
      (let ((new-array (make-array (* 2 size))))
        (declare (type (simple-array T (*)) new-array))
        (dotimes (i count)
          (setf (aref new-array i) (aref array (mod (+ i start) size))))
        (setf start 0)
        (setf size (* 2 size))
        (setf array new-array)
        queue))))

(defmethod aqueue-push ((queue aqueue) value)
  (declare (optimize (speed 3)))
  (aqueue-expand queue)
  (with-slots (count size start array) queue
    (declare (type (unsigned-byte 32) count size start))
    (declare (type (simple-array T (*)) array))
    (setf (aref array (mod (+ start count) size)) value)
    (setf count (1+ count))
    queue))

(defmethod aqueue-pop ((queue aqueue))
  (declare (optimize (speed 3)))
  (with-slots (count size start array) queue
    (declare (type (unsigned-byte 32) count size start))
    (declare (type (simple-array T (*)) array))
    (when (< 0 count)
      (setf count (1- count))
      (let ((value (aref array start)))
        (setf (aref array start) NIL)
        (setf start (mod (1+ start) size))
        value))))

(defmethod aqueue-clear ((queue aqueue))
  (declare (optimize (speed 3)))
  (with-slots (count size start array) queue
    (declare (type (unsigned-byte 32) count size start))
    (declare (type (simple-array T (*)) array))
    (when (< 0 count)
      (dotimes (i size)
        (setf (aref array i) NIL)))
    (setf start 0)
    (setf count 0)
    queue))

(defmethod aqueue-empty-p ((queue aqueue))
  (declare (optimize (speed 3)))
  (= 0 (aqueue-count queue)))

(declaim (inline btree-parent btree-left-child btree-right-child))

(defun btree-parent (index)
  (floor (1- index) 2))

(defun btree-left-child (index)
  (1+ (* 2 index)))

(defun btree-right-child (index)
  (+ 2 (* 2 index)))

(defstruct (heap-node (:constructor %make-heap-node (priority value)))
  (priority #xffffffff :type (unsigned-byte 32))
  (value NIL :type T))

(defstruct (pheap (:constructor %make-pheap (&key (allow-duplicates-p NIL) (compare-f #'<)
                                                  (key-f #'identity))))
  (allow-duplicates-p NIL :type boolean)
  (count 0 :type (unsigned-byte 32))
  (size #x100 :type (unsigned-byte 32))
  (root NIL :type (or null heap-node))
  (array (make-array #x100 :initial-element NIL) :type (simple-array T (*)))
  (values (make-hash-table :test 'equal) :type hash-table)
  (compare-f #'<)
  (key-f #'identity))

(defmethod pheap-expand ((heap pheap))
  (declare (optimize (speed 3)))
  (with-slots (array count size) heap
    (declare (type (simple-array T (*)) array))
    (declare (type (unsigned-byte 32) count size))
    (unless (< count size)
      (setf size (* 2 size))
      (let ((new-array (make-array size :initial-element NIL)))
        (dotimes (i count)
          (setf (aref new-array i) (aref array i)))
        (setf array new-array)))))

(defmethod pheap-swap ((heap pheap) i j)
  (declare (optimize (speed 3)))
  (with-slots (array values key-f) heap
    (declare (type (simple-array T (*)) array))
    (declare (type function key-f))
    (let ((tmp (aref array i))
          (other (aref array j)))
      (when (and tmp (heap-node-value tmp))
        (setf (gethash (funcall key-f (heap-node-value tmp)) values) j))
      (when (and other (heap-node-value other))
        (setf (gethash (funcall key-f (heap-node-value other)) values) i))
      (setf (aref array i) other)
      (setf (aref array j) tmp))))

(defmethod pheap-shift-up ((heap pheap) index)
  (declare (optimize (speed 3)))
  (with-slots (array compare-f) heap
    (declare (type (simple-array T (*)) array))
    (declare (type function compare-f))
    (loop with i of-type (unsigned-byte 32) = index
          while (< 0 i)
          for parent-i = (btree-parent i)
          for node = (aref array i)
          for parent = (aref array parent-i)
          while (funcall compare-f (heap-node-priority node) (heap-node-priority parent))
          do (pheap-swap heap parent-i i)
          do (setf i parent-i))))

(defmethod pheap-shift-down ((heap pheap) index)
  (declare (type (unsigned-byte 32) index))
  (declare (optimize (speed 3)))
  (with-slots (array count compare-f) heap
    (declare (type (simple-array T (*)) array))
    (declare (type (unsigned-byte 32) count))
    (declare (type function compare-f))
    (flet ((compare (i j)
             (let ((a (aref array i))
                   (b (aref array j)))
               (funcall compare-f (heap-node-priority a) (heap-node-priority b)))))
      (let ((max-index index)
            (left-i (btree-left-child index))
            (right-i (btree-right-child index)))
        (when (and (<= left-i count) (compare left-i max-index))
          (setf max-index left-i))
        (when (and (<= right-i count) (compare right-i max-index))
          (setf max-index right-i))
        (unless (= index max-index)
          (pheap-swap heap index max-index)
          (pheap-shift-down heap max-index))))))

(defmethod pheap-change-priority ((heap pheap) index priority)
  (declare (type (unsigned-byte 32) index priority))
  (declare (optimize (speed 3)))
  (with-slots (array) heap
    (declare (type (simple-array T (*)) array))
    (let* ((node (aref array index))
           (old-priority (heap-node-priority node)))
      (setf (heap-node-priority node) priority)
      (if (< priority old-priority)
          (pheap-shift-up heap index)
          (pheap-shift-down heap index)))))

(defmethod pheap-push ((heap pheap) value &optional priority)
  (declare (optimize (speed 3)))
  (with-slots (array count values key-f allow-duplicates-p) heap
    (declare (type (simple-array T (*)) array))
    (declare (type (unsigned-byte 32) count))
    (declare (type function key-f))
    (let* ((key (funcall key-f value))
           (old-index (gethash key values)))
      (cond
        ((and (not allow-duplicates-p) old-index)
         (pheap-change-priority heap old-index (or priority value)))
        (T
         (pheap-expand heap)
         (let ((node (aref array count)))
           (cond
             (node
              (setf (heap-node-priority node) (or priority value))
              (setf (heap-node-value node) value))
             (T
              (setf (aref array count) (%make-heap-node (or priority value) value))
              (setf (gethash key values) count))))
         (pheap-shift-up heap count)
         (setf count (1+ count)))))))

(defmethod pheap-pop ((heap pheap))
  (declare (optimize (speed 3)))
  (with-slots (array count values key-f compare-f) heap
    (declare (type (simple-array T (*)) array))
    (declare (type (unsigned-byte 32) count))
    (declare (type function key-f compare-f))
    (let* ((root (aref array 0))
           (result (heap-node-value root)))
      (setf (heap-node-value root) NIL)
      (setf (heap-node-priority root)
            (if (funcall compare-f 0 (heap-node-priority root))
                #xffffffff 0))
      (remhash (funcall key-f result) values)
      (pheap-swap heap 0 (1- count))
      (setf count (1- count))
      (pheap-shift-down heap 0)
      result)))

(defmethod pheap-empty-p ((heap pheap))
  (declare (optimize (speed 3)))
  (not (< 0 (pheap-count heap))))

;; See https://en.wikipedia.org/wiki/A*_search_algorithm

(defstruct astar
  (start (cons 0 0) :type T)
  (goal (cons 0 0) :type T)
  (key-f #'identity :type (function (T) T))
  (weight-f #'(lambda (astar from to)
                (declare (type astar astar))
                (declare (type (cons (signed-byte 32) (signed-byte 32)) from to))
                (declare (ignore astar))
                (+ (abs (- (car to) (car from))) (abs (- (cdr to) (cdr from)))))
   :type (function (astar T T) (unsigned-byte 32)))
  (heuristic-f #'(lambda (astar from)
                   (declare (type astar astar))
                   (declare (type (cons (signed-byte 32) (signed-byte 32)) from))
                   (let ((goal (astar-goal astar)))
                     (+ (abs (- (car goal) (car from)))
                        (abs (- (cdr goal) (cdr from))))))
   :type (function (astar T) (unsigned-byte 32)))
  (neighbors-f #'(lambda (astar from)
                   (declare (type astar astar))
                   (declare (type (cons (signed-byte 32) (signed-byte 32)) from))
                   (declare (ignore astar))
                   (list (cons (1- (car from)) (cdr from)) (cons (car from) (1- (cdr from)))
                         (cons (1+ (car from)) (cdr from)) (cons (car from) (1+ (cdr from)))))
   :type (function (astar T) list)))

(defmethod astar-find ((astar astar))
  (declare (optimize (speed 3)))
  (with-slots (start goal key-f weight-f heuristic-f neighbors-f) astar
    (declare (type function key-f weight-f heuristic-f neighbors-f))
    (let ((open-set (%make-pheap :key-f key-f))
          (came-from (make-hash-table :test 'equal))
          (g-score (make-hash-table :test 'equal))
          (f-score (make-hash-table :test 'equal))
          (goal-key (funcall key-f goal)))
      (let ((f-start (funcall heuristic-f astar start))
            (key-start (funcall key-f start)))
        (pheap-push open-set start f-start)
        (setf (gethash key-start g-score) 0)
        (setf (gethash key-start f-score) f-start))
      (loop until (pheap-empty-p open-set)
            for current = (pheap-pop open-set)
            for current-key = (funcall key-f current)
            for g-current of-type (unsigned-byte 32) = (gethash current-key g-score)
            when (equal current-key goal-key)
            do (return-from astar-find
                 (loop with path = ()
                       for node = current then prev
                       for key = (funcall key-f node)
                       for prev = (gethash key came-from)
                       do (push node path)
                       while prev
                       finally (return path)))
            do (loop for neighbor in (funcall neighbors-f astar current)
                     for neighbor-key = (funcall key-f neighbor)
                     for g-neighbor of-type (unsigned-byte 32) =
                     (+ g-current
                        (the (unsigned-byte 32)
                             (funcall weight-f astar current neighbor)))
                     for f-neighbor = (+ g-neighbor (the (unsigned-byte 32)
                                                         (funcall heuristic-f astar neighbor)))
                     do (when (and (< g-neighbor
                                      (the (unsigned-byte 64)
                                           (gethash neighbor-key g-score #x100000000)))
                                   (<= (the (unsigned-byte 32) f-neighbor)
                                       (the (unsigned-byte 64)
                                            (gethash neighbor-key f-score #x100000000))))
                          (setf (gethash neighbor-key came-from) current)
                          (setf (gethash neighbor-key g-score) g-neighbor)
                          (setf (gethash neighbor-key f-score) f-neighbor)
                          (pheap-push open-set neighbor f-neighbor)))))))
