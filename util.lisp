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
