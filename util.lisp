#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defun local-file (filename &key error)
  (let ((file (asdf:system-relative-pathname :aoc-2021 filename)))
    (when (and error (not (probe-file file)))
      (error "Missing file: ~a" filename))
    file))

(defun queue-make ()
  (cons NIL NIL)) ;; (HEAD . TAIL)

(defun queue-length (queue)
  (length (car queue)))

(defun queue-empty-p (queue)
  (null (car queue)))

(defun queue-push (obj queue)
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

(defun queue-as-list (queue &optional copy)
  (if copy (copy-list (car queue)) (car queue)))

(defun queue-copy (queue)
  (let ((copy (queue-make)))
    (loop for item in (queue-as-list queue)
          do (queue-push item copy)
          finally (return copy))))

(defun queue-find (item queue &optional (key #'identity) (test #'eql))
  (find item (car queue) :key key :test test))
