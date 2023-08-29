(in-package #:aoc-2021)

;; https://adventofcode.com/2021/day/23

;; Input example:
;; #############
;; #...........#
;; ###B#C#B#D###
;;   #A#D#C#A#
;;   #########

(defparameter *day23-input* (local-file #P"day23.txt" :error T))

(defparameter *day23-input-re* (cl-ppcre:create-scanner "^(##|  )#(\\w)#(\\w)#(\\w)#(\\w)#(##)?$"))

(defparameter *day23-spaces*
  '(:hall-far-west :hall-west :hall-mid-1 :hall-mid-2 :hall-mid-3 :hall-east :hall-far-east
    :room-1-a :room-1-b :room-1-c :room-1-d
    :room-2-a :room-2-b :room-2-c :room-2-d
    :room-3-a :room-3-b :room-3-c :room-3-d
    :room-4-a :room-4-b :room-4-c :coom-4-d))

(defparameter *day23-names* #(:amber :bronze :copper :desert))

(defparameter *day23-costs* #(1 10 100 1000))

(defparameter *day23-rooms* #(2 4 6 8))

(declaim (inline d23-space-coordinate))
(defun d23-space-coordinate (space)
  (declare (type keyword space))
  (declare (optimize (speed 3)))
  (ecase space
    (:hall-far-west  '(0 . 0))
    (:hall-west      '(1 . 0))
    (:hall-mid-1     '(3 . 0))
    (:hall-mid-2     '(5 . 0))
    (:hall-mid-3     '(7 . 0))
    (:hall-east      '(9 . 0))
    (:hall-far-east '(10 . 0))
    (:room-1-a       '(2 . 1))
    (:room-1-b       '(2 . 2))
    (:room-1-c       '(2 . 3))
    (:room-1-d       '(2 . 4))
    (:room-2-a       '(4 . 1))
    (:room-2-b       '(4 . 2))
    (:room-2-c       '(4 . 3))
    (:room-2-d       '(4 . 4))
    (:room-3-a       '(6 . 1))
    (:room-3-b       '(6 . 2))
    (:room-3-c       '(6 . 3))
    (:room-3-d       '(6 . 4))
    (:room-4-a       '(8 . 1))
    (:room-4-b       '(8 . 2))
    (:room-4-c       '(8 . 3))
    (:room-4-d       '(8 . 4))))

(declaim (inline d23-space-from-coordinate))
(defun d23-space-from-coordinate (x y)
  (declare (type (integer 0 10) x))
  (declare (type (integer 0 4) y))
  (declare (optimize (speed 3)))
  (or
   (case x
     (0 (case y (0 :hall-far-west)))
     (1 (case y (0 :hall-west)))
     (2 (case y
          (0 :none)
          (1 :room-1-a)
          (2 :room-1-b)
          (3 :room-1-c)
          (4 :room-1-d)))
     (3 (case y (0 :hall-mid-1)))
     (4 (case y
          (0 :none)
          (1 :room-2-a)
          (2 :room-2-b)
          (3 :room-2-c)
          (4 :room-2-d)))
     (5 (case y (0 :hall-mid-2)))
     (6 (case y
          (0 :none)
          (1 :room-3-a)
          (2 :room-3-b)
          (3 :room-3-c)
          (4 :room-3-d)))
     (7 (case y (0 :hall-mid-3)))
     (8 (case y
          (0 :none)
          (1 :room-4-a)
          (2 :room-4-b)
          (3 :room-4-c)
          (4 :room-4-d)))
     (9 (case y (0 :hall-east)))
     (10 (case y (0 :hall-far-east))))
   :wall))

(defun d23-space-distance (space-a space-b)
  (declare (type keyword space-a space-b))
  (declare (optimize (speed 3)))
  (when (eql space-a space-b) (return-from d23-space-distance 0))
  (let* ((start (d23-space-coordinate space-a))
         (end (d23-space-coordinate space-b))
         (start-x (car start))
         (start-y (cdr start))
         (end-x (car end))
         (end-y (cdr end)))
    (declare (type (integer 0 10) start-x end-x))
    (declare (type (integer 0 4) start-y end-y))
    (+ (abs (- end-x start-x)) start-y end-y)))

(defstruct (d23-burrow (:constructor %make-d23-burrow ()))
  (room-size 2 :type (or (eql 2) (eql 4)))
  (key (make-string 27 :element-type 'base-char :initial-element #\.)
   :type (simple-array base-char (27)))
  (amphipods (make-array 16 :element-type 'keyword :initial-element :none)
   :type (simple-array keyword (16)))
  (spaces (make-array '(5 11) :element-type 'keyword :initial-element :none)
   :type (simple-array keyword (5 11))))

(defmethod d23-burrow-print ((burrow d23-burrow))
  (flet ((amphipod-char (name)
           (ecase name
             (:amber  #\A)
             (:bronze #\B)
             (:copper #\C)
             (:desert #\D)
             (:none   #\.))))
    (let ((spaces (d23-burrow-spaces burrow)))
      (format T "~&#############~%#~c~c.~c.~c.~c.~c~c#~%###~c#~c#~c#~c###~%  #~c#~c#~c#~c#~%"
              (amphipod-char (aref spaces 0 0)) (amphipod-char (aref spaces 0 1))
              (amphipod-char (aref spaces 0 3)) (amphipod-char (aref spaces 0 5))
              (amphipod-char (aref spaces 0 7)) (amphipod-char (aref spaces 0 9))
              (amphipod-char (aref spaces 0 10))
              (amphipod-char (aref spaces 1 2)) (amphipod-char (aref spaces 1 4))
              (amphipod-char (aref spaces 1 6)) (amphipod-char (aref spaces 1 8))
              (amphipod-char (aref spaces 2 2)) (amphipod-char (aref spaces 2 4))
              (amphipod-char (aref spaces 2 6)) (amphipod-char (aref spaces 2 8)))
      (when (< 2 (d23-burrow-room-size burrow))
        (format T "  #~c#~c#~c#~c#~%  #~c#~c#~c#~c#~%"
                (amphipod-char (aref spaces 3 2)) (amphipod-char (aref spaces 3 4))
                (amphipod-char (aref spaces 3 6)) (amphipod-char (aref spaces 3 8))
                (amphipod-char (aref spaces 4 2)) (amphipod-char (aref spaces 4 4))
                (amphipod-char (aref spaces 4 6)) (amphipod-char (aref spaces 4 8))))
      (format T "  #########~%"))))

(defun make-d23-burrow (initial-amphipods &optional (room-size 2))
  (declare (type (simple-array keyword (16)) initial-amphipods))
  (declare (type (or (eql 2) (eql 4)) room-size))
  (declare (optimize (speed 3)))
  (let* ((burrow (%make-d23-burrow))
         (amphipods (d23-burrow-amphipods burrow))
         (spaces (d23-burrow-spaces burrow)))
    (declare (type (simple-array keyword (16)) amphipods))
    (declare (type (simple-array keyword (5 11)) spaces))
    (setf (d23-burrow-room-size burrow) room-size)
    (dotimes (y 5)
      (dotimes (x 11)
        (let ((space (if (or (< room-size y) (eql :wall (d23-space-from-coordinate x y)))
                         :wall :none)))
          (setf (aref spaces y x) space))))
    (dotimes (amphipod 16)
      (let ((space (aref initial-amphipods amphipod)))
        (unless (eql :none space)
          (let ((coord (d23-space-coordinate space))
                (name (svref (the (simple-vector 4) *day23-names*) (floor amphipod 4))))
            (setf (aref spaces (cdr coord) (car coord)) name)))
        (setf (aref amphipods amphipod) space)))
    (d23-burrow-reset-key burrow)))

(defun make-d23-burrow-final (&optional (room-size 2))
  (declare (optimize (speed 3)))
  (make-d23-burrow #(:room-1-a :room-1-b :room-1-c :room-1-d
                     :room-2-a :room-2-b :room-2-c :room-2-d
                     :room-3-a :room-3-b :room-3-c :room-3-d
                     :room-4-a :room-4-b :room-4-c :room-4-d)
                   room-size))

(defmethod d23-burrow-reset-key ((burrow d23-burrow))
  (declare (optimize (speed 3)))
  (let ((key (d23-burrow-key burrow))
        (spaces (d23-burrow-spaces burrow))
        (height (1+ (d23-burrow-room-size burrow)))
        (i 0))
    (declare (type (integer 0 5) height))
    (declare (type (integer 0 28) i))
    (dotimes (y 5 burrow)
      (dotimes (x 11)
        (let ((space (aref spaces y x)))
          (unless (eql :wall space)
            (if (< y height)
                (ecase space
                  (:amber  (setf (char key i) #\A))
                  (:bronze (setf (char key i) #\B))
                  (:copper (setf (char key i) #\C))
                  (:desert (setf (char key i) #\D))
                  (:none   (setf (char key i) #\.)))
                (setf (char key i) #\.))
            (incf i)))))))

(defmethod d23-burrow-copy ((burrow d23-burrow))
  (declare (optimize (speed 3)))
  (let ((copy (%make-d23-burrow)))
    (setf (d23-burrow-room-size copy) (d23-burrow-room-size burrow))
    (dotimes (i 16)
      (setf (aref (d23-burrow-amphipods copy) i) (aref (d23-burrow-amphipods burrow) i)))
    (dotimes (y 5 copy)
      (dotimes (x 11)
        (setf (aref (d23-burrow-spaces copy) y x) (aref (d23-burrow-spaces burrow) y x))))))

(defmethod d23-burrow-moves-from ((burrow d23-burrow) from output)
  (declare (type keyword from))
  (declare (type (simple-array keyword (7)) output))
  (declare (optimize (speed 3)))
  (when (eql :none from) (return-from d23-burrow-moves-from 0))
  (let ((spaces (d23-burrow-spaces burrow)))
    (declare (type (simple-array keyword (5 11)) spaces))
    (dotimes (i 7) (setf (aref output i) :none)) ;; Initialise.
    (let* ((coord (the cons (d23-space-coordinate from)))
           (from-x (car coord))
           (from-y (cdr coord))
           (name (aref spaces from-y from-x))
           (room-x (ecase name
                     (:amber 2)
                     (:bronze 4)
                     (:copper 6)
                     (:desert 8)))
           (blocked-p T)
           (count 0)
           (room-size (d23-burrow-room-size burrow))
           (can-goto-room-p (loop for y from 0 upto room-size
                                  for valid-p = (or (eql :none (aref spaces y room-x))
                                                    (eql name (aref spaces y room-x)))
                                  while valid-p
                                  finally (return valid-p))))
      (declare (type (integer 0 10) from-x))
      (declare (type (integer 0 4) from-y))
      (declare (type (or (eql 2) (eql 4)) room-size))
      (when (and (< 0 from-y) ;; Quick checks.
                 (or (not (eql :none (aref spaces (1- from-y) from-x)))
                     (and (= from-x room-x) can-goto-room-p)))
        (return-from d23-burrow-moves-from 0))
      (loop for x from (1- from-x) downto 0
            while (eql :none (aref spaces 0 x))
            when (= x room-x) do (setf blocked-p NIL)
            do (when (< 0 from-y)
                 (let ((space-name (d23-space-from-coordinate x 0)))
                   (unless (eql :none space-name)
                     (setf (aref output count) space-name)
                     (incf count)))))
      (loop for x from (1+ from-x) upto 10
            while (eql :none (aref spaces 0 x))
            when (= x room-x) do (setf blocked-p NIL)
            do (when (< 0 from-y)
                 (let ((space-name (d23-space-from-coordinate x 0)))
                   (unless (eql :none space-name)
                     (setf (aref output count) space-name)
                     (incf count)))))
      (when (and (not blocked-p) can-goto-room-p)
        (setf (aref output count)
              (dotimes (i room-size (error "No space in room."))
                (declare (type (integer 0 4) i))
                (let ((y (- room-size i)))
                  (when (eql :none (aref spaces y room-x))
                    (return (d23-space-from-coordinate room-x y))))))
        (incf count))
      count)))

(defmethod d23-burrow-move ((burrow d23-burrow) from to)
  (declare (optimize (speed 3)))
  (declare (type keyword from to))
  (let* ((spaces (d23-burrow-spaces burrow))
         (room-size (d23-burrow-room-size burrow))
         (amphipods (d23-burrow-amphipods burrow))
         (from-coord (d23-space-coordinate from))
         (from-x (car from-coord))
         (from-y (cdr from-coord))
         (to-coord (d23-space-coordinate to))
         (to-x (car to-coord))
         (to-y (cdr to-coord))
         (name (aref spaces from-y from-x))
         (amphipod (ecase name
                     (:amber 0)
                     (:bronze 4)
                     (:copper 8)
                     (:desert 12)
                     (:none (error "Space is empty at ~a" to)))))
    (unless (eql :none (aref spaces to-y to-x))
      (error "Space is full at ~a" to))
    (dotimes (i room-size (error "Amphipod ~a is not at ~a" name from))
      (declare (type (integer 0 4) i))
      (when (eql from (aref amphipods amphipod))
        (return amphipod))
      (incf amphipod))
    (setf (aref amphipods amphipod) to)
    (setf (aref spaces to-y to-x) name)
    (setf (aref spaces from-y from-x) :none)
    (d23-burrow-reset-key burrow)
    (* (the (integer 0 1000) (svref (the (simple-vector 4) *day23-costs*) (floor amphipod 4)))
       (the (integer 0 12) (d23-space-distance from to)))))

(defstruct (d23-queue (:constructor %make-d23-queue ()))
  (count 0 :type (unsigned-byte 32))
  (size #x100 :type (unsigned-byte 32))
  (start 0 :type (unsigned-byte 32))
  (array (make-array #x100) :type (simple-array T (*))))

(defmethod d23-queue-expand ((queue d23-queue))
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

(defmethod d23-queue-push ((queue d23-queue) value)
  (declare (optimize (speed 3)))
  (d23-queue-expand queue)
  (with-slots (count size start array) queue
    (declare (type (unsigned-byte 32) count size start))
    (declare (type (simple-array T (*)) array))
    (setf (aref array (mod (+ start count) size)) value)
    (setf count (1+ count))
    queue))

(defmethod d23-queue-pop ((queue d23-queue))
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

(defmethod d23-astar ((root d23-burrow) &optional debug-p)
  (declare (optimize (speed 3)))
  (let ((moves (make-array 7 :element-type 'keyword :initial-element :none)))
    (flet ((weight (astar from to)
             (declare (type astar astar))
             (declare (type d23-burrow from to))
             (declare (ignore astar))
             (declare (optimize (speed 3)))
             (let ((name :none)
                   (cost 0)
                   (from-space :none)
                   (to-space :none)
                   (from-amphipods (d23-burrow-amphipods from))
                   (to-amphipods (d23-burrow-amphipods to)))
               (declare (type (integer 0 1000) cost))
               (dotimes (amphipod 16)
                 (let ((cur-name (svref (the (simple-vector 4) *day23-names*) (floor amphipod 4)))
                       (cur-cost (svref (the (simple-vector 4) *day23-costs*) (floor amphipod 4))))
                   (unless (eql (aref from-amphipods amphipod) (aref to-amphipods amphipod))
                     (unless (eql :none name)
                       (error "Multiple moves between states ~a and ~a" from to))
                     (setf name cur-name)
                     (setf cost cur-cost)
                     (setf from-space (aref from-amphipods amphipod))
                     (setf to-space (aref to-amphipods amphipod)))))
               (* (the (unsigned-byte 32) (d23-space-distance from-space to-space)) cost)))
           (heuristic (astar burrow)
             (declare (type astar astar))
             (declare (type d23-burrow burrow))
             (declare (ignore astar))
             (declare (optimize (speed 3)))
             (loop with spaces = (d23-burrow-spaces burrow)
                   with amphipods = (d23-burrow-amphipods burrow)
                   with room-size = (d23-burrow-room-size burrow)
                   with total-cost of-type (unsigned-byte 32) = 0
                   for amphipod from 0 below 16
                   for name = (svref (the (simple-vector 4) *day23-names*) (floor amphipod 4))
                   for cost of-type (integer 0 1000) = (svref (the (simple-vector 4) *day23-costs*)
                                                              (floor amphipod 4))
                   for room-x of-type (integer 0 10) = (svref (the (simple-vector 4) *day23-rooms*)
                                                              (floor amphipod 4))
                   for space = (aref amphipods amphipod)
                   do (unless (eql :none space)
                        (let* ((coord (the cons (d23-space-coordinate space)))
                               (x (the (integer 0 10) (car coord)))
                               (y (the (integer 0 4) (cdr coord))))
                          (unless (and (= x room-x) ;; Is already valid?
                                       (loop for y0 from y upto room-size
                                             for valid-p = (eql name (aref spaces y0 x))
                                             while valid-p
                                             finally (return valid-p)))
                            ;; Just adding a 1 for going down into a room isn't great but it works.
                            (let ((room-dist (if (= x room-x) 2 (abs (- room-x x)))))
                              (setf total-cost (+ total-cost (* cost (+ room-dist y 1))))))))
                   finally (return total-cost)))
           (neighbors (astar burrow)
             (declare (type astar astar))
             (declare (type d23-burrow burrow))
             (declare (ignore astar))
             (declare (optimize (speed 3)))
             (loop with amphipods = (d23-burrow-amphipods burrow)
                   for amphipod from 0 below 16
                   for from-space = (aref amphipods amphipod)
                   for move-count of-type (integer 0 7) =
                   (d23-burrow-moves-from burrow from-space moves)
                   nconc (loop for i from 0 below move-count
                               for to-space = (aref moves i)
                               for copy = (d23-burrow-copy burrow)
                               do (d23-burrow-move copy from-space to-space)
                               collect copy))))
      (let ((astar (make-astar :start root
                               :goal (make-d23-burrow-final (d23-burrow-room-size root))
                               :key-f #'d23-burrow-key
                               :weight-f #'weight
                               :heuristic-f #'heuristic
                               :neighbors-f #'neighbors)))
        (loop with steps = (astar-find astar)
              with total-cost of-type (unsigned-byte 32) = 0
              for prev = (car steps) then current
              for current in (rest steps)
              do (when debug-p
                   (d23-burrow-print prev)
                   (format T "~&~%")
                   (force-output T))
              do (setf total-cost (+ total-cost (weight astar prev current)))
              finally (return (prog1 (when steps total-cost)
                                (when (and prev debug-p) (d23-burrow-print prev)))))))))

(defun d23-data (&optional (room-size 2))
  (declare (optimize (speed 3)))
  (with-open-file (stream *day23-input* :if-does-not-exist :error)
    (flet ((amphipod-index (str)
             (declare (type (simple-array character (1)) str))
             (ecase (char str 0)
               (#\A 0)
               (#\B 4)
               (#\C 8)
               (#\D 12))))
      (let ((amphipods (make-array 16 :element-type 'keyword :initial-element :none))
            (top-p T))
        (declare (type boolean top-p))
        (dolist (expected '("#############" "#...........#" :regex :regex "  #########"))
          (let ((line (read-line stream)))
            (declare (type (simple-array character (*)) line))
            (unless (or (eql :regex expected) (string= line expected))
              (error "Invalid input \"~a\" when expected \"~a\"" line expected))
            (when (eql :regex expected)
              (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings *day23-input-re* line)
                (declare (type (simple-array (simple-array character (*)) (*)) groups))
                (unless match (error "Invalid input: ~a" line))
                (let ((index (amphipod-index (aref groups 1))))
                  (unless (eql :none (aref amphipods index)) (incf index))
                  (setf (aref amphipods index) (if top-p :room-1-a :room-1-b)))
                (let ((index (amphipod-index (aref groups 2))))
                  (unless (eql :none (aref amphipods index)) (incf index))
                  (setf (aref amphipods index) (if top-p :room-2-a :room-2-b)))
                (let ((index (amphipod-index (aref groups 3))))
                  (unless (eql :none (aref amphipods index)) (incf index))
                  (setf (aref amphipods index) (if top-p :room-3-a :room-3-b)))
                (let ((index (amphipod-index (aref groups 4))))
                  (unless (eql :none (aref amphipods index)) (incf index))
                  (setf (aref amphipods index) (if top-p :room-4-a :room-4-b)))
                (setf top-p NIL)))))
        (let ((line (read-line stream NIL :eof)))
          (unless (eql :eof line)
            (error "Excess content: ~a" line)))
        (ecase room-size
          (2) ;; All good.
          (4
           (dotimes (i 16) ;; First push B spaces to D.
             (let ((room (aref amphipods i)))
               (setf (aref amphipods i) (case room
                                          (:room-1-b :room-1-d)
                                          (:room-2-b :room-2-d)
                                          (:room-3-b :room-3-d)
                                          (:room-4-b :room-4-d)
                                          (T room)))))
           ;; Next set the extra lines.
           ;;   #D#C#B#A#
           ;;   #D#B#A#C#
           ;; Amber
           (setf (aref amphipods 2) :room-4-b)
           (setf (aref amphipods 3) :room-3-c)
           ;; Bronze
           (setf (aref amphipods 6) :room-3-b)
           (setf (aref amphipods 7) :room-2-c)
           ;; Copper
           (setf (aref amphipods 10) :room-2-b)
           (setf (aref amphipods 11) :room-4-c)
           ;; Desert
           (setf (aref amphipods 14) :room-1-b)
           (setf (aref amphipods 15) :room-1-c)))
        (make-d23-burrow amphipods room-size)))))

(defun d23p1 (&optional debug-p)
  (declare (optimize (speed 3)))
  (let ((burrow (d23-data)))
    (d23-astar burrow debug-p)))

;; Answer: 11516

(defun d23p2 (&optional debug-p)
  (declare (optimize (speed 3)))
  (let ((burrow (d23-data 4)))
    (d23-astar burrow debug-p)))

;; Answer: 40272
