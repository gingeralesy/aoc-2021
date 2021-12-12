#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defparameter *day12-input* (local-file #P"day12.txt" :error T))

(defun d12-build-map (paths)
  "Goes through the found paths and turns them into a map."
  (loop with map = NIL ;; Property list. Hash-tables are overkill here.
        for path in paths
        do (push (cdr path) (getf map (car path)))
        do (push (car path) (getf map (cdr path)))
        finally (return map)))

(defun d12-data ()
  "Parses all the pathways and caverns into a map and a cavern size list."
  (with-open-file (stream *day12-input* :if-does-not-exist :error)
    (loop with scanner = (cl-ppcre:create-scanner "^(\\w+)-(\\w+)$")
          with paths = NIL
          with caves = NIL
          for line = (read-line stream NIL)
          while line
          do (cl-ppcre:register-groups-bind (from to)
                 (scanner line)
               (let* ((from-up (string-upcase from))
                      (to-up (string-upcase to))
                      (from-key (intern from-up :keyword))
                      (to-key (intern to-up :keyword)))
                 (push (cons from-key to-key) paths)
                 (unless (getf caves from-key)
                   (setf (getf caves from-key) (string= from-up from)))
                 (unless (getf caves to-key)
                   (setf (getf caves to-key) (string= to-up to)))))
          finally (return (values (d12-build-map paths) caves)))))


#|
--- Day 12: Passage Pathing ---

With your submarine's subterranean subsystems subsisting suboptimally, the only way you're getting
out of this cave anytime soon is by finding a path yourself. Not just a path - the only way to know
if you've found the best path is to find all of them.

Fortunately, the sensors are still mostly working, and so you build a rough map of the remaining
caves (your puzzle input). For example:

start-A
start-b
A-c
A-b
b-d
A-end
b-end

This is a list of how all of the caves are connected. You start in the cave named start, and your
destination is the cave named end. An entry like b-d means that cave b is connected to cave d - that
is, you can move between them.

So, the above cave system looks roughly like this:

    start
    /   \
c--A-----b--d
    \   /
     end

Your goal is to find the number of distinct paths that start at start, end at end, and don't visit
small caves more than once. There are two types of caves: big caves (written in uppercase, like A)
and small caves (written in lowercase, like b). It would be a waste of time to visit any small cave
more than once, but big caves are large enough that it might be worth visiting them multiple times.
So, all paths you find should visit small caves at most once, and can visit big caves any number of
times.

Given these rules, there are 10 paths through this example cave system:

start,A,b,A,c,A,end
start,A,b,A,end
start,A,b,end
start,A,c,A,b,A,end
start,A,c,A,b,end
start,A,c,A,end
start,A,end
start,b,A,c,A,end
start,b,A,end
start,b,end

(Each line in the above list corresponds to a single path; the caves visited by that path are listed
in the order they are visited and separated by commas.)

Note that in this cave system, cave d is never visited by any path: to do so, cave b would need to
be visited twice (once on the way to cave d and a second time when returning from cave d), and since
cave b is small, this is not allowed.

Here is a slightly larger example:

dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc

The 19 paths through it are as follows:

start,HN,dc,HN,end
start,HN,dc,HN,kj,HN,end
start,HN,dc,end
start,HN,dc,kj,HN,end
start,HN,end
start,HN,kj,HN,dc,HN,end
start,HN,kj,HN,dc,end
start,HN,kj,HN,end
start,HN,kj,dc,HN,end
start,HN,kj,dc,end
start,dc,HN,end
start,dc,HN,kj,HN,end
start,dc,end
start,dc,kj,HN,end
start,kj,HN,dc,HN,end
start,kj,HN,dc,end
start,kj,HN,end
start,kj,dc,HN,end
start,kj,dc,end

Finally, this even larger example has 226 paths through it:

fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW

How many paths through this cave system are there that visit small caves at most once?
|#

(defun d12p1 ()
  (multiple-value-bind (paths caves)
      (d12-data)
    (let ((routes))
      (labels ((find-routes (from visited)
                 (queue-push from visited)
                 (if (eql :end from)
                     (push (queue-as-list (queue-copy visited)) routes)
                     (loop for cave in (getf paths from)
                           when (or (not (queue-find cave visited)) (getf caves cave))
                           do (find-routes cave (queue-copy visited))))))
        (find-routes :start (queue-make))
        (length routes)))))

;; Answer: 4495
