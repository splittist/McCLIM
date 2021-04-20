;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2006 by Andy Hefner <ahefner@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; 3D Logic Cube flash game (http://www.newgrounds.com/portal/view/315702),
;;; translated into CL/McCLIM.
;;;

(in-package #:clim-demo)

;; TODO: Improved puzzle generator. The puzzles currently generated by
;; "Random Puzzle" are all extremely easy to solve. I'm not admitting
;; defeat here, but I refuse to waste any more time on this program.

;; FIXME: When shrinking polygons during the victory animation, why
;; does their shape appear to distort? Look at the Z of the transformed
;; coordinates..

;; Pane definition and puzzle generator

(defclass logic-cube-pane (clime:never-repaint-background-mixin basic-gadget)
  ((%background :initform (make-rgb-color 0.35 0.35 0.46) :reader background-color)
   (pitch :initform 0.0 :accessor pitch)
   (yaw   :initform 0.0 :accessor yaw)
   (density :initform 5 :accessor density)
   (playfield :reader playfield)
   (drag-color :initform nil :accessor drag-color)
   (dragging :initform nil :accessor dragging)
   (squeeze :initform nil :accessor squeeze) ; For victory animation
   (flyaway :initform 0.0 :accessor flyaway) ; For victory animation
   (decorator :initform nil :accessor decorator))) ; Hook for victory text

(defun reset-logic-cube (cube new-density)
  (with-slots (density playfield squeeze drag-color dragging flyaway decorator) cube
    (setf density new-density
          dragging nil
          decorator nil
          drag-color nil
          flyaway 0.0
          squeeze nil
          playfield (make-array (list 3 density density) :initial-element (list nil nil)))))

;; Playfield squares are a pair of color and {nil, t, terminal}

(defun scrub-square (square) (if (second square) square (list nil nil)))

(defun cleanup-cube (cube)
  (apply-to-hemicube-faces (density cube)
    (lambda (side i j &rest points)
      (declare (ignore points))
        (symbol-macrolet ((square (aref (playfield cube) side i j)))
          (setf square (scrub-square square))))))

(defparameter *logic-cube-colors* (list +red+ +yellow+ +blue+ +green+ +orange+ +purple+))

; Produce crappy, trivial puzzles, slowly.
(defun generate-cube-puzzle (cube &optional (num-colors 6))
  (reset-logic-cube cube 5)
  (labels ((sq (s i j) (first (aref (playfield cube) s i j)))
           (sql (indices) (apply #'sq indices))
           (set-playfield (indices square)
             (destructuring-bind (s i j) indices
               (setf (aref (playfield cube) s i j) square)))
           (satisfying (pred)
             (loop for tries from 0 by 1
                   as s = (random 3)
                   as i = (random (density cube))
                   as j = (random (density cube))
                   as result = (funcall pred (sq s i j) s i j)
                   while (< tries (expt (density cube) 4)) ; ^_^
                   when result return result)))
    (let ((iterators
           (loop for color-index from 0 below num-colors collect
             (destructuring-bind (root iterator)
                 (satisfying
                  (lambda (color &rest root-indices)
                    (let ((our-color (elt *logic-cube-colors* color-index))
                          (current-head root-indices))
                      (when (null color)
                        (labels
                            ((find-new-head ()
                               (satisfying  ;; Obviously I should not use 'satisfying' here..
                                (lambda (head-color &rest head-indices)
                                  (and (null head-color) ;; .. but computers are very fast.
                                       (not (equal head-indices current-head))
                                       (member head-indices (apply #'adjacent-squares cube current-head) :test #'equal)
                                       (>= 1 (count-if (lambda (c) (eql c our-color))
                                                       (apply #'adjacent-squares cube head-indices)
                                                       :key #'sql))
                                       head-indices))))
                             (choose-new-head ()
                               (let ((new-head (find-new-head)))
                                 (if new-head
                                     (set-playfield new-head (list our-color nil))
                                     (unless (equal current-head root-indices)
                                       (set-playfield current-head (list our-color 'terminal))))
                                 (setf current-head new-head)
                                 (and new-head #'choose-new-head))))
                          (choose-new-head)
                          (and current-head (list root-indices #'choose-new-head)))))))
               (set-playfield root (list (elt *logic-cube-colors* color-index) 'terminal))
               iterator))))
      (loop for i from 0 by 1
            while (and iterators (< i 100))
            do (setf iterators (remove nil (mapcar #'funcall iterators))))
      (apply-to-hemicube-faces (density cube)
        (lambda (side i j &rest points)
          (declare (ignore points))
          (when (and (null (sq side i j))
                     (< (random 1.0) 0.65))
            (set-playfield (list side i j) (list nil t))))))))

;; The puzzles coming out of the above were so bad that I threw this together to
;; reject some of the obviously awful ones.
(defun generate-better-cube-puzzle (cube &optional (num-colors 6))
  (loop for i from 0 below 100 do
        (generate-cube-puzzle cube num-colors)
        (multiple-value-bind (solvable min-path-length)
            (check-victory cube)
          (assert solvable)
          (when (>= min-path-length 6)
            (return-from generate-better-cube-puzzle))))
  (format *trace-output* "~&Settling for lousy puzzle..~%"))

(defmethod initialize-instance :after ((pane logic-cube-pane) &rest args)
  (declare (ignore args))
  (generate-better-cube-puzzle pane 6)
  (cleanup-cube pane))

(defmethod compose-space ((pane logic-cube-pane) &key width height)
  (declare (ignore width height))
  ;; Hmm. How does one constrain the aspect ratio of a pane?
  (make-space-requirement :min-width 200
                          :min-height 200
                          :width 550
                          :height 550))

;; Math utilities

(defun lc-scaling-matrix (scale)
  (let ((matrix (make-array '(3 3) :initial-element 0.0)))
    (dotimes (i 3) (setf (aref matrix i i) scale))
    matrix))

(defun lc-m3xv3 (a b)                   ; multiply 3x3 matrix by vector
  (flet ((f (i) (loop for j from 0 below 3 sum (* (aref a i j) (elt b j)))))
    (vector (f 0) (f 1) (f 2))))

(defun lc-m3xm3 (a b)                   ; multiply two 3x3 matrices
  (let ((matrix (make-array '(3 3) :initial-element 0.0)))
    (dotimes (row 3)
      (dotimes (col 3)
        (dotimes (i 3)
          (incf (aref matrix row col) (* (aref a row i) (aref b i col))))))
    matrix))

(defun lc-rotation-matrix (theta axis-a axis-b)
  (let ((matrix (lc-scaling-matrix 1.0)))
    (setf (aref matrix axis-a axis-a) (cos theta)
          (aref matrix axis-a axis-b) (sin theta)
          (aref matrix axis-b axis-a) (- (sin theta))
          (aref matrix axis-b axis-b) (cos theta))
    matrix))

(defun lc-v+ (a b) (map 'vector #'+ a b)) ; 3-vector addition a+b
(defun lc-v- (a b) (map 'vector #'- a b)) ; 3-vector subtract a-b
(defun lc-scale (a s) (map 'vector (lambda (x) (* x s)) a)) ; 3-vector multiply by scalar

(defun lc-cross (a b)                   ; 3-vector cross product
  (macrolet ((woo (p q)
              `(- (* (elt a ,p) (elt b ,q ))
                  (* (elt a ,q) (elt b ,p)))))
    (vector (woo 1 2)
            (woo 2 0)
            (woo 0 1))))

;; Corner of hemicube is at origin.
;; Sides: 0=XY 1=XZ 2=YZ
(defun apply-to-hemicube-faces (n fn)
  (let ((size (/ n)))
    (dotimes (d 3)
      (flet ((permute (x y)
               ; SBCL warns (erroneously?) below, but the code works.
               (flet ((f (i) (elt (vector x y 0) (mod (+ d i) 3))))
                 (vector (f 0) (f 1) (f 2)))))
        (dotimes (i n)
          (dotimes (j n)
            (let ((base-x (* i size))
                  (base-y (* j size)))
              (funcall fn d i j
                       (permute base-x base-y)
                       (permute (+ base-x size) base-y)
                       (permute (+ base-x size) (+ base-y size))
                       (permute base-x (+ base-y size))))))))))

(defun lc-point-transformer (view-matrix)
  (lambda (point)
    (setf point (map 'vector (lambda (x) (- x 0.5)) point))
    (setf point (lc-m3xv3 view-matrix point))
    (let ((z (+ 2.0 (elt point 2)))
          (zoom 2.0))
      (vector (* zoom (/ (elt point 0) z))
              (* zoom (/ (elt point 1) z))
              z))))

(defun lc-scale-polygon (polygon amount)
  (let ((center  (reduce (lambda (a b) (lc-v+ a (lc-scale b (/ (length polygon))))) polygon
                        :initial-value #(0.0 0.0 0.0))))
    (mapcar (lambda (v) (lc-v+ center (lc-scale (lc-v- v center) amount))) polygon)))

(defun draw-polygon-3d (pane points &rest polygon-args)
  (apply #'draw-polygon pane
         (mapcar (lambda (p) (make-point (elt p 0) (elt p 1))) points)
         polygon-args))

(defun apply-to-transformed-faces (pane continuation)
  (let ((transformer (lc-point-transformer
                      (lc-m3xm3 (lc-scaling-matrix (- 1.0 (flyaway pane)))
                       (lc-m3xm3 (lc-rotation-matrix (pitch pane) 1 2)
                                 (lc-rotation-matrix (yaw pane)   0 2))))))
    (apply-to-hemicube-faces (density pane)
      (lambda (side i j &rest points)
        (apply continuation side i j (mapcar transformer points))))))

(defun lc-face-normal (points)
  (lc-cross (lc-v- (elt points 2)
                   (elt points 1))
            (lc-v- (elt points 0)
                   (elt points 1))))

(defun backface-p (points)
  (<= (elt (lc-face-normal points) 2) 0))

(defun face-light (color side)
  (compose-over (compose-in color (make-opacity 0.65))
                (elt (vector +gray30+ +white+ color) side)))

(defun polygon-edges (points)
  (maplist (lambda (list)
             (lc-v- (or (second list) (first points)) (first list)))
           points))

(defun draw-polygon-outline-3d (pane a b &rest polygon-args)
  (maplist (lambda (a* b*)
             (apply #'draw-polygon-3d pane
                    (list (first a*)
                          (first b*)
                          (or (second b*) (first b))
                          (or (second a*) (first a)))
                    polygon-args))
           a b))

(defun draw-logic-cube (pane)
  (apply-to-transformed-faces pane
    (lambda (side i j &rest camera-points)
      (unless (backface-p camera-points)
        (when (squeeze pane)
          (setf camera-points (lc-scale-polygon camera-points (squeeze pane))))
        (destructuring-bind (color type) (aref (playfield pane) side i j)
          (cond
            ((null type)
             (draw-polygon-3d pane (lc-scale-polygon camera-points 0.8)
                              :filled t :ink (face-light (or color +gray80+) side)))
            ((eql type 'terminal)
             (let ((selected (eql color (drag-color pane))))
               (when selected (draw-polygon-3d pane camera-points :filled t :ink color))
               (draw-polygon-outline-3d pane camera-points (lc-scale-polygon camera-points 0.7)
                                        :filled t
                                        :ink (if selected
                                                +white+
                                                (face-light (or color +gray80+) side)))))))))))

(defun invoke-in-lc-space (pane continuation) ; "logic-cube space" =p
  (let* ((width  (bounding-rectangle-width pane))
         (height (bounding-rectangle-height pane))
         (radius (/ (min width height) 2)))
    (with-translation (pane (/ width 2) (/ height 2))
      (with-scaling (pane radius)
        (funcall continuation pane)))))

(defmethod handle-repaint ((pane logic-cube-pane) region)
  (with-bounding-rectangle* (x0 y0 x1 y1) (sheet-region pane)
    ;; FIXME double buffering
    (draw-rectangle* pane x0 y0 x1 y1 :filled t :ink (background-color pane))
    (invoke-in-lc-space pane #'draw-logic-cube)
    (when (decorator pane)
      (funcall (decorator pane)))))

;;; Locating the face under the pointer:

(defun square (x) (* x x))

(defun point-in-poly-p (x y points)
  (every (lambda (point edge)
           (let* ((edge-length (sqrt (+ (square (elt edge 0)) (square (elt edge 1)))))
                  (nx (/ (- (elt edge 1)) edge-length))
                  (ny (/ (elt edge 0) edge-length))
                  (c (+ (* nx (elt point 0))
                        (* ny (elt point 1)))))
             (< c (+ (* nx x) (* ny y)))))
         points
         (polygon-edges points)))

(defun xy-to-viewport-coordinates (pane x y)
  (let* ((width  (bounding-rectangle-width pane)) ; ..
         (height (bounding-rectangle-height pane))
         (radius (/ (min width height) 2)))
    (values (/ (- x (/ width 2))  radius)
            (/ (- y (/ height 2)) radius))))

(defun find-poly-under-point (pane x y)
  (apply-to-transformed-faces pane
    (lambda (side i j &rest points)
      (unless (backface-p points)
        (when (point-in-poly-p x y points)
          (return-from find-poly-under-point (values side i j))))))
  (values nil nil nil))

;;; Game interaction:

(defmethod handle-event ((pane logic-cube-pane) (event pointer-exit-event))
  (setf (dragging pane) nil))

(defmethod handle-event ((pane logic-cube-pane) (event pointer-button-release-event))
  (setf (dragging pane) nil))

(defun square+ (pane side i j di dj)
  (let ((ni (+ i di))
        (nj (+ j dj)))
    (if (or (> 0 ni)
            (> 0 nj)
            (>= ni (density pane))
            (>= nj (density pane)))
        nil
        (list side ni nj))))

(defun adjacent-squares (pane side i j)
  (remove nil                           ; Ouch..
          (list (square+ pane side i j 1 0)
                (square+ pane side i j 0 1)
                (or (square+ pane side i j -1 0)
                    (and (= side 2) (list 1 j 0))
                    (and (= side 0) (list 2 j 0))
                    (and (= side 1) (list 0 j 0)))
                (or (square+ pane side i j 0 -1)
                    (and (= side 2) (list 0 0 i))
                    (and (= side 1) (list 2 0 i))
                    (and (= side 0) (list 1 0 i))))))

(defun check-victory (pane)
  (let ((success t)
        (min-path-length nil))
    (apply-to-hemicube-faces (density pane)
     (lambda (side i j &rest points)
       (declare (ignore points))
       (when (eql 'terminal (second (aref (playfield pane) side i j)))
         (let ((coverage (make-hash-table :test 'equal))
               (color (first (aref (playfield pane) side i j))))
           (labels ((searching (path-length &rest indices)
                      (setf (gethash indices coverage) t)
                      (some (lambda (indices)
                              (destructuring-bind (color-2 type) (apply #'aref (playfield pane) indices)
                                (and (eql color color-2)
                                     (not (gethash indices coverage))
                                     (or (and (eql type 'terminal)
                                              (setf min-path-length (if min-path-length
                                                                        (min min-path-length path-length)
                                                                        path-length)))
                                         (apply #'searching (1+ path-length) indices)))))
                            (apply #'adjacent-squares pane indices))))
             (unless (searching 1 side i j)
               (setf success nil)))))))
    (values success min-path-length))) ; Successful if no unconnected roots remained

(defun won-logic-cube (pane)
  (let ((start-time (get-internal-real-time))
        (spin-start-time 0.3)
        (text-style (make-text-style :serif :bold :huge))
        (start-yaw (yaw pane))
        (win-message (elt '("Great Success!" "You Win!" "Completed!"
                            "Vanquished!" "Terminated!" "Good job!" "Boom!")
                          (random 7))))
    (loop with sequence-length = 2.0
        as time = (/ (- (get-internal-real-time) start-time) internal-time-units-per-second sequence-length)
        while (< time 1.0) do
        (setf (squeeze pane) (1- time)
              (flyaway pane) (expt time 0.55)
              (yaw pane)
              (let ((foo (if (< time spin-start-time) 0.0 (/ (- time spin-start-time)
                                                             (- 1.0 spin-start-time)))))
                (+ start-yaw (* (expt foo 1.3) pi)))
              (decorator pane)
              (lambda ()
                (draw-text* pane win-message 13 13 :text-style text-style
                            :ink +black+ :align-y :top)
                (draw-text* pane win-message 10 10 :text-style text-style
                            :ink +white+ :align-y :top)))
        (repaint-sheet pane (sheet-region pane))
        (sleep 0.01)))                  ;)
  (generate-better-cube-puzzle pane)
  (cleanup-cube pane)
  (repaint-sheet pane (sheet-region pane)))

(defun drag-on-square (pane cell)
  (if (and (null (second cell)) (drag-color pane))
      (list (drag-color pane) nil)
      cell))

(defun touch-square (cube side i j)
  (when side
    (symbol-macrolet ((cell (aref (playfield cube) side i j)))
      (when (not (equalp cell (setf cell (drag-on-square cube cell)))) ; when we've genuinely changed the state..
        (when (check-victory cube)  ; .. check for win
          (repaint-sheet cube (sheet-region cube))
          (won-logic-cube cube))))))

(defmethod handle-event ((pane logic-cube-pane) (event pointer-motion-event))
  (multiple-value-bind (x y)
      (xy-to-viewport-coordinates pane (pointer-event-x event) (pointer-event-y event))
    (when (dragging pane) (multiple-value-call #'touch-square pane (find-poly-under-point pane x y)))
    (setf (yaw pane)   (- (* (/ pi 4) (max -1.0 (min 1.0 x))) (/ pi 4))
          (pitch pane) (min 0.0 (- (* (/ pi 4) (max -1.0 (min 1.0 y))) (/ pi 8))))
    (repaint-sheet pane (sheet-region pane))))

(defmethod handle-event ((pane logic-cube-pane) (event pointer-button-press-event))
  (multiple-value-bind (side i j)
      (multiple-value-call #'find-poly-under-point pane
                           (xy-to-viewport-coordinates pane (pointer-event-x event) (pointer-event-y event)))
    (when side
      (destructuring-bind (color type) (aref (playfield pane) side i j)
        (touch-square pane side i j)
        (setf (dragging pane) t
              (drag-color pane) (if (eql type 'terminal) color (drag-color pane))))))
  (repaint-sheet pane (sheet-region pane)))

(defun find-cube () (find-pane-named *application-frame* 'logic-cube))

(define-command-table logic-cube-game-commands)

(define-application-frame logic-cube () ()
  (:panes (logic-cube (make-pane 'logic-cube-pane)))
  (:layouts (:default logic-cube))
  (:command-table (logic-cube :inherit-from (logic-cube-game-commands)
                              :menu (("Game" :menu logic-cube-game-commands))))
  (:menu-bar t))

(define-command (com-reset-puzzle :menu t :command-table logic-cube-game-commands) ()
  (cleanup-cube (find-cube)))

(define-command (com-random-puzzle :menu t :command-table logic-cube-game-commands) ()
  (generate-better-cube-puzzle (find-cube))
  (cleanup-cube (find-cube)))

(add-menu-item-to-command-table
 (find-command-table 'logic-cube-game-commands)  nil
 :divider nil
 :errorp nil)

(define-command (com-lc-quit :menu "Quit" :command-table logic-cube-game-commands) ()
  (frame-exit *application-frame*))
