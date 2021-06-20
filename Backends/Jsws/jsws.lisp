;;;; jsws.lisp

(in-package #:clim-jsws)

(defun text-text ()
  (let ((*m* (make-medium (make-instance 'jsws-port) nil)))
    (clear-text-caches)
    (with-drawing-options (*m* :ink +red+)
      (medium-draw-line* *m* 0 100 500 100))
    (medium-draw-text* *m* "Hello jello" 50 100 0 nil :left :top 0 0 nil)
    (medium-draw-text* *m* "Hello jello" 150 100 0 nil :left :bottom 0 0 nil)
    (medium-draw-text* *m* "Hello jello" 250 100 0 nil :left :center 0 0 nil)
    (medium-draw-text* *m* "Hello jello" 350 100 0 nil :left :baseline 0 0 nil)
    (with-drawing-options (*m* :ink +red+)
      (medium-draw-line* *m* 200 200 200 500))
    (medium-draw-text* *m* "Hello jello" 200 250 0 nil :left :baseline 0 0 nil)
    (medium-draw-text* *m* "Hello jello" 200 350 0 nil :center :baseline 0 0 nil)
    (medium-draw-text* *m* "Hello jello" 200 450 0 nil :right :baseline 0 0 nil)))

#+(or)(defun test ()
  (let* ((p (make-instance 'jsws-port))
         (fm (first (slot-value p 'climi::frame-managers)))
         (*default-server-path* (list :jsws)))
    (with-frame-manager (fm) (clim-demo:demodemo))))

(defun clean-up ()
  (let ((processes (remove-if-not
                    #'(lambda (thread)
                        (alexandria:starts-with-subseq "#<JSWS" (bt:thread-name thread)))
                    (bt:all-threads))))
    (map nil #'bt:destroy-thread processes)))

(defclass test-stream (sheet-leaf-mixin
                       sheet-parent-mixin
                       sheet-transformation-mixin
                       sheet-mute-repainting-mixin
                       climi::updating-output-stream-mixin
                       basic-sheet
                       top-level-sheet-mixin
                       standard-extended-output-stream
                       standard-extended-input-stream
                       permanent-medium-sheet-output-mixin
                       standard-output-recording-stream)
  ((port :initform nil :initarg port :accessor port)))

(defclass dom-sheet (sheet-parent-mixin
                     sheet-transformation-mixin
                     sheet-multiple-child-mixin
                     top-level-sheet-mixin
                     permanent-medium-sheet-output-mixin
                     basic-sheet
                     standard-extended-input-stream)
  ((port :initform nil :initarg port :accessor port)))

(defmacro with-output-to-browser ((stream-var) &body body)
  (let ((cont (gensym)))
    `(flet ((,cont (,stream-var)
              ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-output-to-browser #',cont))))

(defun invoke-with-output-to-browser (continuation)
  (let ((*default-server-path* (list :jsws)))
    (with-port (port :jsws)
      (let ((stream (make-instance 'test-stream :port port))
            (graft (find-graft :port port)))
        (sheet-adopt-child graft stream)
        (setf (sheet-region stream) (sheet-region graft))
        (funcall continuation stream)))))

(defparameter *p* nil)
(defparameter *s* nil)
(defparameter *fm* nil)


(defun drain-events ()
  (loop for e = (receive-if (lambda (e) (declare (ignore e)) (constantly t)) t)
        while e))

(defun testx ()
  (clean-up)
  (clear-text-caches)
  (drain-events)
  (setf *p* (make-instance 'jsws-port))
  (setf *s* (make-instance 'test-stream :port *p*))
  (sheet-adopt-child (find-graft :port *p*) *s*)
  (setf (sheet-region *s*) (sheet-region (find-graft :port *p*)))
  (setf (port-keyboard-input-focus *p*) *s*)
  (setf *fm* (first (slot-value *p* 'climi::frame-managers))))

(define-application-frame test-frame () ()
  (:panes
   (b toggle-button)
   (e :interactor-pane :width 300 :max-width +fill+ :height 300 :max-height +fill+))
  (:layouts
   (default
    (vertically ()
      b (scrolling () e)))
   (other
    (vertically ()
      (scrolling () e) b))))

(define-test-frame-command (com-switch :name t :menu t)
    ()
  (setf (frame-current-layout *application-frame*)
        (ecase (frame-current-layout *application-frame*)
          (default other)
          (other default))))

(defun test-events ()
  (labels ((clear-arrow ()
             (draw-circle* *s* 0 0 100 :ink +white+))
           (draw-pointer (x y)
             (let* ((scale (sqrt (+ (expt x 2)
                                       (expt y 2))))
                    (final-x (+ (/ x scale) 200))
                    (final-y (+ (/ y scale) 200)))
               (clear-arrow)
               (draw-arrow* *s* 200 200 final-x final-y :ink +blue+)))
           (escapep (event)
             (and event
                  (eq :key-press (event-type event))
                  (eq :escape (keyboard-event-key-name event)))))
    (loop for event = (get-event *p* nil nil)
          until (escapep event)
          do (print event)
          when event
            do (case (event-type event)
                 (:pointer-motion
                  (draw-pointer (pointer-event-x event)
                                (pointer-event-y event)))
                 (:key-press
                  (and (keyboard-event-character event)
                       (princ (keyboard-event-character event) *s*)))))))

(define-application-frame smallest () ()
  (:pane :application
         :scroll-bars nil)
  (:menu-bar nil))

(defun testy ()
  (clean-up)
  (clear-text-caches)
  (drain-events)
  (setf *p* (make-instance 'jsws-port))
  (setf *s* (make-instance 'dom-sheet :port *p*))
  (sheet-adopt-child (find-graft :port *p*) *s*)
  (setf (sheet-region *s*) +everywhere+)
  (setf *fm* (first (slot-value *p* 'climi::frame-managers))))

(climi::with-transformed-position 
