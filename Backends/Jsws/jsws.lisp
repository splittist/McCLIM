;;;; jsws.lisp

(defpackage #:clim-jsws
  (:use #:clim #:climi #:clime #:climb #:clim-lisp)
  #+(or)(:import-from #:climi
                )
  )

(in-package #:clim-jsws)

;;; Port

(defclass jsws-port (basic-port)
  ((%id :accessor port-id)))

(defmethod find-port-type ((type (eql :jsws)))
  (values 'jsws-port 'identity)) ; FIXME

(defmethod initialize-instance :after ((port jsws-port) &rest initargs)
  (declare (ignore initargs))
  (setf (port-id port) (gensym "JSWS-PORT-")))

;;; Sheet

;;; Mirror

;;; Graft

;;; Medium

(defclass jsws-medium (basic-medium)
  ())

(defmethod make-medium ((port jsws-port) sheet)
  (make-instance 'jsws-medium :port port :sheet sheet))

(defmethod medium-drawable ((medium jsws-medium))
  (first (hunchensocket:clients (first *ports*)))) ;;; FIXME DEBUG

;;; Ws

(defvar *mailboxes-lock* (bt:make-lock "mailboxes lock"))

(defvar *mailboxes* (list))

(defclass mailbox ()
  ((%thread
    :initarg :thread
    :reader mailbox-thread)
   (%lock
    :initform (bt:make-lock)
    :reader mailbox-lock)
   (%cv
    :initform (bt:make-condition-variable)
    :reader mailbox-condition-variable)
   (%queue
    :initform (list)
    :accessor mailbox-queue)))

(defun mailbox (thread)
  (bt:with-lock-held (*mailboxes-lock*)
    (or (find thread *mailboxes* :key #'mailbox-thread)
        (let ((mb (make-instance 'mailbox :thread thread)))
          (push mb *mailboxes*)
          mb))))

(defun send (thread message)
  (let* ((mbox (mailbox thread))
         (lock (mailbox-lock mbox)))
    (bt:with-lock-held (lock)
      (setf (mailbox-queue mbox)
            (nconc (mailbox-queue mbox) (list message)))
      #+(or)(bt:condition-notify (mailbox-condition-variable mbox))  ; FIXME condition-broadcast
      (sb-thread:condition-broadcast (mailbox-condition-variable mbox)))))

(defun wake-thread (thread)
  (let* ((mbox (mailbox thread))
         (lock (mailbox-lock mbox)))
    (bt:with-recursive-lock-held (lock)
      (sb-thread:condition-broadcast (mailbox-condition-variable mbox)))))

(defun receive-if (test &optional timeout)
  (let* ((mbox (mailbox (bt:current-thread)))
         (lock (mailbox-lock mbox))
         (cv (mailbox-condition-variable mbox)))
    (loop
          (bt:with-lock-held (lock)
            (let* ((q (mailbox-queue mbox))
                   (tail (member-if test q)))
              (when tail
                (setf (mailbox-queue mbox)
                      (nconc (ldiff q tail) (cdr tail)))
                (return (car tail)))
              (when (eq timeout t) (return (values nil t)))
              (bt:condition-wait cv lock))))))

(defvar *tag-counter* 0)

(defun make-tag ()
  (setf *tag-counter* (mod (1+ *tag-counter*) (expt 2 16))))

(defun text-size-from-browser (medium string text-style)
  (let ((client (medium-drawable medium))
        (tag (make-tag))
        (options (jsws-text-style text-style)))
    (alexandria:appendf options (list "left" 0 "top" 0))
    (send-command client "textSize" (list string (jsobj options) tag))
    (let ((response
            (receive-if #'(lambda (msg) (and (eq :text-size (first msg))
                                             (eql tag (second msg)))))))
      (list (third response) (fourth response)))))

(defvar *thread* nil)

(defvar *server* (make-instance 'hunchensocket:websocket-acceptor :port 4242))

(defclass client (hunchensocket:websocket-client)
  ())

(defclass ws-port (hunchensocket:websocket-resource)
  ((%name :initarg :name :initform "/port" :accessor port-name))
  (:default-initargs :client-class 'client))

(defvar *ports* (list (make-instance 'ws-port)))

(defun find-port (request)
  (find (hunchentoot:script-name request) *ports* :test #'string= :key #'port-name))

(pushnew 'find-port hunchensocket:*websocket-dispatch-table*)

(defmethod hunchensocket:client-connected ((port ws-port) client)
  (format t "~A has joined ~A"  client (port-name port)))

(defmethod hunchensocket:client-disconnected ((port ws-port) client)
  (format t "~A has left ~A" client (port-name port)))

(defmethod hunchensocket:text-message-received ((port ws-port) client message)
  (format t "~&Received '~A' from ~A~%" message client)
  (send *thread* (read-from-string message)))

;;;

(defun pt>px (pt)
  (/ (* pt 96) 72))

(defun jsobj (args)
  (apply #'st-json:jso args))

(defun send-command (client command args)
  (let ((msg (st-json:write-json-to-string (list (list command args)))))
    (print msg)
    (hunchensocket:send-text-message client msg)))

(defun jsws-client (thing)
  (etypecase thing
    (client thing)))

(defun jsws-color (ink)
  (if ink
      (multiple-value-bind (r g b a) (color-rgba ink)
        (format nil "rgba(~F%, ~F%, ~F%, ~F)" (* r 100) (* g 100) (* b 100) a))
      "none"))

(defun jsws-stroke-width (line-style)
  (let ((unit (line-style-unit line-style))
        (thickness (line-style-thickness line-style)))
    (ecase unit
      ((:normal :coordinate) thickness)
      (:point (pt>px thickness)))))

(defun jsws-stroke-line-join (line-style)
  (ecase (line-style-joint-shape line-style)
    (:miter "miter")
    (:round "round")
    (:bevel "bevel")
    (:none nil)))

(defun jsws-stroke-line-cap (line-style)
  (ecase (line-style-cap-shape line-style)
    (:butt "butt")
    (:round "round")
    (:square "square")
    (:no-end-point nil))) ;; FIXME ??

(defun jsws-stroke-dash-array (line-style)
  (let ((dashes (line-style-dashes line-style)))
    (cond ((null dashes)
           nil)
          ((eq t dashes)
           (list 3 3))
          ((listp dashes)
           dashes)
          ((vectorp dashes)
           (coerce dashes 'list))
          (t
           (error "Unknown line-style-dashes entry: ~S" dashes)))))

(defparameter *string-size-cache*
  (make-hash-table :test 'equal))

(defun get-string-size (medium string text-style)
  (let* ((style-list (multiple-value-list (text-style-components text-style)))
         (cache-key (list medium string style-list)))
    (alexandria:if-let ((cache-value (gethash cache-key *string-size-cache*)))
      cache-value
      (let ((size (text-size-from-browser medium string text-style)))
        (setf (gethash cache-key *string-size-cache*) size)))))

(defparameter *text-style-metrics-cache*
  (make-hash-table :test #'equal)
  "A hash-table, the KEYs of which are lists (family face size), and the VALUES of which are
four-element vector: width, height, ascent, descent")

(defun clear-text-caches ()
  (setf *string-size-cache*
        (make-hash-table :test 'equal))
  (setf *text-style-metrics-cache*
        (make-hash-table :test 'equal)))

(defun tsmetric->index (metric)
  (ecase metric
    (:width 0)
    (:height 1)
    (:ascent 2)
    (:descent 3)))

(defun make-text-style-metrics-cache-entry ()
  (make-array 4 :initial-element nil))

(defun get-text-style-metric (text-style metric if-not-found)
  (let ((key (multiple-value-list (text-style-components text-style)))
        (index (tsmetric->index metric)))
    (multiple-value-bind (entry foundp)
        (gethash key *text-style-metrics-cache*)
      (if foundp
          (alexandria:if-let ((val (svref entry index)))
            val
            (setf (svref entry index) (funcall if-not-found)))
          (let ((entry (make-text-style-metrics-cache-entry)))
            (prog1 (setf (svref entry index) (funcall if-not-found))
              (setf (gethash key *text-style-metrics-cache*) entry)))))))

(defun text-style-base (medium text-style)
  (get-string-size medium "M" text-style))

(defmethod text-style-width ((text-style standard-text-style) (medium jsws-medium))
  (get-text-style-metric text-style
                         :width
                         #'(lambda () (first (text-style-base medium text-style)))))

(defmethod text-style-ascent ((text-style standard-text-style) (medium jsws-medium))
  (get-text-style-metric text-style
                         :ascent
                         #'(lambda () (second (get-string-size medium "A" text-style)))))

(defmethod text-style-descent ((text-style standard-text-style) (medium jsws-medium))
  (get-text-style-metric text-style
                         :descent
                         #'(lambda () (- (second (get-string-size medium "y" text-style))
                                         (second (get-string-size medium "v" text-style))))))

(defmethod text-style-height ((text-style standard-text-style) (medium jsws-medium))
  (get-text-style-metric text-style
                         :height
                         #'(lambda () (+ (text-style-ascent text-style medium)
                                         (text-style-descent text-style medium)))))

(defmethod text-size ((medium jsws-medium) string &key text-style (start 0) end)
  (let* ((string (string string))
         (text-style (or text-style (medium-text-style medium)))
         (end (or end (length string)))
         (line-height (text-style-height text-style medium))
         (total-height 0)
         (width 0)
         (max-width 0))
    (climi::dolines (line (subseq string start end)
                          (values max-width total-height
                                  width (- total-height line-height)
                                  (- total-height (text-style-descent text-style medium))))
      (setf width (if (zerop (length line))
                      0
                      (first (get-string-size medium line text-style))))
      (incf total-height line-height)
      (alexandria:maxf max-width width))))

(defmethod text-style-fixed-width-p ((text-style standard-text-style) (medium jsws-medium))
  (eq :fix (text-style-family text-style)))

(defmethod medium-draw-line* ((medium jsws-medium) x1 y1 x2 y2)
  (let* ((client (medium-drawable medium))
         (ink (medium-ink medium))
         (color (jsws-color ink))
         (line-style (medium-line-style medium))
         (stroke-width (jsws-stroke-width line-style))
         (stroke-line-join (jsws-stroke-line-join line-style))
         (stroke-line-cap (jsws-stroke-line-cap line-style))
         (stroke-dash-array (jsws-stroke-dash-array line-style))
         (points (list x1 y1 x2 y2))
         (options '()))
    (when color
      (alexandria:appendf options (list "stroke" color)))
    (when stroke-width
      (alexandria:appendf options (list "strokeWidth" stroke-width)))
    (when stroke-line-join
      (alexandria:appendf options (list "strokeLineJoin" stroke-line-join)))
    (when stroke-line-cap
      (alexandria:appendf options (list "strokeLineCap" stroke-line-cap)))
    (when stroke-dash-array
      (alexandria:appendf options  (list "strokeDashArray" stroke-dash-array)))
    (send-command client "drawLine" (list points (jsobj options)))))

(defmethod medium-draw-rectangle* ((medium jsws-medium) left top right bottom filled)
  (let* ((client (medium-drawable medium))
         (ink (medium-ink medium))
         (color (jsws-color ink))
         (line-style (medium-line-style medium))
         (stroke-width (jsws-stroke-width line-style))
         (stroke-line-join (jsws-stroke-line-join line-style))
         (stroke-line-cap (jsws-stroke-line-cap line-style))
         (stroke-dash-array (jsws-stroke-dash-array line-style))
         (args (list "left" left "top" top "width" (- right left) "height" (- bottom top))))
    (cond (filled
           (when color
             (alexandria:appendf args (list "fill" color))))
          (t
           (when color
             (alexandria:appendf args (list "stroke" color
                                            "fill" (jsws-color clim:+transparent-ink+))))
           (when stroke-width
             (alexandria:appendf args (list "strokeWidth" stroke-width)))
           (when stroke-line-join
             (alexandria:appendf args (list "strokeLineJoin" stroke-line-join)))
           (when stroke-line-cap
             (alexandria:appendf args (list "strokeLineCap" stroke-line-cap)))
           (when stroke-dash-array
             (alexandria:appendf args (list "strokeDashArray" stroke-dash-array)))))
    (send-command client "drawRect" (jsobj args))))

(defmethod medium-draw-circle* ((medium jsws-medium) center-x center-y radius start-angle end-angle filled)
  (let* ((client (medium-drawable medium))
         (ink (medium-ink medium))
         (color (jsws-color ink))
         (args (list "left" (- center-x radius)
                     "top" (- center-y radius)
                     "radius" radius
                     "startAngle" (- end-angle)
                     "endAngle"  (- start-angle)))) ;;; FIXME slice v arc
    (if filled
        (when color
          (alexandria:appendf args (list "fill" color)))
        (let* ((line-style (medium-line-style medium))
               (stroke-width (jsws-stroke-width line-style))
               (stroke-line-join (jsws-stroke-line-join line-style))
               (stroke-line-cap (jsws-stroke-line-cap line-style))
               (stroke-dash-array (jsws-stroke-dash-array line-style)))
          (when color
            (alexandria:appendf args (list "stroke" color
                                           "fill" (jsws-color clim:+transparent-ink+))))
           (when stroke-width
             (alexandria:appendf args (list "strokeWidth" stroke-width)))
           (when stroke-line-join
             (alexandria:appendf args (list "strokeLineJoin" stroke-line-join)))
           (when stroke-line-cap
             (alexandria:appendf args (list "strokeLineCap" stroke-line-cap)))
           (when stroke-dash-array
             (alexandria:appendf args (list "strokeDashArray" stroke-dash-array)))))
    (send-command client "drawCircle" (jsobj args))))

(defmethod medium-draw-ellipse* ((medium jsws-medium) center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy start-angle end-angle filled)
  (let* ((client (medium-drawable medium))
         (ink (medium-ink medium))
         (color (jsws-color ink))
         (rx (sqrt (+ (expt radius-1-dx 2) (expt radius-1-dy 2))))
         (ry (sqrt (+ (expt radius-2-dx 2) (expt radius-2-dy 2))))
         (args (list "left" (- center-x rx) ; FIXME rotation
                     "top" (- center-y ry)
                     "rx" rx
                     "ry" ry
                     "startAngle" (- end-angle)
                     "endAngle" (- start-angle)))) ; FIXME slice v arc
    (if filled
        (when color
          (alexandria:appendf args (list "fill" color)))
        (let* ((line-style (medium-line-style medium))
               (stroke-width (jsws-stroke-width line-style))
               (stroke-line-join (jsws-stroke-line-join line-style))
               (stroke-line-cap (jsws-stroke-line-cap line-style))
               (stroke-dash-array (jsws-stroke-dash-array line-style)))
          (when color
            (alexandria:appendf args (list "stroke" color
                                           "fill" (jsws-color clim:+transparent-ink+))))
           (when stroke-width
             (alexandria:appendf args (list "strokeWidth" stroke-width)))
           (when stroke-line-join
             (alexandria:appendf args (list "strokeLineJoin" stroke-line-join)))
           (when stroke-line-cap
             (alexandria:appendf args (list "strokeLineCap" stroke-line-cap)))
           (when stroke-dash-array
             (alexandria:appendf args (list "strokeDashArray" stroke-dash-array)))))
    (send-command client "drawEllipse" (jsobj args))))

(defmethod medium-draw-point* ((medium jsws-medium) x y)
  (let* ((client (medium-drawable medium))
         (ink (medium-ink medium))
         (radius (/ (jsws-stroke-width (medium-line-style medium)) 2))
         (args (list "left" (- x radius)
                     "top" (- y radius)
                     "radius" radius
                     "fill" (jsws-color ink))))
    (send-command client "drawCircle" (jsobj args))))

(defun polypoints (coord-seq)
  (loop for (x y) on (coerce coord-seq 'list) by #'cddr
        collecting (st-json:jso "x" x "y" y)))

(defmethod medium-draw-polygon* ((medium jsws-medium) coord-seq closed filled)
  (let* ((client (medium-drawable medium))
         (ink (medium-ink medium))
         (color (jsws-color ink))
         (points (polypoints coord-seq))
         (command (if (and (null filled)
                           (null closed))
                      "drawPolyline"
                      "drawPolygon"))
         (options '()))
    (if filled
        (when color
          (alexandria:appendf options (list "fill" color)))
        (let* ((line-style (medium-line-style medium))
               (stroke-width (jsws-stroke-width line-style))
               (stroke-line-join (jsws-stroke-line-join line-style))
               (stroke-line-cap (jsws-stroke-line-cap line-style))
               (stroke-dash-array (jsws-stroke-dash-array line-style)))
          (when color
            (alexandria:appendf options (list "stroke" color
                                              "fill" (jsws-color clim:+transparent-ink+))))
          (when stroke-width
            (alexandria:appendf options (list "strokeWidth" stroke-width)))
          (when stroke-line-join
            (alexandria:appendf options (list "strokeLineJoin" stroke-line-join)))
          (when stroke-line-cap
            (alexandria:appendf options (list "strokeLineCap" stroke-line-cap)))
          (when stroke-dash-array
            (alexandria:appendf options (list "strokeDashArray" stroke-dash-array)))))
    (send-command client command (list points (jsobj options)))))

(defun jsws-font-family (family)
  (ecase family
    (:fix "monospace")
    (:serif "serif")
    (:sans-serif "sans-serif")
    ('nil (jsws-font-family (text-style-family *default-text-style*)))))

(defun jsws-font-size (size)
  (etypecase size
    (real (pt>px size))
    (symbol
     (case size
       ('nil (jsws-font-size (text-style-size *default-text-style*)))
       (:smaller (jsws-font-size (climi::find-smaller-size (text-style-size *default-text-style*))))
       (:larger (jsws-font-size (climi::find-larger-size (text-style-size *default-text-style*))))
       (t (pt>px (cadr (member size climi::+font-sizes+))))))))

(defun jsws-font-style (face)
  (cond
    ((null face) (jsws-font-face (text-style-face *default-text-style*)))
    ((eq :roman face) "normal")
    ((eq :italic face) "italic")
    ((and (consp face) (find :italic face)) "italic")
    (t "nomral")))

(defun jsws-font-weight (face)
  (if (or (eq face :bold)
          (and (consp face) (find :bold face)))
      "bold"
      "normal"))

(defun jsws-text-style (text-style)
  (multiple-value-bind (family face size)
      (text-style-components text-style)
    (list "fontFamily" (jsws-font-family family)
          "fontSize" (jsws-font-size size)
          "fontStyle" (jsws-font-style face)
          "fontWeight" (jsws-font-weight face))))

(defmethod medium-draw-text* ((medium jsws-medium) string x y start end align-x align-y toward-x toward-y transform-glyphs)
  (let* ((client (medium-drawable medium))
         (ink (medium-ink medium))
         (color (jsws-color ink))
         (str (subseq string start end))
         (style (medium-text-style medium))
         (size (jsws-font-size (text-style-size style)))
         (options (jsws-text-style style)))
    (multiple-value-bind (width height final-x final-y baseline)
        (text-size medium string :text-style style)
      (declare (ignore final-x final-y))
      (let* ((_fontSizeFraction 222) ; from Fabric js
             (lineHeight 1.16) ; ditto
             (finagling-factor (- baseline(/ (* height (- 1 _fontSizeFraction)) lineHeight))))
        (incf y finagling-factor))
      (ecase align-y
        (:top (incf y baseline))
        (:baseline nil)
        (:bottom (decf y (text-style-descent style medium)))
        (:center (incf y (/ baseline 2))))
      (ecase align-x
        (:left nil)
        (:center (decf x (/ width 2)))
        (:right (decf x width)))
      (alexandria:appendf options (list "left" x
                                        "top" (- y size)))
      (alexandria:appendf options (list "fill" color))
      (send-command client "drawText" (list str (jsobj options))))))

(defun text-text ()
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
  (medium-draw-text* *m* "Hello jello" 200 450 0 nil :right :baseline 0 0 nil))
