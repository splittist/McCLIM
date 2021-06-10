;;;; jsws.lisp

(defpackage #:clim-jsws
  (:use #:clim #:climi #:clime #:climb #:clim-lisp)
  #+(or)(:import-from #:climi
                )
  )

(in-package #:clim-jsws)

;;; Port

(defclass jsws-port (basic-port)
  ((%id :accessor port-id)
   (%client
    :accessor port-client)))

(defclass jsws-pointer (standard-pointer)
  ())

(defclass jsws-graft (graft)
  ())

(defmethod make-graft ((port jsws-port) &key (orientation :default) (units :device))
  (let* ((client (port-client port))
         (window (make-window client (gensym "ID-") t))
         (mirror (make-instance 'jsws-mirror :window window))
         (screen-size (screen-size-from-browser client))
         (region (make-bounding-rectangle 0 0 (getf screen-size :width) (getf screen-size :height))))
    (make-instance 'jsws-graft
                   :port port
                   :region region
                   :mirror mirror
                   :orientation orientation
                   :units units)))

(defmethod graft ((port jsws-port))
  (first (climi::port-grafts port)))

(defclass jsws-window ()
  ((%client
    :initarg :client
    :reader window-client)
   (%id
    :initarg :id
    :reader window-id)))

(defun make-window (client id &optional canvasp)
  (when canvasp
    (send-command client "createWindow" (list (princ-to-string id) "fabricCanvas")))
  (make-instance 'jsws-window :client client :id id))

(defclass jsws-mirror ()
  ((%window
    :initarg :window
    :reader mirror-window)))

(defclass jsws-frame-manager (standard-frame-manager)
  ())

(defmethod find-port-type ((type (eql :jsws)))
  (values 'jsws-port 'identity)) ; FIXME

(defmethod initialize-instance :after ((port jsws-port) &rest initargs)
  (declare (ignore initargs))
  (setf (port-id port) (gensym "JSWS-PORT-"))
  (push (make-instance 'jsws-frame-manager :port port)
        (slot-value port 'climi::frame-managers))
  (setf (slot-value port 'pointer)
        (make-instance 'jsws-pointer :port port))
  (initialize-jsws port))

(defun initialize-jsws (port)
  (setf (port-client port)
        (first (hunchensocket:clients (first *sockets*)))) ; FIXME
  (make-graft port)
  (when clim-sys:*multiprocessing-p*
    (clim-sys:make-process
     (lambda ()
       (loop (with-simple-restart
                 (restart-event-loop "Restart CLIM's event loop")
               (loop (process-next-event port)))))
     :name (format nil "~S's event process" port))))

(defmethod process-next-event ((port jsws-port) &key wait-function (timeout nil))
  (when (climi::maybe-funcall wait-function)
    (return-from process-next-event
      (values nil :wait-function)))
  (let ((event (get-event port timeout))K) ; FIXME TODO (receive-if ...)
    (case event
      ((nil)
       (if (climi::maybe-funcall wait-function)
           (values nil :wait-function)
           (values nil :timeout)))
      ((t)
       (values nil :wait-function))
      (otherwise
       (prog1 t
         (distribute-event port event))))))

(defparameter *character-name-translations*
  '(("Alt" :alt)
    ("CapsLock" :caps-lock)
    ("Control" :control)
    ("Meta" :meta)
    ("NumLock" :num-lock)
    ("ScrollLock" :scroll-lock)
    ("Shift" :shift)
    ("Hyper" :hyper)
    ("Super" :super)
    ("Enter" :return)
    ("Tab" :tab)
    ("ArrowDown" :down)
    ("ArrowLeft" :left)
    ("ArrowRight" :right)
    ("ArrowUp" :up)
    ("End" :end)
    ("Home" :home)
    ("PageDown" :page-down)
    ("PageUp" :page-up)
    ("Backspace" :backspace)
    ("Clear" :clear)
    ("Delete" :delete)
    ("Insert" :insert)
    ("Redo" :redo)
    ("Undo" :undo)
    ("Cancel" :cancel)
    ("ContextMenu" :menu)
    ("Escape" :escape)
    ("Execute" :execute)
    ("Find" :find)
    ("Help" :help)
    ("Pause" :pause)
    ("Select" :select)
    ("F1" :f1)
    ("F2" :f2)
    ("F3" :f3)
    ("F4" :f4)
    ("F5" :f5)
    ("F6" :f6)
    ("F7" :f7)
    ("F8" :f8)
    ("f9" :f9)
    ("F10" :f10)
    ("F11" :f11)
    ("F12" :f12)))

(defun process-name (event)
  (let* ((name (getf event :char))
         (loc (getf event :loc))) ; 0 standard 1 left 2 right 3 numpad
    (if (= 1 (length name))
        (values (character name) (intern name :keyword))
        (let ((base-name
                (second
                 (assoc name *character-name-translations* :test #'string=))))
          (values nil (or base-name (intern name :keyword))))))) ; FIXME

(defun get-event (port wait-function &optional (timeout nil))
  (flet ((eventp (item) (eq :event (car item))))
    (let ((event (receive-if #'eventp timeout)))
      (case (second event)
        ((:key-press-event :key-release-event)
         (multiple-value-bind (character name)
             (process-name event)
           (make-instance (if (eq :key-press-event (car event))
                              'key-press-event
                              'key-release-event)
                          :key-character character
                          :key-name name
                          :modifier-state (getf event :mod)
                          :sheet (port-keyboard-input-focus port))))
        ((:pointer-button-press-event :pointer-button-release-event)
         (make-instance (if (eq :pointer-button-press-event (car event))
                            'pointer-button-press-event
                            'pointer-button-release-event)
                        :button (getf event :but)
                        :x (getf event :x)
                        :y (getf event :y)
                        :pointer (port-pointer port)
                        :sheet (graft port))) ; FIXME
        ((:pointer-enter-event :pointer-exit-event)
         (make-instance (if (eq :pointer-enter-event (car event))
                            'pointer-enter-event
                            'pointer-exit-event)
                        :button (getf event :but)
                        :x (getf event :x)
                        :y (getf event :y)
                        :pointer (port-pointer port)
                        :sheet (graft port))) ; FIXME
        (:pointer-move-event
         (make-instance 'pointer-move-event
                        :button (getf event :but)
                        :x (getf event :x)
                        :y (getf event :y)
                        :pointer (port-pointer port)
                        :sheet (graft port))) ; FIXME - all the same, too
        (t
         (climi::maybe-funcall wait-function))))))

(defclass jsws-medium (basic-medium)
  ())

(defmethod make-medium ((port jsws-port) sheet)
  (make-instance 'jsws-medium :port port :sheet sheet))

(defun dummy-window ()
  (make-instance 'jsws-window
                 :client (first (hunchensocket:clients (first *sockets*)))
                 :id "canvas"))

(defmethod medium-drawable ((medium jsws-medium))
  (mirror-window (slot-value (graft (port medium)) 'climi::mirror))) ;;; FIXME DEBUG

;;; INPUT

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
  (let* ((window (medium-drawable medium))
         (client (window-client window))
         (id (window-id window))
         (tag (make-tag))
         (options (jsws-text-style text-style)))
    (alexandria:appendf options (list "left" 0 "top" 0))
    (send-command client "textSize" (list (princ-to-string id) string (jsobj options) tag))
    (let ((response
            (receive-if #'(lambda (msg) (and (eq :text-size (first msg))
                                             (eql tag (second msg)))))))
      (list (third response) (fourth response)))))

(defun screen-size-from-browser (client)
  (let ((tag (make-tag)))
    (send-command client "screenSize" (list tag))
    (let ((response
            (receive-if #'(lambda (msg) (and (eq :screen-size (first msg))
                                             (eql tag (second msg)))))))
      (nthcdr 2 response))))

(defvar *thread* nil)

(defvar *server* (make-instance 'hunchensocket:websocket-acceptor :port 4242))

(defclass client (hunchensocket:websocket-client)
  ())

(defclass ws-socket (hunchensocket:websocket-resource)
  ((%name :initarg :name :initform "/port" :accessor socket-name))
  (:default-initargs :client-class 'client))

(defvar *sockets* (list (make-instance 'ws-socket)))

(defun find-socket (request)
  (find (hunchentoot:script-name request) *sockets* :test #'string= :key #'socket-name))

(defmethod hunchensocket:client-connected ((socket ws-socket) client)
  (format t "~A has joined ~A"  client (socket-name socket)))

(defmethod hunchensocket:client-disconnected ((socket ws-socket) client)
  (format t "~A has left ~A" client (socket-name socket)))

(defmethod hunchensocket:text-message-received ((socket ws-socket) client message)
  (format t "~&Received '~A' from ~A~%" message client)
  (send *thread* (read-from-string message)))

(defun start-server ()
  (pushnew 'find-socket hunchensocket:*websocket-dispatch-table*)
  (hunchentoot:start *server*)
  (setf *thread* (bt:current-thread)))

(defun stop-server ()
  (hunchentoot:stop *server*))

;;;

(defun pt>px (pt)
  (/ (* pt 96) 72))

(defun jsobj (args)
  (apply #'st-json:jso args))

(defgeneric send-command (destination command args)
  (:method ((window jsws-window) command args)
    (let ((client (window-client window))
          (msg (st-json:write-json-to-string
                (list
                 (list command
                       (list (princ-to-string (window-id window))
                             args))))))
      (print msg) ; DEBUG
      (hunchensocket:send-text-message client msg)))
  (:method ((client client) command args)
    (let ((msg (st-json:write-json-to-string (list (list command args)))))
      (print msg) ; DEBUG
      (hunchensocket:send-text-message client msg))))

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

(defmethod climb:text-bounding-rectangle* ((medium jsws-medium) string
                                           &key text-style start end align-x align-y direction)
  (declare (ignore align-x align-y direction))
  (let* ((sub (subseq string (or start 0) (or end (length string))))
         (text-style (or text-style (medium-text-style medium))))
    (multiple-value-bind (width height x y baseline)
        (text-size medium sub :text-style text-style)
      (declare (ignore x y))
      (values 0 (- baseline) width (- height baseline)))))

(defmethod text-style-fixed-width-p ((text-style standard-text-style) (medium jsws-medium))
  (eq :fix (text-style-family text-style)))

(defmethod medium-draw-line* ((medium jsws-medium) x1 y1 x2 y2)
  (let* ((window (medium-drawable medium))
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
    (send-command window "drawLine" (list points (jsobj options)))))

(defmethod medium-draw-rectangle* ((medium jsws-medium) left top right bottom filled)
  (let* ((window (medium-drawable medium))
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
    (send-command window "drawRect" (jsobj args))))

(defmethod medium-draw-circle* ((medium jsws-medium) center-x center-y radius start-angle end-angle filled)
  (let* ((window (medium-drawable medium))
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
    (send-command window "drawCircle" (jsobj args))))

(defmethod medium-draw-ellipse* ((medium jsws-medium) center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy start-angle end-angle filled)
  (let* ((window (medium-drawable medium))
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
    (send-command window "drawEllipse" (jsobj args))))

(defmethod medium-draw-point* ((medium jsws-medium) x y)
  (let* ((window (medium-drawable medium))
         (ink (medium-ink medium))
         (radius (/ (jsws-stroke-width (medium-line-style medium)) 2))
         (args (list "left" (- x radius)
                     "top" (- y radius)
                     "radius" radius
                     "fill" (jsws-color ink))))
    (send-command window "drawCircle" (jsobj args))))

(defun polypoints (coord-seq)
  (loop for (x y) on (coerce coord-seq 'list) by #'cddr
        collecting (st-json:jso "x" x "y" y)))

(defmethod medium-draw-polygon* ((medium jsws-medium) coord-seq closed filled)
  (let* ((window (medium-drawable medium))
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
    (send-command window command (list points (jsobj options)))))

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
    ((null face) (jsws-font-style (text-style-face *default-text-style*)))
    ((eq :roman face) "normal")
    ((eq :italic face) "italic")
    ((and (consp face) (find :italic face)) "italic")
    (t "normal")))

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
  (let* ((window (medium-drawable medium))
         (ink (medium-ink medium))
         (color (jsws-color ink))
         (str (subseq string start end))
         (style (medium-text-style medium))
         (size (jsws-font-size (text-style-size style)))
         (options (jsws-text-style style)))
    (multiple-value-bind (width height final-x final-y baseline)
        (text-size medium string :text-style style)
      (declare (ignore final-x final-y))
      (let* ((_fontSizeFraction 0.222) ; from Fabric js
             (lineHeight 1.16) ; ditto
             (finagling-factor (- baseline (/ (* height (- 1 _fontSizeFraction)) lineHeight))))
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
      (send-command window "drawText" (list str (jsobj options))))))

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

(defun test ()
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
                       sheet-mute-input-mixin
                       sheet-mute-repainting-mixin
                       climi::updating-output-stream-mixin
                       basic-sheet
                       standard-extended-output-stream
                       extended-input-stream
                       permanent-medium-sheet-output-mixin
                       standard-output-recording-stream)
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
      (let ((stream (make-instance 'test-stream :port port)))
        (sheet-adopt-child (find-graft :port port) stream)
        (funcall continuation stream)))))
