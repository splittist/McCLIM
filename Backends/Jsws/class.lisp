;;;; class.lisp

(in-package #:clim-jsws)

(defclass jsws-port (climi::standard-port)
  ((%id :accessor port-id)
   (%client
    :accessor port-client)))

(defclass jsws-pointer (standard-pointer)
  ())

(defmethod port-grab-pointer ((port jsws-port) (pointer jsws-pointer) sheet &key multiple-window)
  nil)

(defmethod port-ungrab-pointer ((port jsws-port) (pointer jsws-pointer) sheet)
  nil)

(defmethod set-sheet-pointer-cursor ((port jsws-port) sheet cursor)
  nil) ; FIXME

(defclass jsws-graft (graft)
  ())

(defun screen-size-from-browser (client)
  (let ((tag (make-tag)))
    (send-command client "screenSize" (list tag))
    (let ((response
            (receive-if #'(lambda (msg) (and (eq :screen-size (first msg))
                                             (eql tag (second msg)))))))
      (nthcdr 2 response))))

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
    (setf *event-thread*
          (clim-sys:make-process
           (lambda ()
             (loop (with-simple-restart
                       (restart-event-loop "Restart CLIM's event loop")
                     (loop (process-next-event port)))))
           :name (format nil "~S's event process" port)))))

(defclass jsws-medium (basic-medium)
  ())

(defmethod make-medium ((port jsws-port) sheet)
  (make-instance 'jsws-medium :port port :sheet sheet))

(defmethod medium-drawable ((medium jsws-medium))
  (mirror-window (slot-value (graft (port medium)) 'climi::mirror))) ;;; FIXME DEBUG
