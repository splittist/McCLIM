;;;; server.lisp

(in-package #:clim-jsws)

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

(defvar *main-thread* nil)
(defvar *event-thread* nil)

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
  #+(or)(format t "~&Received '~A' from ~A~%" message client)
  (let ((form (read-from-string message)))
    (if (eq :event (first form))
        (send *event-thread* form)
        (send *main-thread* form))))

(defun start-server ()
  (pushnew 'find-socket hunchensocket:*websocket-dispatch-table*)
  (hunchentoot:start *server*)
  (setf *main-thread* (bt:current-thread)))

(defun stop-server ()
  (hunchentoot:stop *server*))

(defmethod st-json::write-json-element ((element real) stream)
  (format stream "~,6,,,,,'eE" element)) ; limit decimal places

(defun jsobj (args)
  (apply #'st-json:jso args))

(defclass jsws-window ()
  ((%client
    :initarg :client
    :reader window-client)
   (%id
    :initarg :id
    :reader window-id)))

(defun make-window (client id &optional canvasp)
  (when canvasp
    (send-command client "createWindow" (list (princ-to-string id) "canvas")))
  (make-instance 'jsws-window :client client :id id))

(defgeneric send-command (destination command args)
  (:method ((window jsws-window) command args)
    (let ((client (window-client window))
          (msg (st-json:write-json-to-string
                (list
                 (list command
                       (list (princ-to-string (window-id window))
                             args))))))
      #+(or)(print msg) ; DEBUG
      (hunchensocket:send-text-message client msg)))
  (:method ((client client) command args)
    (let ((msg (st-json:write-json-to-string (list (list command args)))))
      #+(or)(print msg) ; DEBUG
      (hunchensocket:send-text-message client msg))))

(defun jsws-client (thing)
  (etypecase thing
    (client thing)))
