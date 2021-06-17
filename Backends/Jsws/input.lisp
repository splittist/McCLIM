;;;; input.lisp

(in-package #:clim-jsws)

(defmethod process-next-event ((port jsws-port) &key wait-function (timeout nil))
  (when (climi::maybe-funcall wait-function)
    (return-from process-next-event
      (values nil :wait-function)))
  (let ((event (get-event port timeout)))
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

(defparameter *ambidextrous-names*
  '(:alt :control :meta :shift :hyper :super))

(defun event-property (event key)
  (getf (cddr event) key))

(defun process-name (event)
  (let* ((name (event-property event :char))
         (loc (event-property event :loc)) ; 0 standard 1 left 2 right 3 numpad
         (mod (event-property event :mod)))
    (if (= 1 (length name))
        (let ((char (character name)))
          (values char
                  (intern name :keyword)
                  (if (upper-case-p char)
                      (logandc2 mod +shift-key+)
                      mod)))
        (let ((base-name
                (second
                 (assoc name *character-name-translations* :test #'string=))))
          (cond
            ((and (member base-name *ambidextrous-names*)
                  (<= 1 loc 2))
             (values nil
                     (alexandria:format-symbol :keyword "~A-~A"
                                               base-name
                                               (ecase loc
                                                 (1 :left)
                                                 (2 :right)))
                     mod))
            ((= 3 loc)
             (values nil
                     (alexandria:format-symbol :keyword "KP-~A" name)
                     mod))
            ((null base-name)
             (values nil (intern (string-upcase name) :keyword) mod)) ; unknown; ignore modern mode
            (t
             (values nil base-name mod)))))))

(defun get-event (port wait-function &optional (timeout nil))
  (flet ((eventp (item) (eq :event (car item))))
    (let ((event (receive-if #'eventp timeout)))
      (case (second event)
        ((:key-press-event :key-release-event)
         (multiple-value-bind (character name mod)
             (process-name event)
           (make-instance (if (eq :key-press-event (cadr event))
                              'key-press-event
                              'key-release-event)
                          :key-character character
                          :key-name name
                          :modifier-state mod
                          :x (event-property event :x)
                          :y (event-property event :y)
                          :sheet (port-keyboard-input-focus port))))
        ((:pointer-button-press-event :pointer-button-release-event)
         (make-instance (if (eq :pointer-button-press-event (cadr event))
                            'pointer-button-press-event
                            'pointer-button-release-event)
                        :button (event-property event :but)
                        :graft-x (event-property event :x)
                        :graft-y (event-property event :y)
                        :pointer (port-pointer port)
                        :modifier-state (event-property event :mod)
                        :sheet (graft port))) ; FIXME
        ((:pointer-enter-event :pointer-exit-event)
         (make-instance (if (eq :pointer-enter-event (cadr event))
                            'pointer-enter-event
                            'pointer-exit-event)
                        :button (event-property event :but)
                        :graft-x (event-property event :x)
                        :graft-y (event-property event :y)
                        :pointer (port-pointer port)
                        :modifier-state (event-property event :mod)
                        :sheet (graft port))) ; FIXME
        (:pointer-motion-event
         (make-instance 'pointer-motion-event
                        :button (event-property event :but)
                        :graft-x (event-property event :x)
                        :graft-y (event-property event :y)
                        :pointer (port-pointer port)
                        :modifier-state (event-property event :mod)
                        :sheet (graft port))) ; FIXME - all the same, too
        ((t)
         (climi::maybe-funcall wait-function))))))
