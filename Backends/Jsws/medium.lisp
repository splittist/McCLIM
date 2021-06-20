;;;; medium.lisp

(in-package #:clim-jsws)

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

(defun pt>px (pt)
  (/ (* pt 96) 72))

(defun jsws-color (ink)
  (cond
    ((eq ink +flipping-ink+) "rgba(0,0,0,0)") ; FIXME - interim
    (ink
     (multiple-value-bind (r g b a) (color-rgba ink)
       (format nil "rgba(~F%, ~F%, ~F%, ~F)" (* r 100) (* g 100) (* b 100) a)))
    (t
     "none")))

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

(defmethod text-style-width (text-style (medium jsws-medium))
  (text-style-width (merge-text-styles text-style
                                       (medium-merged-text-style medium))
                    medium))

(defmethod text-style-width ((text-style standard-text-style) (medium jsws-medium))
  (get-text-style-metric text-style
                         :width
                         #'(lambda () (first (text-style-base medium text-style)))))

(defmethod text-style-ascent (text-style (medium jsws-medium))
  (text-style-ascent (merge-text-styles text-style
                                       (medium-merged-text-style medium))
                     medium))

(defmethod text-style-ascent ((text-style standard-text-style) (medium jsws-medium))
  (get-text-style-metric text-style
                         :ascent
                         #'(lambda () (second (get-string-size medium "A" text-style)))))

(defmethod text-style-descent (text-style (medium jsws-medium))
  (text-style-descent (merge-text-styles text-style
                                         (medium-merged-text-style medium))
                      medium))

(defmethod text-style-descent ((text-style standard-text-style) (medium jsws-medium))
  (get-text-style-metric text-style
                         :descent
                         #'(lambda () (- (second (get-string-size medium "y" text-style))
                                         (second (get-string-size medium "v" text-style))))))

(defmethod text-style-height (text-style (medium jsws-medium))
  (text-style-height (merge-text-styles text-style
                                       (medium-merged-text-style medium))
                     medium))

(defmethod text-style-height ((text-style standard-text-style) (medium jsws-medium))
  (get-text-style-metric text-style
                         :height
                         #'(lambda () (+ (text-style-ascent text-style medium)
                                         (text-style-descent text-style medium)))))

(defmethod text-size ((medium jsws-medium) string &key text-style (start 0) end)
  (let* ((string (string string))
         (text-style (or text-style (medium-text-style medium)(medium-default-text-style medium))) ; FIXME - really?
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

(defmethod medium-draw-point* ((medium jsws-medium) x y)
  (climi::with-transformed-position ((medium-native-transformation medium) x y)
    (let* ((window (medium-drawable medium))
           (ink (medium-ink medium))
           (radius (/ (jsws-stroke-width (medium-line-style medium)) 2))
           (args (list "x" x
                       "y" y
                       "radius" radius
                       "fill" (jsws-color ink))))
      (send-command window "drawCircle" (jsobj args)))))

(defmethod medium-draw-line* ((medium jsws-medium) x1 y1 x2 y2)
  (let ((tr (medium-native-transformation medium)))
    (climi::with-transformed-position (tr x1 y1)
      (climi::with-transformed-position (tr x2 y2)
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
          (send-command window "drawLine" (list points (jsobj options))))))))

(defmethod medium-draw-rectangle* ((medium jsws-medium) left top right bottom filled)
  (let ((tr (medium-native-transformation medium)))
    (if (rectilinear-transformation-p tr)
        (multiple-value-bind (left top right bottom)
            (transform-rectangle* tr left top right bottom)
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
                     (alexandria:appendf args (list "stroke" color)))
                   (when stroke-width
                     (alexandria:appendf args (list "strokeWidth" stroke-width)))
                   (when stroke-line-join
                     (alexandria:appendf args (list "strokeLineJoin" stroke-line-join)))
                   (when stroke-line-cap
                     (alexandria:appendf args (list "strokeLineCap" stroke-line-cap)))
                   (when stroke-dash-array
                     (alexandria:appendf args (list "strokeDashArray" stroke-dash-array)))))
            (send-command window "drawRect" (jsobj args))))
        (medium-draw-polygon* medium
                              (vector left top right top right bottom left bottom left top)
                              t
                              filled))))

(defmethod medium-draw-circle* ((medium jsws-medium) center-x center-y radius start-angle end-angle filled)
  (let* ((ellipse (make-elliptical-arc* center-x center-y
                                        radius 0
                                        0 radius
                                        :start-angle start-angle
                                        :end-angle end-angle))
         (transformed-ellipse (transform-region (medium-native-transformation medium) ellipse))
         (start-angle (ellipse-start-angle transformed-ellipse))
         (end-angle (ellipse-end-angle transformed-ellipse)))
    (multiple-value-bind (center-x center-y) (ellipse-center-point* transformed-ellipse)
      (let* ((window (medium-drawable medium))
             (ink (medium-ink medium))
             (color (jsws-color ink))
             (args (list "x" center-x
                         "y" center-y
                         "radius" radius
                         "startAngle" start-angle
                         "endAngle" end-angle)))
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
        (send-command window "drawCircle" (jsobj args))))))

(defmethod medium-draw-ellipse* ((medium jsws-medium) center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy start-angle end-angle filled)
  (let* ((ellipse (make-elliptical-arc* center-x center-y
                                        radius-1-dx radius-1-dy
                                        radius-2-dx radius-2-dy
                                        :start-angle start-angle
                                        :end-angle end-angle))
         (transformed-ellipse (transform-region (medium-native-transformation medium) ellipse))
         (start-angle (ellipse-start-angle transformed-ellipse))
         (end-angle (ellipse-end-angle transformed-ellipse)))
    (multiple-value-bind (center-x center-y) (ellipse-center-point* transformed-ellipse)
      (multiple-value-bind (radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
          (ellipse-radii transformed-ellipse)
        (let* ((window (medium-drawable medium))
               (ink (medium-ink medium))
               (color (jsws-color ink))
               (rx (sqrt (+ (expt radius-1-dx 2) (expt radius-1-dy 2))))
               (ry (sqrt (+ (expt radius-2-dx 2) (expt radius-2-dy 2))))
               (args (list "x" center-x
                           "y" center-y
                           "rx" rx
                           "ry" ry
                           "rotation" (atan radius-1-dy radius-1-dx)
                           "startAngle" start-angle
                           "endAngle" end-angle)))
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
          (send-command window "drawEllipse" (jsobj args)))))))

(defun polypoints (coord-seq)
  (loop for (x y) on (coerce coord-seq 'list) by #'cddr
        collecting (st-json:jso "x" x "y" y)))

(defmethod medium-draw-polygon* ((medium jsws-medium) coord-seq closed filled)
  (let ((tr (medium-native-transformation medium)))
    (climi::with-transformed-positions (tr coord-seq)
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
                (alexandria:appendf options (list "stroke" color)))
              (when stroke-width
                (alexandria:appendf options (list "strokeWidth" stroke-width)))
              (when stroke-line-join
                (alexandria:appendf options (list "strokeLineJoin" stroke-line-join)))
              (when stroke-line-cap
                (alexandria:appendf options (list "strokeLineCap" stroke-line-cap)))
              (when stroke-dash-array
                (alexandria:appendf options (list "strokeDashArray" stroke-dash-array)))))
        (send-command window command (list points (jsobj options)))))))

(defmethod medium-draw-text* ((medium jsws-medium) string x y start end align-x align-y toward-x toward-y transform-glyphs)
  (let* ((transformation (medium-device-transformation medium))
         (window (medium-drawable medium))
         (ink (medium-ink medium))
         (color (jsws-color ink))
         (str (subseq string start end))
         (style (medium-text-style medium))
         (options (jsws-text-style style)))
    (multiple-value-bind (width height final-x final-y baseline)
        (text-size medium string :text-style style)
      (declare (ignore height final-x final-y baseline))
      (alexandria:appendf options (list "baseline"
                                        (ecase align-y
                                          (:top "top")
                                          (:baseline "alphabetic")
                                          (:bottom "bottom")
                                          (:center "middle"))))
      (ecase align-x
        (:left nil)
        (:center (decf x (/ width 2)))
        (:right (decf x width)))
      (multiple-value-bind (x y)
          (transform-position transformation x y)
        (alexandria:appendf options (list "x" x
                                          "y" y))))
      (alexandria:appendf options (list "fill" color))
      (send-command window "drawText" (list str (jsobj options)))))
