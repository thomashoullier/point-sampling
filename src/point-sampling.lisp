;;;; Point-sampling class
(in-package :point-sampling)

(defclass point-sampling ()
  ((points :documentation "Vector of points."
           :accessor points :initarg :points
           :type (vector point))))

(defmethod ps-n ((ps point-sampling))
  "Return the number of points in the sampling"
  (length (points ps)))

;;; Instantiators
(defun make-point-sampling-empty ()
  "Create an empty point-sampling instance."
  (make-instance 'point-sampling
                 :points (make-array 0 :fill-pointer 0 :element-type 'point)))

(defun make-point-sampling-frompoints (points)
  "Create a point-sampling instance filled with the vector of points."
  (make-instance 'point-sampling
                 :points (alexandria:copy-array
                          points :fill-pointer (length points))))

(defun make-point-sampling-fromtable (points)
  "Create a point-sampling instance from a vector of points coordinates."
  (let* ((n (length points))
         (points-vec (map-into (make-array n :fill-pointer n)
                               #'make-point-coords points)))
    (make-instance 'point-sampling :points points-vec)))

(defmethod ps-deepcpy ((ps point-sampling))
  "Return a deep copy of the point-sampling instance."
  (let ((n (ps-n ps)))
    (make-instance 'point-sampling
                   :points (map-into (make-array n :fill-pointer n)
                                     #'p-cpy (points ps)))))

;;; Printer
(defmethod print-object ((obj point-sampling) stream)
  "Printer for point-sampling."
  (print-unreadable-object (obj stream :type T)
    (with-slots ((points points)) obj
      (format stream "~A" points))))

;;; Point access
(defmethod ps-push (new-point (ps point-sampling))
  "Push a new point to the point-sampling instance."
  (vector-push-extend new-point (points ps)))

(defmethod ps-pop ((ps point-sampling))
  "Pop the last point from the point-sampling."
  (vector-pop (points ps)))

(defmethod ps-ref ((ps point-sampling) index)
  "Access a point in the point-sampling."
  (aref (points ps) index))

(defmethod (setf ps-ref) ((new-point point) (ps point-sampling) index)
  "Setf a point in point-sampling."
  (setf (aref (points ps) index) new-point))
