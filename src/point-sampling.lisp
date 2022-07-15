;;;; Point-sampling class
(in-package :point-sampling)

(defclass point-sampling ()
  ((points :documentation "Vector of points."
           :accessor points :initarg :points
           :type (vector point))))

;;TODO: export #:points

;;; Instantiators
(defun make-point-sampling-empty ()
  "Create an empty point-sampling instance."
  (make-instance 'point-sampling
                 :points (make-array 0 :fill-pointer 0 :element-type 'point)))

(defun make-point-sampling-frompoints (points)
  "Create a point-sampling instance filled with the vector of points."
  (make-instance 'point-sampling :points points))

(defun make-point-sampling-fromtable (points)
  "Create a point-sampling instance from a vector of points coordinates."
  (let ((points-vec (map 'vector #'make-point-coords points)))
    (make-instance 'point-sampling :points points-vec)))

(defmethod ps-deepcpy ((ps point-sampling))
  "Return a deep copy of the point-sampling instance."
  (make-instance 'point-sampling :points (map 'vector #'p-cpy (points ps))))

;;; Printer
(defmethod print-object ((obj point-sampling) stream)
  "Printer for point-sampling."
  (print-unreadable-object (obj stream :type T)
    (with-slots ((points points)) obj
      (format stream "~A" points))))

;;; Point access
(defmethod ps-ref ((ps point-sampling) index)
  "Access a point in the point-sampling."

  )

;; TODO: try something like (setf (px (aref ps 0)) 1.5d0)

(defmethod (setf ps-ref) ((new-point point) (ps point-sampling))
  "Setf a point in point-sampling."

  )

(defmethod ps-push ((ps point-sampling) new-point)
  "Push a new point to the point-sampling instance."

  )

(defmethod ps-pop ((ps point-sampling))
  "Pop the last point from the point-sampling."

  )

;;; Find and remove duplicates
(defmethod find-duplicates ((ps point-sampling))
  "Find duplicate points."

  )

(defmethod remove-duplicates ((ps point-sampling))
  "Remove duplicate points."

  )

;;; Predicates
(defmethod ps-eq ((ps1 point-sampling) (ps2 point-sampling))
  "Equality predicate between two point-sampling instances."

  )
