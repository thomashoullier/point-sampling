;;;; Point-sampling class
(in-package :point-sampling)

(defclass point-sampling ()
  ((points :documentation "Vector of points."
           :accessor points :initarg :points)))

;;TODO: export #:points

;;; Instantiators
(defun make-point-sampling-empty ()
  "Create an empty point-sampling instance."

  )

(defun make-point-sampling-frompoints (points)
  "Create a point-sampling instance filled with the vector of points."

  )

(defun make-point-sampling-fromtable (points)
  "Create a point-sampling instance from a vector of points coordinates."

  )

(defmethod ps-cpy ((ps point-sampling))
  "Return a copy of the point-sampling instance."

  )

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

;;; Predicates
(defmethod ps-eq ((ps1 point-sampling) (ps2 point-sampling))
  "Equality predicate between two point-sampling instances."

  )
