;;;; Equality predicate for point-sampling.
(in-package :point-sampling)

;;; Setting up the required hash tables.
(defmethod p-hash ((p point))
  "Hash function for point instances."
  (sxhash (coordinates p)))

(define-custom-hash-table-constructor make-tally-ht
  :test p-eq :hash-function p-hash)

;;; Equality predicate.
(defmethod ps-eq ((ps1 point-sampling) (ps2 point-sampling))
  "Equality predicate between two point-sampling instances."
  ;; We tally the points in two hash tables. Then we check the
  ;; two tables are equal.
  (let ((hs1 (hash-tally ps1)) (hs2 (hash-tally ps2)))
    (equalp hs1 hs2)))

(defmethod hash-tally ((ps point-sampling))
  "Tally points of a point-sampling into a hash-table of counts."
  (let ((hs (make-tally-ht :size (ps-n ps))))
    (with-custom-hash-table
      (loop for point across (points ps) do
        (if (gethash point hs)
            (incf (gethash point hs))
            (setf (gethash point hs) 0))))
    hs))
