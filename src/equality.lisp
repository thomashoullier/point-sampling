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
  (let ((hs1 (hash-tally ps1)) (hs2 (hash-tally ps2))
        (tmp-count 0))
    ;; TODO: equalp can be used on hash-tables.
    (with-custom-hash-table
      (and (= (hash-table-count hs1) (hash-table-count hs2))
           (loop for key1 being the hash-keys of hs1 using (hash-value val1) do
             (if (setf tmp-count (gethash key1 hs2))
                 (when (/= tmp-count val1) (return-from ps-eq nil))
                 (return-from ps-eq nil))
                 finally (return T))))))

(defmethod hash-tally ((ps point-sampling))
  "Tally points of a point-sampling into a hash-table of counts."
  (let ((hs (make-tally-ht :size (ps-n ps))))
    (with-custom-hash-table
      (loop for point across (points ps) do
        (if (gethash point hs)
            (incf (gethash point hs))
            (setf (gethash point hs) 0))))
    hs))
