;;;; Rove test suite for point-sampling.
(in-package :point-sampling/test)

(deftest point-sampling
  (let* ((ps-empty) (ps-points) (ps-table) (ps-cpy)
         (p1 (make-point 1 2)) (p2 (make-point 3 4)) (p3 (make-point 5 6))
         (points-vec (make-array 2 :initial-contents (list p1 p2))))
    (testing "Instantiators"
      (setf ps-empty (make-point-sampling-empty)
            ps-points (make-point-sampling-frompoints points-vec)
            ps-table (make-point-sampling-fromtable #(#(1 2) #(3 4)))
            ps-cpy (ps-deepcpy ps-table))
      (pass "OK"))
    (testing "Basic methods"
      (ok (and (= 0 (ps-n ps-empty)) (= 2 (ps-n ps-points))
               (= 2 (ps-n ps-table)) (= 2 (ps-n ps-cpy))) "ps-n")
      (format nil "~&~A~%" ps-points) (pass "printing: pass"))
    (testing "Push/Pop"
      (ok (p-eq (ps-pop ps-table) p2) "ps-pop")
      (ps-push p1 ps-table) (ps-push p1 ps-empty) (ps-push p1 ps-table)
      (ps-push p1 ps-cpy)
      (ok (p-eq (aref (points ps-table) (1- (ps-n ps-table))) p1) "ps-push"))
    (testing "Point access"
      (ok (p-eq p1 (ps-ref ps-cpy 0)) "Reading")
      (setf (ps-ref ps-cpy 0) p3)
      (ok (p-eq p3 (ps-ref ps-cpy 0)) "setf")))
  (let ((ps1 (make-point-sampling-fromtable #(#(1 2) #(3 4))))
        (ps2 (make-point-sampling-fromtable #(#(1 2) #(3 4))))
        (ps3 (make-point-sampling-fromtable #(#(1 2) #(3 4) #(3 4))))
        (ps4 (make-point-sampling-fromtable #(#(1 2) #(3 5)))))
    (testing "Equality predicate"
      (ok (and (ps-eq ps1 ps2) (not (ps-eq ps1 ps3)) (not (ps-eq ps1 ps4)))
          "ps-eq"))))
