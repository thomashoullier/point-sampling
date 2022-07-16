(defpackage :point-sampling
  (:use :cl :point :cl-custom-hash-table)
  (:export #:point-sampling #:points
           #:ps-n
           #:make-point-sampling-empty #:make-point-sampling-frompoints
           #:make-point-sampling-fromtable #:ps-deepcpy
           #:ps-push #:ps-pop #:ps-ref
           #:ps-eq
           #:ps-tocsv-file))
