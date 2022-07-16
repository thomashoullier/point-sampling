(defpackage :point-sampling
  (:use :cl :point)
  (:export #:point-sampling #:points
           #:ps-n
           #:make-point-sampling-empty #:make-point-sampling-frompoints
           #:make-point-sampling-fromtable #:ps-deepcpy
           #:ps-push #:ps-pop #:ps-ref))
