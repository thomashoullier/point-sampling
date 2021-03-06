(defsystem point-sampling
  :name "point-sampling"
  :author "Thomas HOULLIER"
  :depends-on ("point" "alexandria" "cl-custom-hash-table")
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "point-sampling" :depends-on ("package"))
                 (:file "equality" :depends-on ("point-sampling"))
                 (:file "tocsv-file" :depends-on ("point-sampling")))))
  :in-order-to ((test-op (test-op "point-sampling/test"))))

(defsystem point-sampling/test
  :name "point-sampling/test"
  :depends-on ("point-sampling" "rove" "cl-fad")
  :components
  ((:module "test"
    :components ((:file "package")
                 (:file "rove-suite" :depends-on ("package")))))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
