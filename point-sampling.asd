(defsystem point-sampling
  :name "point-sampling"
  :author "Thomas HOULLIER"
  :depends-on ("point" "alexandria")
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "point-sampling" :depends-on ("package"))
                 (:file "equality" :depends-on ("package"))))))
