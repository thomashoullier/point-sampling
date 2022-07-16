;;;; Export point-sampling to csv file.
(in-package :point-sampling)

(defmethod ps-tocsv-file ((ps point-sampling) filename)
  "Export a point-sampling to a csv text file with the point coordinates.
   Order is kept."
  (with-open-file (str filename :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
    (loop for point across (points ps) do
      (write-string (p-tocsv-str point) str))))
