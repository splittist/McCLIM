(defsystem "mcclim-jsws"
  :depends-on ("clim-basic"
               "mcclim-backend-common"

               "hunchensocket"
               "st-json")
  :serial t
  :components ((:file "package")
               (:file "server")
               (:file "class")
               (:file "medium")
               (:file "input")
               (:file "jsws")
               (:static-file "index.html")))

