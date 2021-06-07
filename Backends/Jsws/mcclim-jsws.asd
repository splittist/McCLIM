(defsystem "mcclim-jsws"
  :depends-on ("clim-basic"
               "mcclim-backend-common"

               "hunchensocket"
               "st-json")
  :serial t
  :components ((:file "jsws")
               (:static-file "index.html"))

