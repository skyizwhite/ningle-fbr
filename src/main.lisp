(uiop:define-package :ningle-fbr
  (:nicknames #:ningle-fbr/main)
  (:use #:cl
        #:ningle-fbr/router)
  (:export #:set-routes
           #:list-routes))
(in-package :ningle-fbr)
