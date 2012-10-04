;;;; router.asd

(defsystem :router
	:depends-on (:hunchentoot
							 :cl-fad
							 :split-sequence
							 :yason
               #:trivial-http)
	:components
  ((:module "router"
            :components
            ((:file "package")
             (:file "config" :depends-on ("package"))
             (:file "parser" :depends-on ("package"))
						 (:file "server" :depends-on ("parser" "config"))))))
