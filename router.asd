;;;; router.asd

(defsystem :router
	:depends-on (:hunchentoot
							 :cl-fad
							 :split-sequence
							 :yason
               :drakma)
	:components
  ((:module "router"
            :components
            ((:file "package")
             (:file "config" :depends-on ("package"))
             (:file "parser" :depends-on ("package"))
             (:file "route"  :depends-on ("package" "config"))
						 (:file "server" :depends-on ("parser" "config" "route"))))))
