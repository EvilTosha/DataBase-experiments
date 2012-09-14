;;;; database.asd

;;;; ASDF loader for database project

(defsystem :database
	:depends-on (:hunchentoot
							 :cl-fad
							 :split-sequence
							 :yason)
	:components
  ((:module "src"
            :components
            ((:file "packages")
						 (:file "database" :depends-on ("packages"))
						 (:file "parser" :depends-on ("packages"))
						 (:file "server" :depends-on ("parser" "database"))))))
