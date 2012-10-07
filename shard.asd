;;;; shard.asd

;;;; ASDF systems for database project

(defsystem :shard
	:depends-on (:hunchentoot
							 :cl-fad
							 :split-sequence
							 :yason
               :drakma)
	:components
  ((:module "shard"
            :components
            ((:file "package")
						 (:file "collection" :depends-on ("package"))
             (:file "config" :depends-on ("package"))
						 (:file "server" :depends-on ("collection" "config"))))))
