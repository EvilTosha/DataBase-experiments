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
             (:file "journal" :depends-on ("package" "config"))
             (:file "route" :depends-on ("package" "config" "collection" "journal"))
						 (:file "server" :depends-on ("collection" "config" "route" "journal"))))))
