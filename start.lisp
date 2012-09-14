(require 'asdf)

;; loaading all required libs
(mapcar #'(lambda (lib-path)
						(push lib-path asdf:*central-registry*))
				;; list of all libs' paths
				(directory "libs/*/"))

;; load database source code
(push #P"./" asdf:*central-registry*)
(asdf:load-system :database)
