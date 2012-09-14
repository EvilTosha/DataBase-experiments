(require 'asdf)

;; loaading all required libs
(mapcar #'(lambda (lib-path)
						(push lib-path asdf:*central-registry*))
				;; list of all libs' paths
				(directory "dev-libs/*/"))

(asdf:load-system :swank)

(swank:create-server :dont-close t :port 4005)

(load "start.lisp")
