(require 'asdf)

;; loaading all required dev libs
(mapcar #'(lambda (lib-path)
						(push lib-path asdf:*central-registry*))
				;; list of all libs' paths
				(directory "dev-libs/*/"))

;; load and start swank
(asdf:load-system :swank)
(swank:create-server :dont-close t :port 4005)

;; load system
(load "start.lisp")
