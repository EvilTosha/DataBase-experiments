(require 'asdf)

;; parsing specified args from sh start script
(defparameter *dev-server-p* (sb-unix::posix-getenv "dev")
  "Specifies whether server should load dev libs and start swank server")
(defparameter *db-action* (sb-unix::posix-getenv "action")
  "Specifies which action should accomplish start script (start-shard, start-router, etc)")
(defparameter *server-name* (sb-unix::posix-getenv "servername")
  "Specifies server name, which is used as name of screen, and also used for searching options in config file")
(defparameter *swank-port* (parse-integer (sb-unix::posix-getenv "swankport"))
  "Specifies port to start swank on, if dev flag specified; if no port given, use 4005")

;; loaading all required libs
(mapcar #'(lambda (lib-path)
            (push lib-path asdf:*central-registry*))
        ;; list of all libs' paths
        ;; when dev param specified, add dev libs to list
        (if *dev-server-p*
            (append (directory "libs/*/")
                    (directory "dev-libs/*/"))
            ;; else
            (directory "libs/*/")))

;; load dev libs
(when *dev-server-p*
  (asdf:load-system :swank))

;; start swank
(when (and *dev-server-p* *swank-port*
  (swank:create-server :port *swank-port* :dont-close t)))

;; load hunchentoot the webserver
(asdf:load-system :hunchentoot)

(push #P"./" asdf:*central-registry*)

;; load specified system
(cond
  ((string= "start-router" *db-action*)
   (asdf:load-system :router))
  ((string= "start-shard" *db-action*)
   (asdf:load-system :shard))
  (t (error "Unknown action ~A" *db-action*)))

(defun start-server (package)
  (funcall (intern (symbol-name 'config-load) package))
  (funcall (intern (symbol-name 'start-server) package)
           *server-name* :dev *dev-server-p*))

;; accomplish on-start tasks
(cond
  ((string= "start-router" *db-action*)
   (start-server (find-package :router)))
  ((string= "start-shard" *db-action*)
   (let ((package (find-package :shard)))
     (start-server package)
     (funcall (intern (symbol-name 'db-load) package))))
  (t (error "Unknown action ~A" *db-action*)))
