(require 'asdf)

;; parsing specified args from sh start script
(defparameter *dev-server-p* (sb-unix::posix-getenv "dev")
  "Specifies whether server should load dev libs and start swank server")
(defparameter *db-action* (sb-unix::posix-getenv "action")
  "Specifies which action should accomplish start script (start-shard, start-router, etc)")
(defparameter *server-name* (sb-unix::posix-getenv "servername")
  "Specifies server name, which is used as name of screen, and also used for searching options in config file")
(defparameter *master-p* (when (sb-unix::posix-getenv "masterspecifier")
                           (string= "master" (sb-unix::posix-getenv "masterspecifier")))
  "Must be \"master\" or \"slave\" (used only for shard). Specifies whether master or slave part of shard should be started.")
(defparameter *swank-port* (let ((port-str (sb-unix::posix-getenv "swankport")))
                             (when (and port-str (string/= port-str ""))
                               (parse-integer (sb-unix::posix-getenv "swankport"))))
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
(when (and *dev-server-p* *swank-port*)
  (swank:create-server :port *swank-port* :dont-close t))

(push #P"./" asdf:*central-registry*)

;; load specified system
(cond
  ((string= "start-router" *db-action*)
   (asdf:load-system :router))
  ((string= "start-shard" *db-action*)
   (asdf:load-system :shard))
  (t (error "Unknown action ~A" *db-action*)))

;; load hunchentoot the webserver
(asdf:load-system :hunchentoot)

;; accomplish on-start tasks
(cond
  ((string= "start-router" *db-action*)
   (let ((package (find-package :router)))
     (funcall (intern (symbol-name 'config-load) package))
     (funcall (intern (symbol-name 'start-server) package) *server-name*)))
  ((string= "start-shard" *db-action*)
   (let ((package (find-package :shard)))
     (funcall (intern (symbol-name 'config-load) package))
     (funcall (intern (symbol-name 'start-server) package) *server-name* *master-p*)
     (funcall (intern (symbol-name 'db-load) package))))
  (t (error "Unknown action ~A" *db-action*)))
