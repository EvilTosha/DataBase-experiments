(in-package :shard)

(defparameter *config* nil
  "Stores config options (as property list) such as urls and ports parsed from config.json file")

(defun config-load (&optional (conf-path #P"config.json"))
  "Load data from config file to *config* variable"
  (declare (pathname conf-path))
  ;; parse file contents
  (with-open-file (file conf-path :direction :input)
    (let* ((len (file-length file))
           (conf-string (make-string len)))
      (read-sequence conf-string file)
      (setf *config* (json:parse conf-string :object-as :hash-table)))))

(defun config-get-server-option (server-name option &optional (master nil master-specified-p))
  "By given server name, option and master-specifier returns value of this option in config;
If no such server (or server part) or option found, signals error"
  (declare (string server-name option) (boolean master-p))
  (let* ((server-options (gethash server-name *config*))
         (part-options (if master-specified-p
                           (gethash (if master "master" "slave") server-options)
                           ;; else, router options
                           server-options)))
    (if part-options
        (multiple-value-bind (val val-specified-p) (gethash option part-options)
          (if val-specified-p
              val
              (error "No option ~A specified for server ~A" option server-name)))
        ;; else, no such server in config, signal error
        (error "No options for server (or part) ~A specified" server-name))))

(defun config-get-my-option (option)
  "Get option from config for current server.
Note: for correct work, *server-info* variable should be correctly set up"
  (declare (string option))
  (config-get-server-option (name *server-info*) option (master-p *server-info*)))
