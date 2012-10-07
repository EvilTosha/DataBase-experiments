;;;; route.lisp

(in-package :router)

(defparameter *bucket-ends* (sort (config-get-buckets) #'<)
  "Stores plist with bucket ends as keys and server names as values. Needed for
\"ring\" consistent hashing")


(defun handle-db-request (action args)
  "Handle request from client, return text representation of response"
  (declare (string action) (list args))
  (let ((key (car args)))
    (cond
      ((string= "help" action) (help-message))
      ((string= "create" action)
       (if (not key)
           (error-message 'key-not-specified)
           (shard-request key (make-request-json "create" args) t)))
      ((string= "get" action)
       (if (not key)
           (error-message 'key-not-specified)
           (shard-request key (make-request-json "retrieve" args) nil)))
      ((string= "update" action)
       (if (not key)
           (error-message 'key-not-specified)
           (shard-request key (make-request-json "update" args) t)))
      ((string= "remove" action)
       (if (not key)
           (error-message 'key-not-specified)
           (shard-request key (make-request-json "delete" args) t)))
      (t (error-message 'unknown-command))))

(defun make-request-json (command args)
  "Creates JSON representation of request to shard"
  (declare (string command) (list args))
  (with-output-to-string (*standard-output*)
    (json:encode-plist (list :command command :args args))))

(defun get-shard-by-key (key)
  "Returns name of server where specified key is stored"
  (declare (string key))
  (let ((in-ring-hash
         ;; (nth-value 2 (floor a b)) is similar to (a % b) in C++
         (nth-value 1 (floor (sxhash key) (apply #'max *bucket-ends*)))))
    (alexandria:doplist (end server-name *bucket-ends*)
      (when (> end in-ring-hash)
        ;; yes, its return in lisp. But here its the easiest way to finish
        (return server-name)))))

(defun shard-request (key request-json &optional (destructive t))
  "Make request to shard, using given request json representation.
Selects shard according to hash of key. Destructive flag determines whether command
should be addressed to master"
  (declare (string key request-json) (boolean destructive))
  (let* (result
         (shard-name (get-shard-by-hash key))
         (master-url (puri:merge-uris "/query" (config-get-server-option shard-name "addr" t)))
         (slave-url (puri:merge-uris "/query" (config-get-server-option shard-name "addr" nil)))
         (parameters (list (cons "request" request-json))))
    (if destructive
        (progn
          ;; make post request to master
          (setf result (%shard-request master-url parameters :post))
          ;; if no content received, request slave
          ;; TODO: ask for or assign master permissions before adressing request to slave
          (unless result
            (setf result (%shard-request slave-url parameters :post))))
        ;; else, non-destructive operations
        ;; make get request to slave
        (setf result (%shard-request slave-url parameters :get)))
    (if result
        (render-shard-response result)
        ;; else something went wrong and we assume servers are down
        (error-message 'server-down))))

(defun %shard-request (url parameters &optional (method :get))
  "Do request to shard and returns received content, if some error occurs returns nil"
  (handler-case ; handle errors during connection
      (multiple-value-bind (content code)
          (drakma:http-request url :method method :parameters parameters)
        (when (equal code 200) ; OK
          content))
    ;; process error, assuming server down
    (error () nil)))

(defun render-shard-response (response)
  "Renders answer from shard to text representation for user"
  (declare (string response))
  (let ((parsed-response (json:parse response :object-as :hash-table)))
    (if (gethash "error" parsed-response)
        (gethash "error-msg" parsed-response)
        (gethash "data" parsed-response))))

(defun error-message (symbol &rest args)
  "Convert error symbol to corresponding error message (using args)"
  (case symbol
    (key-not-specified "Error: key must be specified")
    (unknown-command "Error: unknown command, try to type 'help'")
    (server-down "Error: server containing requested information is currently unavailable")))

(defun help-message ()
  "Available commands:
help - show this message;
create KEY [NAME] [PHONE] - create entry in database with specified KEY;
get KEY - retrieve entry with specified KEY;
update KEY [NAME] [PHONE] - update enry information;
remove KEY - remove entry with specified key from database;
flush - save current database state to disk;

Remember - if you specify more args than needed, all extra args will be ignored")
