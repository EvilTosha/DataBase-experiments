;;;; route.lisp

;;;; File contains functions for handling http requests to database

(in-package :shard)

(defun create-json-response (error-flag error-msg data)
  "Forms response to router in JSON format"
  (declare (boolean error-flag) ((or null string) error-msg data))
  (with-output-to-string (*standard-output*)
    (json:encode-plist (list :error error-flag
                             :error-msg (or error-msg "")
                             :data (or data "")))))

(defun handle-db-request (request-json)
  "Handle request from router, return json-encoded response"
  (let* ((parsed-request (json:parse request-json :object-as :hash-table))
         (action (gethash "ACTION" parsed-request))
         (args (gethash "ARGS" parsed-request))
         ;; first arg isn't always key, but for now its true
         (key (car args))
         ;; parameters for response
         error-flag
         error-msg
         data)
    (handler-case
        (prog1
            (setf data
                  (cond
                    ((string= "create" action)
                     (if (not key)
                         (error (error-message 'key-not-specified))
                         (apply #'db-create args)))
                    ((string= "get" action)
                     (if (not key)
                         (error (error-message 'key-not-specified))
                         (print-entry (db-retrieve key))))
                    ((string= "update" action)
                     (if (not key)
                         (error (error-message 'key-not-specified))
                         (apply #'db-update args)))
                    ((string= "remove" action)
                     (if (not key)
                         (error (error-message 'key-not-specified))
                         (db-delete key)))
                    ((string= "flush" action)
                     (db-flush))
                    (t (error (error-message 'unknown-command)))))
          (when (and (master-p *server-info*)
                     (or (string= "create" action)
                         (string= "update" action)
                         (string= "remove" action)))
            (replicate-request request-json :syncronized t)
            ;; store request to journal
            (record-journal-entry request-json)))
      ;; handle occured error
      (error (e)
        (setf error-flag t
              error-msg (simple-condition-format-control e))))
    (create-json-response error-flag error-msg data)))
