;;;; master-slave.lisp

;;; File contains operations requied for master-slave replication:
;;; copy operations from master to slave, master rights transfer, operations lo, etc.

(in-package :shard)


(defun get-my-slave ()
  "Returns url of slave corresponding to current server"
  (config-get-server-option (name *server-info*) "addr" nil))

(defun get-my-master ()
  "Returns url of master corresponding to current server"
  (config-get-server-option (name *server-info*) "addr" t))

(defun %syncronized-replicate-request (url parameters)
  (ignore-errors ; ignore errors during connection, assuming server's down
    (multiple-value-bind (content code)
        ;; method is always :post as we're only replicationg destructive operations
        (drakma:http-request url :method :post :parameters parameters)
      ;; return whether request was successful
      (and (equal code 200) (not (gethash "ERROR" (json:parse content :object-as :hash-table)))))))


(defun replicate-request (request-json &key syncronized)
  "Sends copy of request to slave, if SYNCRONIZED parameter specified, returns whether
replication has succed"
  (declare (string request-json) (boolean syncronized))
  (let ((url (puri:merge-uris "/query" (get-my-slave)))
        (parameters `(("request" . ,request-json))))
    (if syncronized
        (%syncronized-replicate-request url parameters)
        ;; else, start in different thread and return t
        (progn
          (bt:make-thread #'(lambda () (%syncronized-replicate-request url parameters)))
          t))))
