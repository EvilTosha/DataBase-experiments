;;;; parser.lisp

;;;; Simple parser for queries to database

(in-package :router)

(defun split-query-to-tokens (query)
  "Splits given query (which should be string) to tokens by spaces"
  (declare (string query))
  (split-sequence #\Space query :remove-empty-subseqs t))


(defun parse-shard-response (response-json)
  "Parses response and return multiple values:
error flag, error message, data."
  (declare (string response-json))
  (let ((parsed-value (json:parse response-json)))
    (values (gethash "error" parsed-value)
            (gethash "error-msg" parsed-value)
            (gethash "data" parsed-value))))
