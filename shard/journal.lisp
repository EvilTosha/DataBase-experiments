;;;; journal.lisp

;;; File contains functions for add all operations with collection to journal and request entries from journal
;;; All obtained json requests are stored with timestamp, which encoded at additional json level

(in-package :shard)

(defun path-to-journal ()
  "Retruns pathname of journal"
  (pathname (config-get-my-option "journal-path")))


(defun record-journal-entry (request-json &optional (timestamp (get-universal-time)))
  "Writes a single entry to journal"
  (declare (string request-json) (number timestamp))
  (let ((path (path-to-journal)))
  (ensure-directories-exist path)
  (with-open-file (file path
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
    (json:encode-plist (list :time timestamp
                             :request request-json)
                       file))))

(defun get-records-since-timestamp (timestamp)
  "Returns list of records occured after specified timestamp"
  (declare (number timestamp))
  ;; not optimal to read all entries from journal, but for now its Ok
  (let ((path (path-to-journal)))
    (when (probe-file path)
      (with-open-file (file path :direction :input)
        (loop :for line := (read-line file nil 'EOF)
           :for decoded-value := (json:parse line :object-as :hash-table)
           :when (< timestamp (gethash "TIME" decoded-value))
           :collect (gethash "REQUEST" decoded-value))))))
