;;;; master-slave.lisp

;;; File contains operations requied for master-slave replication:
;;; copy operations from master to slave, master rights transfer, operations lo, etc.

(in-package :shard)


(defun get-my-slave ()
  "Returns url of slave corresponding to current server"
  (config-get-server-option (name *server-info*) "addr" nil))
