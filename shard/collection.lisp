;;;; database.lisp

;;;; Simple database implemenatation (operations: load, CRUD, flush)

(in-package :shard)

(defclass db-entry ()
  ((name  :initform "" :initarg :name  :accessor name)
   (phone :initform "" :initarg :phone :accessor phone))
  (:documentation "Simple entry for database; includes 2 fields - name and phone"))

(defun print-entry (entry)
  "Return text represtntation (human-readable) of database entry"
  ;; if entry is string, it's error message
  (if (stringp entry)
      entry
      ;; else, print entry
      (format nil "Name: ~A; Phone: ~A;"
              (name entry) (phone entry))))

(defmethod json:encode ((object db-entry) &optional stream)
  (json:encode-plist (list :name (name object) :phone (phone object)) stream))

(defvar *database* (make-hash-table :test #'equal)
  "Dynamic (global) variable for storing database entries. Inner structure - hash-table")

;;; CRUD operations

(defun error-message (symbol &rest args)
  "Convert error symbol to corresponding error message (using args)"
  (case symbol
    (key-not-specified "Error: key must be specified")
    (unknown-command "Error: unknown command, try to type 'help'")
    (existing-entry (apply #'format nil "Error: entry ~#[~;with key ~A ~]already exists" args))
    (no-entry-presented
     (apply #'format nil "Error: ~#[no such entry~;entry with key ~A wasn't found~] in database" args))
    (t "Error: unknown error ~A" symbol)))

(defun db-create (key &optional (name "") (phone ""))
  "Create and store new database entry"
  (declare (string key name phone))
  (if (gethash key *database*)
      (error (error-message 'existing-entry key))
      ;; else
      (progn
        ;; initialize and store new entry
        (setf (gethash key *database*)
              (make-instance 'db-entry :name name :phone phone))
        ;; return success message
        (format nil "New entry with key ~A created" key))))

(defun db-retrieve (key)
  "Get a single entry from database by its key"
  (declare (string key))
  (let ((entry (gethash key *database*)))
    ;; check existance
    (if entry
        entry
        ;; else return error
        (error (error-message 'no-entry-presented key)))))


(defun db-update (key &optional (name "") (phone ""))
  "Update entry with new data. If entry isn't found, return error message"
  (declare (string key name phone))
  (let ((entry (gethash key *database*)))
    ;; check existance
    (if entry
        (progn
          ;; update fields
          (setf (name entry) name
                (phone entry) phone)
          ;; return sucess message
          (format nil "Entry with key ~A successfully updated with new data!" key))
        ;; else return error
        (error (error-message 'no-entry-presented key)))))

(defun db-delete (key)
  "Removes single entry from database by key"
  (declare (string key))
  (let ((entry (gethash key *database*)))
    ;; check existance
    (if entry
        (progn
          ;; delete
          (remhash key *database*)
          ;; no need to remove object, garbage-collector will do it
          (format nil "Entry with key ~A was removed from storage" key))
        ;; else return error
        (error (error-message 'no-entry-presented key)))))

;;;; server-state operations (flush, load)

(defun db-flush ()
  "Saves current database state to disk"
  (ensure-directories-exist "data/")
  (with-open-file (file #P"data/snapshot.db"
                        :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
    (json:encode *database* file))
  ;; return
  "Successfully saved data")

(defun db-load (&optional (pathname #P"data/snapshot.db"))
  "Restores saved database stage. Warning: all current data will be overwritten!"
  (declare (pathname pathname))
  (with-open-file (file pathname
                        :direction :input :if-does-not-exist nil)
    (let ((database-temp (json:parse (read-line file))))
      ;; FIXME: database shouldn't reset here
      (setf *database* (make-hash-table :test #'equal))
      (maphash #'(lambda (key value)
                   (setf (gethash key *database*)
                         (make-instance 'db-entry
                                        :name (gethash "NAME" value)
                                        :phone (gethash "PHONE" value))))
               database-temp))))
