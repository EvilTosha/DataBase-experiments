;;;; database.lisp

;;;; Simple database implemenatation (operations: load, CRUD, flush, halt)

(in-package :database)

(defclass db-entry ()
	((name  :initform "" :initarg :name  :accessor name)
	 (phone :initform "" :initarg :phone :accessor phone))
	(:documentation "Simple entry for database; includes 2 fields - name and phone"))

(defvar *database* (make-hash-table :test #'equal)
	"Dynamic (global) variable for storing database entries. Inner structure - hash-table")

;;; CRUD operations

(defun db-create (key &optional (name "") (phone ""))
	"Create and store new database entry"
	(declare (string key name phone))
	(if (gethash key *database*)
			;; return error "Entry already exists"
			"Error: entry already exists"
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
				(format nil "Error: entry with key ~A wasn't found in database" key))))


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
				(format nil "Error: entry with key ~A wasn't found in database" key))))

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
				(format nil "Error: entry with key ~A wasn't found in database" key))))


(defun db-flush ()
	"Saves current database state to disk"
	(ensure-directories-exist "data/")
	(with-open-file (file #P"data/snapshot.db"
												:direction :output :if-exists :supersede
												:if-does-not-exist :create)
		))

