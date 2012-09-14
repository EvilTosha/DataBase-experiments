;;;; parser.lisp

;;;; Simple parser for queries to database

(in-package :database)

(defun split-query-to-tokens (query)
	"Splits given query (which should be string) to tokens by spaces"
	(declare (string query))
	(split-sequence #\Space query :remove-empty-subseqs t))
