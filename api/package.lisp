;;; ==================================================================
;;; ==================================================================
;;; Clean App Search - a search engine for energy efficiency data
;;; ==================================================================
;;; ==================================================================

(defpackage :cleanapp
  (:use common-lisp
	hunchentoot
	json
	postmodern
	cl-ppcre
	parse-number
	split-sequence
	flexi-streams))
  