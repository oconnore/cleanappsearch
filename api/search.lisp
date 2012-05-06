;;; ==================================================================
;;; ==================================================================
;;; Clean App Search - a search engine for energy efficiency data
;;; ==================================================================
;;; ==================================================================

(in-package :cleanapp)

;;; ------------------------------------------------------------------

(defparameter *number-regex* "[0-9]+(\\.[0-9]+)?")
(defparameter *filter-regex*
  (format nil "^([a-z]+):(([a-z]+)|((~A)~0@*(-(~A)~0@*)?))$" number-regex))
(defparameter *filter-scanner*
  (create-scanner *filter-regex*
		  :case-insensitive-mode t))

;;; ------------------------------------------------------------------

(defun parse-filter (filter)
  (register-groups-bind
      (name value nil nil first nil nil second)
      (*filter-scanner* filter)
    (cond
      ((not first) (list name :string value))
      ((not second) (list name :number (parse-num first))
      (t (list name :range first second)))))

;;; ------------------------------------------------------------------

;(defun search-products (query filters)
;  (let (

;;; ==================================================================
;;; End of file
;;; ==================================================================