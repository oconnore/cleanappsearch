;;; ==================================================================
;;; ==================================================================
;;; Clean App Search - a search engine for energy efficiency data
;;; ==================================================================
;;; ==================================================================

(in-package :cleanapp)

;;; ------------------------------------------------------------------

(defparameter *number-regex* "[0-9]+(\\.[0-9]+)?")
(defparameter *filter-regex*
  (format nil "^([a-z]+):(([a-z]+)|((~A)~0@*(-(~A)~0@*)?))$" *number-regex*))
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
      ((not second) (list name :number (parse-number first)))
      (t (list name :range (parse-number first) (parse-number second))))))

;;; ------------------------------------------------------------------

(defparameter *model-regex*
  (create-scanner "[0-9a-z]+"
		  :case-insensitive-mode t))

(defun collapse-model-number (x)
  (string-downcase
   (apply #'concatenate
	  (cons 'string
		(all-matches-as-strings *model-regex* x)))))

;;; ------------------------------------------------------------------

(defun find-model (model filters)
  (declare (ignore filters))
  (query (:select '* :from 'products
		  :where (:like 'model (concatenate 'string model "%")))
	 :alists))

;;; ------------------------------------------------------------------

(defun find-company (model)
  (let ((res (query (:select '* :from 'companies
			     :left-join 'products :on (:= 'products.company_id
							  'companies.company_id)
			     :where (:= (collapse-model-number model) 'products.model))
			     :alist)))
    (cdr (assoc :company-name res))))

;;; ------------------------------------------------------------------

(defun find-specs (product-id &optional cols)
  (let ((res (query (:select '* :from 'specifications
			     :left-join 'metrics :on (:= 'specifications.metric_id
							 'metrics.metric_id)
			     :where (:= product-id
					'specifications.product_id))
		    :alist))
	(specs (make-hash-table :test 'equal)))
    (loop for spec in res
       do
	 (let* ((type (cdr (assoc :type spec)))
		(form (cdr (assoc :format spec)))
		(metric-name (cdr (assoc :name spec)))
		(value (cdr (assoc
			     (cond ((string= type "float") :float-value)
				   ((string= type "int") :int-value)
				   (t :string-value))
			     res))))
	   (setf (gethash metric-name cols) t)
	   (setf (gethash metric-name specs) (format nil form value)))
	 (values specs cols))))

;;; ------------------------------------------------------------------

(defun compose-table-rows (model filters)
  (let* ((model-data (find-model model filters))
	 (cols (make-hash-table :test 'equal))
	 (rows (list))
	 (col-list (list "Company" "Model")))
    (loop for product in model-data	 
       do
	 (handler-case
	     (let ((specs (find-specs (cdr (assoc :product-id product))
				      cols))
		   (row (list)))
	       (push specs row)
	       (push (cdr (assoc :model product)) row)
	       (push (find-company (cdr (assoc :model product))) row)
	       (push row rows))
	   (t () nil)))
    (let ((keys (list)))
      (maphash (lambda (k v) (push keys k)) cols)
      (setf keys (sort keys #'string<))
      (setf col-list (append col-list keys)))
    (let ((real-rows (list)))
      (loop for row in rows
	 do
	   (let ((tmp-row (reverse (subseq row 0 2))))
	     (loop for key in (subseq col-list 2)
		do
		  (push (gethash key (elt row 2)) tmp-row))
	     (push (reverse tmp-row) real-rows)))
      (values real-rows col-list))))

;;; ------------------------------------------------------------------

(defun search-products (query filters)
  

;;; ==================================================================
;;; End of file
;;; ==================================================================