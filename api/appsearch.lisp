;;; ==================================================================
;;; ==================================================================
;;; Clean App Search - a search engine for energy efficiency data
;;; ==================================================================
;;; ==================================================================

(in-package :cleanapp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(load-config
	    start-server)))

;;; ------------------------------------------------------------------
;;; Global variables

(defvar *global-config* nil
  "Holds the server configuration")
(defvar *psql-connection* nil
  "Holds a list of psql arguments to #'toplevel-connection")
(defvar *acceptor* nil
  "Hunchentoot acceptor")

;;; ------------------------------------------------------------------

(defun load-config (&optional (filename "api/appsearch.json"))
  (let ((config
	 (with-open-file (f filename :direction :input)
	   (with-decoder-simple-clos-semantics
	     (decode-json f)))))
    (setf *global-config* config)
    config))

;;; ------------------------------------------------------------------

(defun refresh-db-connection (&optional (config *global-config*))
  ;; Initialize db connection
  (when *database*
    (disconnect-toplevel))
  (connect-toplevel
   (slot-value config :database-name)
   (slot-value config :database-user)
   (slot-value config :database-password)
   (slot-value config :database-host)))

;;; ------------------------------------------------------------------

(defun start-server (&optional (config *global-config*))
  (when *acceptor*
    (handler-case
	(stop *acceptor*)
      (t () nil)))
  (let ((acceptor (make-instance 'easy-acceptor
				 :address
				 (slot-value config :listen-host)
				 :port
				 (slot-value config :listen-port)
				 :access-log-destination
				 (slot-value config :access)
				 :message-log-destination
				 (slot-value config :error))))
    (setf *acceptor* acceptor)
    (start acceptor)))

(defun stop-server (&optional (acc *acceptor*))
  (stop acc))

;;; ------------------------------------------------------------------
;;; API endpoints

;;; ------------------------------------------------------------------
;;; API endpoints


(define-easy-handler (cleanapp-search :uri "/api/search")
    ((query :request-type :both)
     (filter :request-type :both
	     :parameter-type '(list string)))
  (setf (content-type*) "application/json")
  (let ((success t)
	(columns (list "Type"
		       "Style"
		       "Company"
		       "Model"
		       "Height"
		       "Width"
		       "Depth"
		       "Volume "
		       "kWh/Year"
		       "kWh/Year/ft^3"))
	(products
	 '((45
	    (
	     "Refrigerator"
	     "Top Freezer"
	     "Frigidaire"

	     "LFTR1814L"
	     64
	     30
	     31
	     18.2
	     479
	     26))))
	(*json-output* (make-string-output-stream)))
    (with-object ()
      (encode-object-member :response success)
      (as-object-member (:columns)
	(with-array ()
	  (mapc #'encode-array-member columns)))
      (as-object-member (:products)
	(with-array ()
	  (loop for (id cells) in products
	     do
	       (as-array-member ()
		 (with-object ()
		   (encode-object-member :id id)
		   (as-object-member (:cells)
		     (with-array ()
		       (loop for cell in cells
			  do
			    (encode-array-member cell))))))))))
    (get-output-stream-string *json-output*)))

;;; ------------------------------------------------------------------

(define-easy-handler (cleanapp-product :uri "/api/product")
    ((id :request-type :both :parameter-type 'integer))
  (setf (content-type*) "application/json")
  (encode-json-alist-to-string
   `((:product-id . ,id))))

;;; ------------------------------------------------------------------



;;; ==================================================================
;;; End of file
;;; ==================================================================