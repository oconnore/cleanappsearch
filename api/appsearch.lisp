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
    (stop *acceptor*))
  (let ((acceptor (make-instance 'acceptor
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

;;(define-easy-handler (

;;; ==================================================================
;;; End of file
;;; ==================================================================