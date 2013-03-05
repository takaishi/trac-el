(require 'json-rpc)

(defcustom *trac-url* "")

(defun trac--request (url method func args)
  (json-rpc:call-method url method func args))

(defun trac-ticket-get (url id)
  (trac--request url "GET" "ticket.get" (list (number-to-string id))))

(defun trac-ticket-query (url query)
  (trac--request url "GET" "ticket.query" (list query)))

(defmacro trac-with-ticket (url id keys &rest body)
  `(let* ((data (trac-ticket-get ,url ,id))
          (ticket (aref (assoc-default 'result data) 3))
          ,@(mapcar (lambda (key)
                      `(,key (assoc-default (quote ,key) ticket)))
                    keys))
     ,@body))


(defun trac-search-ticket (url query)
  (let* ((ids (append (assoc-default 'result (trac-ticket-query url query)) nil)))
    (mapcar (lambda (id)
              (trac-with-ticket url id (summary) summary))
            ids)
    ))
