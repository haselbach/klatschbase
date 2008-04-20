;;;
;;; Chat Server
;;;
;;; This requires sbcl for threads and networking.

(asdf:oos 'asdf:load-op :hunchentoot)
(asdf:oos 'asdf:load-op :cl-json)
(asdf:oos 'asdf:load-op :cl-ppcre)
(asdf:oos 'asdf:load-op :s-http-client)

(defpackage :klatschbase
  (:use :common-lisp :hunchentoot :rest-my-case))
(in-package :klatschbase)


(defparameter *base-path* "/klatschbase/ops/")
(defparameter *chat-server* (make-instance 'chat-server))

(defspec chat-api
    :export
    ((register
      "client" :put
      ((user :url-arg 1) (body :body)))
     (login
      "login" :get
      ((auth :user)))
     (post-message
      "post-message" :post
      ((user :user) (body :body)))
     (get-message
      "get-message" :get
      ((auth :user) (category :url-arg 1) (id :url-arg 2) (key :url-arg 3)))
     (get-messages
      "get-messages" :get
      ((auth :user)
       (category :url-arg 1) (id :url-arg 2) (start-key :url-arg 3)))
     (get-messages-list
      "get-messages" :post
      ((auth :user) (body :body)))
     (room-list
      "list-rooms" :get
      ())
     (client-list
      "list-clients" :get
      ())
     (room-info
      "room" :get
      ((auth :user) (id :url-arg 1)))
     (client-info
      "client" :get
      ((auth :user) (id :url-arg 1)))
     (make-room
      "room" :put
      ((auth :user) (name :url-arg 1)))
     (delete-room
      "room" :delete
      ((auth :user) (name :url-arg 1)))))

(defparameter *client-api-string*
  (client-remote-api
   "/klatschbase/ops/"
   'klatschbase
   'chat-api))

(defun check-access-right (operation auth)
  (when (null auth)
    (error "authorization required"))
  (let ((user (get-client-by-id *chat-server* (first auth))))
    (when (or (null user) (not (string= (second auth) (client-password user))))
      (error "authentication error ~A" auth))
    (let ((op* (car operation)))
      (cond
	((eq 'post-message op*)
	 t)
	((eq 'login op*)
	 t)
	((eq 'get-message op*)
	 (destructuring-bind (category user) (cdr operation)
	   (if (string= category "client")
	       (string= user (first auth))
	       t)))
	((eq 'make-room operation)
	 t)
	((or (eq 'client-info op*) (eq 'room-info op*))
	 t)
	((eq 'delete-room operation)
	 (not (null (find 'delete-room (allowed-client-operations user)))))
	(t (error "operation denied: ~A" operation))))))

(defparameter *chat-dispatcher*
  (labels
      ((register (user body)
	 (let* ((password (cdr (assoc :password body)))
		(client   (make-instance 'chat-client
					 :name user
					 :password password
					 :server *chat-server*)))
	   (register-client *chat-server* client)))
       (login (auth)
	 (check-access-right '(login) auth)
	 (chat-object-dto (get-client-by-id *chat-server* (first auth))))
       (post-message (auth message)
	 (check-access-right '(post-message) auth)
	 (let* ((clientname (cdr (assoc :client  message)))
		(roomname   (cdr (assoc :room    message)))
		(msgtext    (cdr (assoc :msgtext message)))
		(user       (get-client-by-id *chat-server* (elt auth 0))))
	   (cond ((not (null clientname))
		  (let ((client (get-client-by-id *chat-server* clientname)))
		    (not (null (client-message user client msgtext)))))
		 ((not (null roomname))
		  (let ((room (get-room-by-id *chat-server* roomname)))
		    (not (null (client-message user room msgtext)))))
		 (t (error "neither client nor room defined")))))
       (get-message (auth category user key)
	 (check-access-right `(get-message ,category ,user) auth)
	 (let ((client    (get-client-by-id *chat-server* user))
	       (key*      (parse-integer key)))
	   (chat-message-dto (get-chat-message client key*))))
       (get-messages (auth category id startkey)
	 (check-access-right `(get-message ,category ,id) auth)
	 (let ((chat-obj  (cond
			    ((string= "client" category)
			     (get-client-by-id *chat-server* id))
			    ((string= "room" category)
			     (get-room-by-id *chat-server* id))
			    (t
			     (error "unknown category"))))
	       (key*      (if (stringp startkey)
			      (parse-integer startkey)
			      startkey)))
	   (mapcar #'chat-message-dto
		   (poll-chat-messages chat-obj key*))))
       (get-messages-list (auth body)
	 (mapcar (lambda (x)
		   (let ((category (cdr (assoc :category x)))
			 (name     (or (cdr (assoc :name x))
				       (cdr (assoc :id x))))
			 (startkey (cdr (assoc :startkey x))))
		     `((key      . ((category . ,category)
				    (name     . ,name)
				    (startkey . ,startkey)))
		       (messages . ,(get-messages auth category name
						  startkey)))))
		 body))
       (client-info (auth id)
	 (check-access-right '(client-info) auth)
	 (chat-object-dto (get-client-by-id *chat-server* id)))
       (room-info (auth id)
	 (check-access-right '(room-info) auth)
	 (chat-object-dto (get-room-by-id *chat-server* id)))
       (room-list ()
	 (list-rooms *chat-server*))
       (client-list ()
	 (list-clients *chat-server*))
       (make-room (auth name)
	 (check-access-right `(make-room) auth)
	 (create-room *chat-server* name))
       (delete-room (auth name)
	 (check-access-right `(delete-room) auth)
	 (remove-room *chat-server* name)))
    (remote-api *base-path* chat-api)))

(push *chat-dispatcher* *dispatch-table*)

(push (create-prefix-dispatcher "/klatschbase/client.js"
				(lambda ()
				  (setf (content-type) "text/javascript")
				  *client-api-string*))
      *dispatch-table*)

(pop *dispatch-table*)

(setf *catch-errors-p* nil)
(setf rest-my-case:*transform-errors-p* t)

(defparameter *hunchentoot-server* (start-server :port 4242))

(stop-server *hunchentoot-server*)

(register-client *chat-server*
		 (make-instance 'chat-client :name "fo"
					     :server *chat-server*))

(mapcar
 #'chat-message-dto
 (poll-chat-messages (get-client-by-id *chat-server* "foo") 0))

(unregister-client (get-client-by-name *chat-server* "ajax") )

(msg-to-client (get-client-by-name *chat-server* "foo") "mehr davon")

(create-room *chat-server* "foo")
(create-room *chat-server* "bar")
