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
     (room-list
      "list-rooms" :get
      ())
     (make-room
      "room" :put
      ((name :url-arg 1)))
     (delete-room
      "room" :delete
      ((name :url-arg 1)))))

(defparameter *client-api-string*
  (client-remote-api
   "http://localhost/klatschbase/ops/"
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
	(t (error "operation denied"))))))

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
       (get-messages (auth category user startkey)
	 (check-access-right `(get-message ,category ,user) auth)
	 (let ((client    (get-client-by-id *chat-server* user))
	       (key*      (parse-integer startkey)))
	   (mapcar #'chat-message-dto
		   (poll-chat-messages client key*))))
       (room-list ()
	 (list-rooms *chat-server*)))
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