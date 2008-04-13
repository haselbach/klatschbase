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
     (post-message
      "post-message" :post
      ((body :body)))
     (get-message
      "get-message" :get
      ((category :url-arg 1) (id :url-arg 2) (key :url-arg 3)))
     (get-messages
      "get-messages" :get
      ((category :url-arg 1) (id :url-arg 2) (start-key :url-arg 3)))
     (room-list
      "list-rooms" :get
      ())
     (create-room
      "room" :put
      ((name :url-arg 1)))
     (remove-room
      "room" :delete
      ((name :url-arg 1)))))

(defparameter *client-api-string*
  (client-remote-api
   "http://localhost/klatschbase/ops/"
   'klatschbase
   'chat-api))

(defparameter *chat-dispatcher*
  (labels
      ((register (user body)
	 (let ((client (make-instance 'chat-client
				      :name user
				      :server *chat-server*)))
	   (register-client *chat-server* client)))
       (post-message (message)
	 (let* ((username   (cdr (assoc :user    message)))
		(clientname (cdr (assoc :client  message)))
		(roomname   (cdr (assoc :room    message)))
		(msgtext    (cdr (assoc :msgtext message)))
		(user       (get-client-by-id *chat-server* username)))
	   (cond ((not (null clientname))
		  (let ((client (get-client-by-id *chat-server* clientname)))
		    (not (null (client-message user client msgtext)))))
		 ((not (null roomname))
		  (let ((room (get-room-by-id *chat-server* roomname)))
		    (not (null (client-message user room msgtext)))))
		 (t (error "neither client nor room defined")))))
       (get-message (category user key)
	 (let ((client    (get-client-by-id *chat-server* user))
	       (key*      (parse-integer key)))
	   (chat-message-dto (get-chat-message client key*))))
       (get-messages (category user startkey)
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
(setf rest-my-case:*transform-errors-p* nil)

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