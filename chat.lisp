;;;
;;; Chat Server
;;;
;;; This requires sbcl for threads and networking.

(in-package :klatschbase)

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
     (get-messages-wait
      "get-messages-wait" :get
      ((auth :user)
       (category :url-arg 1) (id :url-arg 2) (start-key :url-arg 3)))
     (get-messages-list-wait
      "get-messages-list-wait" :get
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

(defun create-client-js-dispatcher (base-path)
  (let ((client-api-string (client-remote-api (concatenate 'string
							   base-path "ops/")
					      'klatschbase 'chat-api)))
    (create-prefix-dispatcher
     (concatenate 'string base-path "client.js")
     (lambda ()
       (setf (content-type) "text/javascript")
       client-api-string))))

(defun check-server-access-right (chat-server operation auth)
  (when (not (listp auth))
    (error "authorization required"))
  (let ((user (authenticate-user chat-server (first auth) (second auth))))
    (when (null user)
      (error "authentication error ~A" (first auth)))
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
	((eq 'make-room op*)
	 t)
	((or (eq 'client-info op*) (eq 'room-info op*))
	 t)
	((eq 'delete-room op*)
	 (not (null (find 'delete-room (allowed-client-operations user)))))
	(t (error "operation denied: ~A" operation))))))


(defun create-chat-dispatcher (base-path chat-server)
  (labels
      ((check-access-right (operation auth)
	 (check-server-access-right chat-server operation auth))
       (register (user body)
	 (let* ((password (cdr (assoc :password body)))
		(pass     (sha256 (concatenate 'string password user)))
		(client   (make-instance 'chat-client
					 :name user
					 :password pass
					 :password-type (cons :sha256 user)
					 :server chat-server)))
	   (register-client chat-server client)))
       (get-chat-object (category id)
         (cond
           ((string= "client" category)
            (get-client-by-id chat-server id))
           ((string= "room" category)
            (get-room-by-id chat-server id))
           (t
            (error "unknown category"))))
       (parse-key (startkey)
         (if (stringp startkey)
             (parse-integer startkey)
             startkey))
       (login (auth)
	 (check-access-right '(login) auth)
	 (chat-object-dto (get-client-by-id chat-server (first auth))))
       (post-message (auth message)
	 (check-access-right '(post-message) auth)
	 (let* ((clientname (cdr (assoc :client  message)))
		(roomname   (cdr (assoc :room    message)))
		(msgtext    (cdr (assoc :msgtext message)))
		(user       (get-client-by-id chat-server (elt auth 0))))
	   (cond ((not (null clientname))
		  (let ((client (get-client-by-id chat-server clientname)))
		    (not (null (client-message user client msgtext)))))
		 ((not (null roomname))
		  (let ((room (get-room-by-id chat-server roomname)))
		    (not (null (client-message user room msgtext)))))
		 (t (error "neither client nor room defined")))))
       (get-message (auth category user key)
	 (check-access-right `(get-message ,category ,user) auth)
	 (let ((client    (get-client-by-id chat-server user))
	       (key*      (parse-key key)))
	   (chat-message-dto (get-chat-message client key*))))
       (get-messages (auth category id startkey)
	 (check-access-right `(get-message ,category ,id) auth)
	 (let ((chat-obj (get-chat-object category id))
	       (key*     (parse-key startkey)))
	   (mapcar #'chat-message-dto
		   (poll-chat-messages chat-obj key*))))
       (get-messages-wait (auth category id startkey)
         (check-access-right `(get-message ,category ,id) auth)
         (let ((chat-obj (get-chat-object category id))
               (key*     (parse-key startkey)))
           (mapcar #'chat-message-dto
                   (poll-chat-messages-wait chat-obj key* 120))))
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
       (get-messages-list-wait (auth body)
         (let ((co-list
                (mapcar
                 (lambda (x)
                   (let ((category (cdr (assoc :category x)))
                         (name     (or (cdr (assoc :name x))
                                       (cdr (assoc :id x))))
                         (startkey (cdr (assoc :startkey x))))
                     (check-access-right `(get-message ,category ,name) auth)
                     (list (get-chat-object category name)
                           (parse-key startkey))))
 		 body)))
           (mapcar #'chat-message-dto
                   (poll-chat-messages-list-wait co-list 120))))
       (client-info (auth id)
	 (check-access-right '(client-info) auth)
	 (chat-object-dto (get-client-by-id chat-server id)))
       (room-info (auth id)
	 (check-access-right '(room-info) auth)
	 (chat-object-dto (get-room-by-id chat-server id)))
       (room-list ()
	 (list-rooms chat-server))
       (client-list ()
	 (list-clients chat-server))
       (make-room (auth name)
	 (check-access-right `(make-room) auth)
	 (create-room chat-server name))
       (delete-room (auth name)
	 (check-access-right `(delete-room) auth)
	 (remove-room chat-server name)))
    (remote-api (concatenate 'string base-path "ops/") chat-api)))

(defun obtain-dispatch-table (server)
  (let ((dispatch-table (server-dispatch-table server)))
    (if (null dispatch-table)
	(funcall *meta-dispatcher* server)
	dispatch-table)))

(defun start-service (&optional &key
		      hunchentoot-server
		      chat-server
		      (base-path "/klatschbase/")
		      static-files-path)
  (let* ((server         (if (null hunchentoot-server)
			      (start-server :address "127.0.0.1" :port 4242)
			      hunchentoot-server))
	 (chat-server*   (if (null chat-server)
			     (make-instance 'chat-server)
			     chat-server))
	 (dispatch-table (obtain-dispatch-table server)))
    (unless (null static-files-path)
      (push (create-folder-dispatcher-and-handler base-path static-files-path)
	    dispatch-table))
    (push (create-client-js-dispatcher base-path) dispatch-table)
    (push (create-chat-dispatcher base-path chat-server*) dispatch-table)
    (setf (server-dispatch-table server) dispatch-table)
    chat-server*))
