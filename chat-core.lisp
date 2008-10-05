(in-package :klatschbase)

(defparameter *max-age-in-s-default* (* 2 24 60 60))

(defclass chat-message ()
  ((id        :accessor msgid        :initarg :id)
   (timestamp :accessor msgtimestamp :initarg :timestamp)
   (text      :accessor msgtext      :initarg :text)
   (sender    :accessor msgsender    :initarg :sender)))

(defgeneric chat-message-dto (chat-message))

(defclass msg-store ()
  ())

(defgeneric add-msg (msg-store sender text &optional &key))

(defgeneric get-msg (mst-store key))

(defgeneric poll-msgs (msg-store start-key))

(defgeneric poll-msgs-wait (msg-store start-key timeout))

(defgeneric clean-msg-store (msg-store &optional &key max-age-in-s))

(defgeneric msg-store-dto (msg-store))


(defclass simple-msg-store (msg-store)
  ((msg-mtx  :accessor msg-mtx
	     :initform (portable-threads:make-lock))
   (seq      :accessor storeseq
	     :initform 0)
   (messages :accessor chat-messages
	     :initform (make-hash-table))
   (trigger  :accessor msg-trigger
             :initform (portable-threads:make-condition-variable))))

(defmethod initialize-instance :after ((store msg-store) &key (seq 0) messages)
  (setf (storeseq store) seq)
  (loop
     :with msgs-ht = (chat-messages store)
     :for msg :in messages
     :for (sec min hour date month year day daylight-p zone)= (cdr (assoc 'timestamp msg))
     :for id        = (cdr (assoc 'id msg))
     :for timestamp = (encode-universal-time sec min hour date month year zone)
     :for chat-msg  = (make-instance 'chat-message
                                     :id id :timestamp timestamp
                                     :text (cdr (assoc 'text msg))
                                     :sender (cdr (assoc 'sender msg)))
     :do (setf (gethash id msgs-ht) chat-msg)))

(defmethod add-msg ((store simple-msg-store) sender msgtext
		    &key (max-age-in-s *max-age-in-s-default*))
  (portable-threads:with-lock-held ((msg-mtx store))
    (let* ((msgs     (chat-messages store))
	   (max-ts   (- (get-universal-time) max-age-in-s))
	   (old-msgs (loop
			:for msg :being :the :hash-values :in msgs
			:if (>= max-ts (msgtimestamp msg))
			:collect (msgid msg)))
	   (id (1- (incf (storeseq store)))))
      (loop :for id :in old-msgs :do (remhash id msgs))
      (setf (gethash id msgs)
	    (make-instance 'chat-message
			   :id id
			   :timestamp (get-universal-time)
			   :sender sender
			   :text msgtext))))
  (portable-threads:with-lock-held ((msg-trigger store))
    (portable-threads:condition-variable-broadcast (msg-trigger store))))

(defmethod get-msg ((store simple-msg-store) key)
  (gethash key (chat-messages store)))

(defmethod poll-msgs ((store simple-msg-store) startkey)
  (sort
   (loop
      :for msg :being :the :hash-values :in (chat-messages store)
      :if (>= (msgid msg) startkey) :collect msg)
   #'<
   :key #'msgid))

(defmethod poll-msgs-wait ((store simple-msg-store) startkey timeout)
  (let ((msgs (poll-msgs store startkey)))
    (if (zerop (length msgs))
        (progn
          (portable-threads:with-lock-held ((msg-trigger store))
            (portable-threads:condition-variable-wait-with-timeout
             (msg-trigger store) timeout))
          (poll-msgs store startkey))
        msgs)))

(defmethod clean-msg-store ((store simple-msg-store)
                            &key (max-age-in-s *max-age-in-s-default*))
  (let* ((msgs     (chat-messages store))
	 (max-ts   (- (get-universal-time) max-age-in-s))
	 (old-msgs (loop
		      :for msg :being :the :hash-values :in msgs
		      :if (>= max-ts (msgtimestamp msg))
		      :collect (msgid msg))))
    (portable-threads:with-lock-held ((msg-mtx store))
      (loop :for id :in old-msgs :do (remhash id msgs)))))

(defmethod msg-store-dto ((store simple-msg-store))
  `(message-store
    (seq      ,(storeseq store))
    (messages ,@(loop
                   :for msg :being :the :hash-values :in (chat-messages store)
                   :collect (chat-message-dto msg)))))


(defclass chat-property-store () ())

(defgeneric put-property (chat-property-store key value)
  (:documentation "puts an element to the store"))

(defgeneric get-property (chat-property-store key)
  (:documentation "gets an element from the store"))

(defclass simple-chat-property-store (chat-property-store)
  ((store :initform (make-hash-table :test 'equal))))

(defmethod put-property ((store simple-chat-property-store) key value)
  (not (null (setf (gethash key (slot-value store 'store)) value))))

(defmethod get-property ((store simple-chat-property-store) key)
  (gethash key (slot-value store 'store)))


(defclass chat-object ()
  ((name      :accessor name       :initarg :name)
   (server    :accessor server     :initarg :server)
   (messages  :accessor chatmsgs
	      :initform (make-instance 'simple-msg-store)
              :initarg  :messages)
   (last-poll :accessor last-poll
	      :initform nil))
  (:documentation "Representation of a chat object (e.g., chat client or room)"))
  

(defclass chat-client (chat-object simple-chat-property-store)
  ((password      :accessor client-password
	          :initarg :password)
   (password-type :accessor client-password-type
		  :initarg :password-type)
   (operations    :accessor allowed-client-operations
	          :initform ()))
  (:documentation "Representation of the chat client"))

(defclass chat-room (chat-object)
  ()
  (:documentation "Chat room representation."))

(defclass chat-server ()
  ((clients      :accessor clients
		 :initform (make-hash-table :test 'equal))
   (rooms        :accessor rooms
		 :initform (make-hash-table :test 'equal))
   (server-mtx   :accessor server-mtx 
		 :initform (portable-threads:make-lock))
   (rooms-mtx    :accessor rooms-mtx
		 :initform (portable-threads:make-lock)))
 (:documentation "Representation of the chat server"))


(defgeneric register-client (chat-server chat-client)
  (:documentation "Registers a client to the server (set in the client)"))

(defgeneric unregister-client (chat-client)
  (:documentation "Unregisters a client from the server. The client is also parted from all joined chat rooms."))

(defgeneric send-message (chat-object chat-message)
  (:documentation "Sends a message to a chat-object"))

(defgeneric client-message (chat-client chat-object msg)
  (:documentation "Sends a client message msg to the destination denoted by destination."))

(defgeneric get-client-by-id (chat-server client-id)
  (:documentation "Gets a registered client by the given id"))

(defgeneric get-room-by-id (chat-server room-id)
  (:documentation "Gets a room by the given id"))

(defgeneric get-chat-message (chat-object key))

(defgeneric poll-chat-messages (chat-object start-key))

(defgeneric poll-chat-messages-wait (chat-object start-key timeout))

(defgeneric poll-activity (chat-object)
  (:documentation "Determines the activity regarding the chat object's polling history. The activity is an integer in the range 0 to 255 where 0 means no activity and 255 means full activity"))

(defgeneric register-room (chat-server chat-room))

(defgeneric create-room (chat-server name))

(defgeneric remove-room (chat-server name))

(defgeneric list-rooms (chat-server))

(defgeneric list-clients (chat-server))

(defgeneric chat-object-dto (chat-object))

(defgeneric authenticate-client (chat-server name password))

(defgeneric save-chat-server (chat-server file-name))


(defparameter *disallowed-chars*
  (list #\space #\tab #\newline #\linefeed #\page #\backspace #\return
	#\/ #\$ #\% #\\ #\& #\^ #\" #\' #\: #\; #\? #\_ #\#))

(defun name-p (name)
  "Checks whether the name is valid (i.e., contains only letters and numbers)."
  (and (>= (length name) 2)
       (<= (length name) 32)
       (not (find-if (lambda (c) (find c *disallowed-chars*)) name))))

(defmethod client-message ((client chat-client) (dst chat-object) msg)
  (add-msg (chatmsgs dst) client msg))

(defmethod get-client-by-id ((server chat-server) name)
  (gethash name (clients server)))

(defmethod get-room-by-id ((server chat-server) name)
  (gethash name (rooms server)))

(defmethod register-client ((server chat-server) (client chat-client))
  (if (name-p (name client))
      (portable-threads:with-lock-held ((server-mtx server))
	(let ((name    (name client))
	      (clients (clients server)))
	  (if (cadr (multiple-value-list (gethash name clients)))
	      (error "~S is already used" name)
	      (progn (setf (gethash name clients) client)
		     `((id . ,name)
		       (startkey . ,(storeseq (chatmsgs client))))))))
      (error "~S is not a valid name" (name client))))

(defmethod unregister-client ((client chat-client))
  (portable-threads:with-lock-held ((server-mtx (server client)))
    (remhash (name client) (clients (server client)))))

(defmethod create-room ((server chat-server) name)
  (register-room server (make-instance 'chat-room :name name)))

(defmethod register-room ((server chat-server) (room chat-room))
  (let ((name (name room)))
    (if (name-p name)
        (portable-threads:with-lock-held ((rooms-mtx server))
          (let ((rooms (rooms server)))
            (let ((r (gethash name rooms)))
              (if (null r)
                  (progn
                    (setf (gethash name rooms) room)
                    `((id . ,name)
                      (created . t)))
                  `((id . ,name)
                    (created . nil))))))
        (error "room name is malformed"))))


(defmethod remove-room ((server chat-server) name)
  (portable-threads:with-lock-held ((rooms-mtx server))
    (let* ((rooms (rooms server))
	   (room  (gethash name rooms)))
      (if (null room)
	  (error "room not found")
	  (progn
	    (remhash name (rooms server))
	    `((id . ,name)
	      (removed . t)))))))

(defmethod chat-object-dto ((obj chat-room))
  `((id . ,(name obj))
    (category . room)
    (poll-activity . ,(poll-activity obj))
    (startkey . ,(storeseq (chatmsgs obj)))))

(defmethod chat-object-dto ((obj chat-client))
  `((id . ,(name obj))
    (category . client)
    (poll-activity . ,(poll-activity obj))
    (startkey . ,(storeseq (chatmsgs obj)))))


(defmethod list-rooms ((server chat-server))
  (loop
     :for r :being :the :hash-value :of (rooms server)
     :collect (chat-object-dto r)))

(defmethod list-clients ((server chat-server))
  (loop
     :for r :being :the :hash-value :of (clients server)
     :collect (chat-object-dto r)))

(defmethod get-chat-message ((co chat-object) key)
  (get-msg (chatmsgs co) key))

(defmethod poll-chat-messages ((co chat-object) startkey)
  (setf (last-poll co) (get-universal-time))
  (poll-msgs (chatmsgs co) startkey))

(defmethod poll-chat-messages-wait ((co chat-object) startkey timeout)
  (setf (last-poll co) (get-universal-time))
  (poll-msgs-wait (chatmsgs co) startkey timeout))

(defun poll-chat-messages-list (co-list)
  (loop
     :for (co startkey) :in co-list
     :append (poll-chat-messages co startkey)))

(defmethod poll-activity ((co chat-object))
  (labels ((norm-time (x)
	     (- 256 (max 1 (truncate (log (+ 1 (expt x 12)) 2))))))
    (if (null (last-poll co))
	0
	(norm-time (- (get-universal-time) (last-poll co))))))

(defmethod chat-message-dto ((msg chat-message))
  (let* ((sender    (msgsender msg))
	 (name      (if (null sender) nil (name sender)))
	 (id        (msgid msg))
	 (text      (msgtext msg))
	 (timestamp (msgtimestamp msg)))
  `((id . ,id)
    (timestamp . ,(multiple-value-list (decode-universal-time timestamp)))
    (text . ,text)
    (sender . ,name))))

(defun sha256 (str)
  (let* ((utf8 (flexi-streams:make-external-format :utf-8))
	 (str* (flexi-streams:string-to-octets str :external-format utf8)))
    (ironclad:digest-sequence :sha256 str*)))

(defmethod authenticate-user ((server chat-server) name password)
  (let ((client (get-client-by-id server name)))
    (if (null client)
	nil
	(let ((cpass (client-password client))
	      (ptype (if (slot-exists-p client 'password-type)
			 (client-password-type client)
			 nil)))
	  (cond
	    ((null ptype)
	     (if (string= password cpass)
		 client
		 nil))
	    ((and (consp ptype) (eq :sha256 (car ptype)))
	     (if (equalp (sha256 (concatenate 'string password (cdr ptype)))
			 cpass)
		 client
		 nil))
	    (t
	     (error "unknown password type")))))))

(defmethod print-object ((client chat-client) stream)
  (write `(client
           (name               ,(name client))
           (password           ,(client-password-type client) ,(client-password client))
           (allowed-operations ,@(allowed-client-operations client))
           (properties         ,@(loop
                                    :for key :being :the :hash-keys :in (slot-value client 'store)
                                    :using (hash-value value)
                                    :collect (cons key value)))
           ,(msg-store-dto (chatmsgs client)))
         :stream stream))

(defmethod print-object ((room chat-room) stream)
  (write `(room
           (name ,(name room))
           ,(msg-store-dto (chatmsgs room)))
         :stream stream))

(defmethod save-chat-server ((server chat-server) file-name)
  (with-open-file (stream file-name :direction :output :if-exists :supersede)
    (loop
       :for client :being :the :hash-values :in (clients server)
       :do (format stream "~S~%" client))
    (loop
       :for room :being :the :hash-values :in (rooms server)
       :do (format stream "~S~%" room))))


(defun process-server-spec (server spec-stream)
  (labels
      ((spec-props (store props)
         (loop
            :for (key . value) :in props
            :do (put-property store key value)))
       (spec-client (params)
         (let* ((name     (second (assoc 'name params)))
                (pwd      (cdr (assoc 'password params)))
                (props    (cdr (assoc 'properties params)))
                (msgs     (cdr (assoc 'message-store params)))
                (msgstore (make-instance 'simple-msg-store
                                         :seq      (second (assoc 'seq msgs))
                                         :messages (cdr (assoc 'messags msgs))))
                (client   (make-instance 'chat-client
                                         :name          name
                                         :server        server
                                         :password-type (first pwd)
                                         :password      (second pwd)
                                         :messages      msgstore)))
           (spec-props client props)
           (register-client server client)))
       (spec-room (params)
         (let* ((name     (second (assoc 'name params)))
                (msgs     (cdr (assoc 'message-store params)))
                (msgstore (make-instance 'simple-msg-store
                                         :seq      (second (assoc 'seq msgs))
                                         :messages (cdr (assoc 'messags msgs))))
                (room     (make-instance 'chat-room
                                         :name          name
                                         :server        server
                                         :messages      msgstore)))
           (register-room server room)))
       (spec-stmt (stmt params)
         (case stmt
           (client (spec-client params))
           (room   (spec-room params)))))
    (loop
       :for stmt = (read spec-stream nil)
       :if (null stmt) :do (return)
       :do (spec-stmt (car stmt) (cdr stmt)))))

(defmethod initialize-instance :after ((server chat-server) &key save-file save-period)
  (unless (null save-file)
    (with-open-file (stream save-file :if-does-not-exist nil)
      (unless (null stream)
        (process-server-spec server stream)))
    (unless (null save-period)
      (portable-threads:spawn-periodic-function
       (lambda ()
         (save-chat-server server save-file))
       save-period :name 'save-server-job))))
