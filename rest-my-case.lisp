(in-package :rest-my-case)

(defparameter *transform-errors-p* t)

(defmacro defspec (name &rest spec)
  `(eval-when (:compile-toplevel :load-toplevel)
     (setf (get ',name 'specification) ',spec)))

(defun generate-api-fun-call (fun args command)
  (cons fun
	(mapcar 
	 (lambda (x)
	   (destructuring-bind (name type . rest) x
	     (cond
	       ((eq :post-param type)
		`(post-parameter ,(car rest)))
	       ((eq :get-param type)
		`(get-parameter ,(car rest)))
	       ((eq :body type)
		'(get-body))
	       ((eq :user type)
		'(multiple-value-list (authorization)))
	       ((eq :url-arg type)
		`(elt ,command ,(car rest))))))
	 args)))

(defun generate-api-export-exp (command fun name type args)
  (let ((api-fun-call (generate-api-fun-call fun args command)))
    `((and (string= ,name (car ,command))
	   (eq ,type (request-method)))
      (if *transform-errors-p*
	  (handler-case 
	      (encode-json-to-string ,api-fun-call)
	    (error (e)
	      (encode-json-to-string
	       (list (cons 'error       t)
		     (cons 'description (format nil "~A" e))
		     (cons 'function    ,name)))))
	  (encode-json-to-string ,api-fun-call)))))
	  


(defun subseq* (sequence start &optional end)
  (let* ((len  (length sequence))
	 (end* (if (< len end) len end)))
    (subseq sequence start end*)))

(defparameter *log* ())

(defun split-uri (uri prefix)
  (let* ((len        (length prefix))
	 (uri-prefix (subseq* uri 0 len)))
    (if (string= prefix uri-prefix)
	(split "/" (subseq uri (length prefix)))
	nil)))

(defun get-command-from-request (prefix &optional (uri (request-uri)))
  (let* ((pos  (position-if (lambda (x) (find x "?&#")) uri))
	 (uri* (if (null pos) uri (subseq uri 0 pos))))
    (split-uri uri* prefix)))

(defun handle-malformed-request ()
  (encode-json-to-string
   `((error       . t)
     (description . "did not unterstand the uri")
     (uri         . ,(request-uri)))))

(defun get-body ()
  (let ((body (raw-post-data :force-text t)))
    (if (null body)
	nil
	(decode-json-from-string body))))

(defmacro remote-api (url-prefix &rest specification)
  (destructuring-bind (&optional &key export)
      (if (and (= 1 (length specification)) (symbolp (car specification)))
	  (get (car specification) 'specification)
	  specification)
    (let ((command (gensym)))
      `(create-prefix-dispatcher
	,url-prefix
	(lambda ()
	  (let ((,command (get-command-from-request ,url-prefix)))
	    (cond ,@(mapcar (lambda (x) (apply #'generate-api-export-exp
					       (cons command x)))
			    export)
		  (t (handle-malformed-request)))))))))

(defun fill-holes (l e &optional (i 1))
  (if (null l)
      nil
      (if (< i (caar l))
	  (cons (cons i e) (fill-holes l e (1+ i)))
	  (cons (car l) (fill-holes (cdr l) e (1+ i))))))

(defun create-dyna-url (url-prefix name args jargs)
  (let ((url-args (loop
		     :for arg :in args
		     :for jarg :in jargs
		     :if (eq :url-arg (second arg))
		     :collect (cons (third arg) jarg)))
	(url*     (concatenate 'string url-prefix name)))
    (if (null url-args)
	url*
	`(+ ,url*
	    ,@(mapcan (lambda (x) (list "/" (cdr x)))
		      (fill-holes (sort url-args #'< :key #'car)
				  "x"))))))

(defun create-dyna-body (args jargs)
  (let ((i (position :body args :key #'second)))
    (if (null i) nil (elt jargs i))))

(defun create-dyna-authorization (remote-object args jargs)
  (let ((i (position :user args :key #'second)))
    (if (null i)
	nil
	(let ((jarg (elt jargs i)))
	  `('before-send
	    (lambda (req)
	      (req.set-request-header
	       "Authorization"
	       ((slot-value ,remote-object '_make-base-auth) (aref ,jarg 0)
		(aref ,jarg 1)))))))))

(defun generate-client-args (args)
  (loop
     :for arg :in args
     :collect (first arg)))

(defun generate-client-fun (url-prefix remote-object fun name type args)
  (let* ((jargs     (generate-client-args args))
	 (dyna-url  (create-dyna-url url-prefix name args jargs))
	 (dyna-body (create-dyna-body args jargs)))
    `(,fun (lambda ,(append jargs '(success))
	     (j-query.ajax (create 'url ,dyna-url
				   'type ,(symbol-name type)
				   'data-type "json"
				   ,@(if (null dyna-body)
					 nil
					 `('data (*J-S-O-N.stringify
						  ,dyna-body)))
				   ,@(create-dyna-authorization remote-object
								args jargs)
				   'success success))))))

(defun client-remote-api (url-prefix remote-object &rest specification)
  (destructuring-bind (&optional &key export)
      (if (and (= 1 (length specification)) (symbolp (car specification)))
	  (get (car specification) 'specification)
	  specification)
    (parenscript:ps*
     `(setf ,remote-object
	    (create _make-base-auth
		    (lambda (user password)
		      (return (+ "Basic "
				 (*base64.encode (+ user ":" password)))))
		    ,@(mapcan (lambda (x)
				(apply #'generate-client-fun
				       `(,url-prefix ,remote-object ,@x)))
			      export))))))

