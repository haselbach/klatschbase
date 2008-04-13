(defpackage :calculator
  (:use :common-lisp :hunchentoot :rest-my-case))
(in-package :calculator)

(+ 2 3)

(start-server :port 4242)

(defparameter *calculator-dispatcher*
  (labels
      ((add (x y) (+ (parse-integer x) (parse-integer y)))
       (foo () '(foo bar))
       (addlist (l) (apply #'+ l)))
    (remote-api
     "/calculator/"
     :export
     ((add
       "add" :get
       ((:get-param "sum1") (:get-param "sum2")))
      (addlist
       "add" :post
       ((:body)))
      (add
       "sum" :get
       ((:url-arg 1) (:url-arg 2)))
      (foo
       "foo" :get
       ())))))

(print
 (macroexpand-1
  '(remote-api
    "/calculator/"
    :export
    ((add
      "add" :get
      ((:get-param "sum1") (:get-param "sum2")))
     (addlist
      "add" :post
      ((:body)))))))

(pop *dispatch-table*)

(push *calculator-dispatcher* *dispatch-table*)
(push (create-prefix-dispatcher
       "/blubber"
       (lambda () (json:encode-json-to-string '(foo bar))))
      *dispatch-table*)

(labels
    ((foo () '(foo 3)))
  (push (CREATE-PREFIX-DISPATCHER
	 "/calculatorwtf1/"
	 (LAMBDA ()
	   (LET ((command
		  (REST-MY-CASE::GET-COMMAND-FROM-REQUEST "/calculatorwtf1/")))
	     (cond ((and (string= "foo" (car command))
			 (eq :get (request-method)))
		    (JSON:ENCODE-JSON-TO-STRING (FOO)))
		   (t
		    "error")))))
	*dispatch-table*))


(push (create-prefix-dispatcher "/klatschbase/ops"
				(lambda () (handle-operation-requests)))
      *dispatch-table*)


(s-http-client:do-http-request "http://localhost:4242/calculator/add"
  :method :post
  :content-type "text/json"
  :content "[2, 3, 5, 1]")

(s-http-client:do-http-request
    "http://localhost:4242/klatschbase/ops/client/chriss"
  :method :put
  :content-type "text/json"
  :content (json:encode-json-to-string
	    '((login . "chriss") (password . "blub"))))

(s-http-client:do-http-request
    "http://localhost:4242/klatschbase/ops/client/foo"
  :method :put
  :content-type "text/json"
  :content (json:encode-json-to-string
	    '((login . "foo") (password . "bla"))))


(print
 (s-http-client:do-http-request
     "http://localhost:4242/klatschbase/ops/post-message"
   :method :post
   :content-type "text/json"
   :basic-authorization '("chriss" . "blub")
   :content (json:encode-json-to-string
	     '((user . "zort2")
	       (client . "foo")
	       (msgtext . "Hallo nochmal")))))

(print
 (s-http-client:do-http-request
     "http://localhost:4242/klatschbase/ops/get-messages/foo/0"
   :method :get))





;;; something else...

(defun jolly-p (list)
  (let ((len (length list)))
    (labels
	((jolly (x xs used)
	   (if (null xs)
	       t
	       (jolly-step x (car xs) (cdr xs) used)))
	 (jolly-step (x y ys used)
	   (let ((d (abs (- x y))))
	     (if (or (zerop d) (>= d len) (logbitp d used))
		 nil
		 (jolly y ys (logior used (ash 1 d)))))))
      (if (> len 3000)
	  nil
	  (jolly (car list) (cdr list) 0)))))

