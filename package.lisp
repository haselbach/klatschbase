(defpackage :rest-my-case
  (:use :common-lisp :hunchentoot :cl-ppcre :json)
  (:export defspec remote-api client-remote-api *transform-errors-p*))

(defpackage :klatschbase
  (:use :common-lisp :hunchentoot :rest-my-case :portable-threads)
  (:export start-service))

