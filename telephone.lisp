;;;; telephone.lisp

(in-package #:telephone)

(defparameter my-id "blah1")
(defparameter instances nil)
(defparameter partners nil)
(defparameter my-handler nil)

(defparameter selected-partner nil)

(defun start-instance (name-keyword port)
  (setf (getf instances name-keyword) 
   (make-instance 'hunchentoot:easy-acceptor :port port :access-log-destination nil))
  (hunchentoot:start (getf instances name-keyword))
  (format t "~&Server started, ID: ~a, port: ~a~%" name-keyword port))

(defun stop-acceptor (name-keyword)
  (let ((instance (getf instances name-keyword)))
    (if instance
        (progn
          (hunchentoot:stop instance)
          (setf instance nil))
        (format t "~&Server is not running.~%"))))

(defun setup-handler ()
  (setf my-handler
        (hunchentoot:define-easy-handler (h1 :uri "/h1") (cmd)
          (let ((*standard-output* *standard-output*))
            (let ((retval
                    (if cmd
                        (handler-case
                            (progn
                              (format *standard-output* "Remote request: ~a~%" cmd)
                              (eval (read-from-string cmd)))
                          (error (e) (list 'error (type-of e))))
                        nil)))
              (format nil "<html><body><div id=result>~a</div><form action=/h1 method=POST><input type=hidden name=cmd></input><input type=submit></input></form></body></html>" (quri:url-encode (write-to-string retval))))))))

(defun remote-command (&key (url selected-partner) cmd)
  (let ((retval (read-from-string
                 (quri:url-decode
                  (caddr (third (fourth
                                 (chtml:parse
                                  (drakma:http-request url :method :post :parameters (list (cons "cmd" cmd)))
                                  (chtml:make-lhtml-builder)))))))))
    retval))

(defun define-partner (url name-keyword)
  (setf (getf partners name-keyword) url))

(defun get-partner (name-keyword)
  (getf partners name-keyword))

(setup-handler)
(start-instance :mikrobi 7050)
(define-partner "http://localhost:7050/h1" :mikrobi)

(defun select-partner (name-keyword)
  (setf selected-partner (get-partner name-keyword)))
