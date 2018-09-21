;;;; telephone.lisp

(in-package #:telephone)

(defparameter acceptor nil)
(defparameter partners nil)
(defparameter my-handler nil)

(defparameter selected-partner nil)

(defun start-acceptor (port)
  (setf acceptor 
   (make-instance 'hunchentoot:easy-acceptor :port port :access-log-destination nil))
  (hunchentoot:start acceptor)
  (format t "~&Server started, port: ~a~%" port))

(defun stop-acceptor (name-keyword)
  (if acceptor
      (progn
        (hunchentoot:stop acceptor)
        (setf acceptor nil))
      (format t "~&Server is not running.~%")))

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

(defun select-partner (name-keyword)
  (setf selected-partner (get-partner name-keyword)))
