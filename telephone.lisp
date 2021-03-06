;;;; telephone.lisp

(in-package #:telephone)

(defparameter acceptor nil)
(defparameter partners nil)
(defparameter command-handler nil)
(defparameter result-handler nil)

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

(defclass calculation ()
  ((id :initarg :id :accessor id :documentation "Unique identifier for calculation request.")
   (cmd :initarg :cmd :accessor cmd :documentation "The lisp form that is to be calculated.")
   (calculation-thread :initarg :calculation-thread :initform nil :accessor calculation-thread :documentation "The thread that performs the calculation.")))

(defparameter exec-queue nil)

(defun exec-cmd (cmd)
  (handler-case
      (eval (read-from-string cmd))
    (error (e) (list 'error (type-of e)))))

(defun exec-next ()
  (let ((next-calculation
         (car (remove-if #'(lambda (x) (slot-value x 'calculation-thread)) exec-queue))))
    (when next-calculation
      (setf (slot-value next-calculation 'calculation-thread) (sb-thread:make-thread #'(lambda (cmd) (exec-cmd cmd)) :arguments (list (slot-value next-calculation 'cmd)))))))

(defun exec-res (id)
  (let ((calc (car (remove-if-not #'(lambda (x) (string= id (id x))) exec-queue))))
    (if calc
        (if (calculation-thread calc)
            (if (sb-thread:thread-alive-p (calculation-thread calc))
                (list 'running nil)
                (list 'finished
                      (sb-thread:join-thread (calculation-thread calc))))
            (list 'no-thread nil))
        (list 'id-not-found nil))))

(defun setup-handlers ()
  (setf command-handler
        (hunchentoot:define-easy-handler (cmd-handler :uri "/tcmd") (cmd)
          (let ((*standard-output* *standard-output*))
            (when cmd
              (let ((id (symbol-name (gensym))))
                (format *standard-output* "~a remote request: ~a~%" id cmd)
                (push (make-instance 'calculation :id id :cmd cmd) exec-queue)
                (format nil "~a" (quri:url-encode (write-to-string id))))))))
  (setf result-handler
        (hunchentoot:define-easy-handler (res-handler :uri "/tres") (id)
          (let ((*standard-output* *standard-output*))
            (when id
              (format nil "~a" (quri:url-encode (write-to-string (exec-res id)))))))))

(defun start-queue-thread ()
  (sb-thread:make-thread #'(lambda () (loop (exec-next) (sb-thread:thread-yield) (sleep .1))) :name "telephone-queue-thread"))

(defun stop-queue-thread ()
  (let ((telephone-queue-thread (car (remove-if-not #'(lambda (x) (string= "telephone-queue-thread" (sb-thread:thread-name x))) (sb-thread:list-all-threads)))))
    (if telephone-queue-thread
        (progn
          (sb-thread:terminate-thread telephone-queue-thread)
          (format t "telephone-queue-thread terminated."))
        (format t "telephone-queue-thread not found!"))))

(defun remote-command (&key (url (format nil "~a/tcmd" selected-partner)) cmd)
  (let ((retval (read-from-string
                 (quri:url-decode
                  (drakma:http-request url :method :post :parameters (list (cons "cmd" cmd)))))))
    retval))

(defun remote-result (&key (url (format nil "~a/tres" selected-partner)) id)
  (let ((retval (read-from-string
                 (quri:url-decode
                  (drakma:http-request url :method :post :parameters (list (cons "id" id)))))))
    retval))

(defun define-partner (url name-keyword)
  (setf (getf partners name-keyword) url))

(defun get-partner (name-keyword)
  (getf partners name-keyword))

(defun select-partner (name-keyword)
  (setf selected-partner (get-partner name-keyword)))

(defun server-start (&optional (port 4242))
  (unless acceptor
    (start-acceptor port))
  (unless (and command-handler result-handler)
    (setup-handlers)))

(defun wait-for-result (&key (url selected-partner) id (polling-interval 1))
  (loop
     do
       (let ((result (remote-result :url (format nil "~a/tres" url) :id id)))
         (when (eq (car result) 'finished)
           (return (second result))))
       (sleep polling-interval)))

(defun calculate (&key (url selected-partner) cmd (overhead-interval .1) (polling-interval 1))
  (let ((id (remote-command :url (format nil "~a/tcmd" url) :cmd cmd)))
    (if id
        (progn
          (sleep overhead-interval)
          (wait-for-result :url url :id id :polling-interval polling-interval))
        'error-no-id)))

(defmacro calculate-lazy (&key (url selected-partner) cmd)
  (let ((id (remote-command :url (format nil "~a/tcmd" url) :cmd (write-to-string cmd))))
    (lambda () (wait-for-result :url url :id id))))

(defun init ()
  (server-start)
  (start-queue-thread)
  (define-partner "http://localhost:4242" 'me)
  (select-partner 'me))

;; example usage:
;; (init)

;; send command and get the id
;; (remote-command :cmd "(+ 1 2)")

;; the following will return the status of the calculation and the result if there is available
;; (remote-result :id "whatever-the-id-is-that-you-got")

;; the following will wait and block until there is a result
;; (wait-for-result :id "whatever-the-id-is-that-you-have")

;; the following returns a closure that you can funcall later any time to retrieve the result
;; this is the most convenient usage
;; (calculate-lazy :cmd (+ 1 2))

;; (stop-queue-thread)

