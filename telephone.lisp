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
            (list 'system-down nil))
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

;; example usage:
;; (server-start)
;; (define-partner "http://localhost:4242" 'thats-me)
;; (select-partner 'thats-me)
;; (remote-command :cmd "(+ 1 2)")
;; (remote-result :id "whatever-the-id-is-that-you-got")
