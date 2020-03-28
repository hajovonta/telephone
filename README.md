# telephone
This is a small project to allow communication between CL instances.

It uses hunchentoot and drakma to allow communication over HTTP. Common Lisp makes it possible to send code to execute on the other side (compiled) and return results as code or data. Originally I developed it to easily command multiple instances that are running on different computers scattered around in my house.

telephone utilizes some basic error handling so when receiving faulty code over the wire, the remote REPL doesn't end up in debugger. The calling side receives a hint about the error on the remote side.

I think it's a good base for implementing distributed computing scenarios, distributed mapcar or REPL instances that are talking to each other. The communication is over HTTP so there is more overhead, but less connectivity problems as firewalls often allow through HTTP traffic without requiring special configuration.

The communication is asynchronous. You send a command remotely (using drakma), the remote side files your request and puts it in the queue. You get an ID for your request. You can the poll the remote side with this ID and when the calculation is completed you get the result.

Sample REPL output:

```
CL-USER> (ql:quickload 'telephone)
To load "telephone":
  Load 1 ASDF system:
    telephone
; Loading "telephone"
.
;;; Checking for wide character support... WARNING: Lisp implementation doesn't use UTF-16, but accepts surrogate code points.
 yes, using code points.
;;; Building Closure with CHARACTER RUNES
......
(TELEPHONE)
CL-USER> (in-package :telephone)
#<PACKAGE "TELEPHONE">
TELEPHONE> (server-start)
Server started, port: 4242
RES-HANDLER
TELEPHONE> (start-queue-thread)
#<SB-THREAD:THREAD "telephone-queue-thread" RUNNING {1004748B43}>
TELEPHONE> (define-partner "http://localhost:4242" 'thats-me)
"http://localhost:4242"
TELEPHONE> (select-partner 'thats-me)
"http://localhost:4242"
TELEPHONE> (remote-command :cmd "(+ 1 2)")
G610 remote request: (+ 1 2)
"G610"
TELEPHONE> (remote-result :id "G610")
(FINISHED 3)
TELEPHONE>
```
