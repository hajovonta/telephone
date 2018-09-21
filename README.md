# telephone
This is a small project to allow communication between CL instances.

It uses hunchentoot and drakma to allow communication over HTTP. Common Lisp makes it possible to send code to execute on the other side (compiled) and return results as code or data. Originally I developed it to easily command multiple instances that are running on different computers scattered around in my house.

telephone utilizes some basic error handling so when receiving faulty code over the wire, the remote REPL doesn't end up in debugger. The calling side receives a hint about the error on the remote side.

I think it's a good base for implementing distributed computing scenarios, distributed mapcar or REPL instances that are talking to each other. The communication is over HTTP so there is more overhead, but less connectivity problems as firewalls often allow through HTTP traffic without requiring special configuration.
