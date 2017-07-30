
# site
My site code

sbcl --load site.lisp

(in-package :site)

(sb-ext:save-lisp-and-die "start-server" :toplevel #'start-server :executable t)

screen

sudo ./start-server

<C-a d>
