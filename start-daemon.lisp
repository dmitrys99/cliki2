(in-package #:cliki2)

;; start sbcl as sbcl --sysinit /home/cliki2/.sbclrc

(cl-daemonize:daemonize)

(swank:create-server :port 4010 :dont-close t :coding-system "utf-8-unix")

;; bind port 80
(start-cliki-server)

;; drop priveleges
(sb-posix:setgid (sb-posix:group-gid (sb-posix:getgrnam "cliki2")))
(sb-posix:setuid (sb-posix:passwd-uid (sb-posix:getpwnam "cliki2")))

;; *must* be done as cliki2 user or store gets wiped
(load-cliki-store)



