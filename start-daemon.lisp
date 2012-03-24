(in-package #:cliki2)

;; start sbcl as sbcl --sysinit /home/cliki2/.sbclrc

(defun start-cliki-daemon (http-port swank-port cliki-home
                           &rest other-config)
  (cl-daemonize:daemonize) ;; this has to be first for some reason

  (swank:create-server :port swank-port :dont-close t :coding-system "utf-8-unix")

  ;; bind port
  (apply #'start-cliki-server :port http-port :wiki-home cliki-home other-config)

  ;; drop priveleges
  ;;(sb-posix:setgid (sb-posix:group-gid (sb-posix:getgrnam cliki-user)))
  ;;(sb-posix:setuid (sb-posix:passwd-uid (sb-posix:getpwnam cliki-user)))
  ;; fix up fasl locations for ASDF
  ;;(sb-posix:putenv (format nil "HOME=~A" cliki-home))
  ;;(asdf:clear-output-translations)

  ;; *must* be done as cliki2 user or store gets wiped
  (load-cliki-store cliki-home))
