(defpackage "MY-CLIKI" (:use "CL" "ARANEIDA" "CLIKI" "SB-EXT"))
(in-package :my-cliki)

(defun my-fqdn ()
  (sb-bsd-sockets:host-ent-name 
   (sb-bsd-sockets:get-host-by-name (machine-instance))))

(defvar *url* (make-url :scheme "http" :host (my-fqdn)
			:port 8000))
(defvar *listener* (make-instance 'serve-event-http-listener
				  :port (url-port *url*)))
(defvar *cliki-instance* nil)
(setf *cliki-instance*
      (make-instance 'cliki::cliki-net
                     :data-directory "/var/www/cliki/"
                     :url-root (merge-url *url* "/cliki/")))
(install-handler (http-listener-handler *listener*)
		 *cliki-instance* 
		 (merge-url *url* "/cliki/") nil)
(start-listening *listener*)


