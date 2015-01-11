;;;; --- not used yet ---

(defclass xterm-screen (screen)
  ()
  (:documentation
   #+french "Cette sous-classe de SCREEN utilise un xterm via un pty."
   #-(or french) "This SCREEN subclass uses an xterm thru a pty."))
(defmethod screen-size ((self xterm-screen))
  (values 25 80))
(defmethod screen-cursor-position ((self xterm-screen))
  )
(defmethod set-screen-cursor-position ((self xterm-screen) line column)
  )
(defmethod clear-screen-to-eot ((self xterm-screen))
  )
(defmethod clear-screen-to-eol ((self xterm-screen))
  )
(defmethod delete-screen-line ((self xterm-screen))
  )
(defmethod insert-screen-line ((self xterm-screen))
  )
(defmethod screen-highlight-on ((self xterm-screen))
  )
(defmethod screen-highlight-off ((self xterm-screen))
  )
(defmethod screen-cursor-on ((self xterm-screen))
  )
(defmethod screen-cursor-off ((self xterm-screen))
  )
(defmethod keyboard-chord-no-hang ((self xterm-screen))
  )
(defmethod call-with-screen ((self xterm-screen) thunk)
  ;; todo before
  (unwind-protect
       (funcall thunk self)
    ;; todo after
    (progn)))
