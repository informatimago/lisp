;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               botergo.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Botergo: an IRC bot answering questions about ergo on #clergo.
;;;;
;;;;    The bot indexes the ergo documentation (org-mode sources) into
;;;;    chunks, embeds them with a local ollama embedding model, and, for
;;;;    each question, retrieves the most relevant chunks and asks a local
;;;;    ollama chat model to answer from them.  Nothing leaves the host:
;;;;    ollama listens on 127.0.0.1 only.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2026-08-12 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2026 - 2026
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTERGO"
  (:use "COMMON-LISP")
  (:export "MAIN"
           "RUN-BOT" "ANSWER-QUESTION" "BUILD-INDEX" "LOAD-CONFIGURATION"
           "*VERSION*" "*CONFIGURATION*"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTERGO")

(defparameter *version* "1.0.0")

(defparameter *sources-url*
  "https://gitlab.com/com-informatimago/com-informatimago/-/tree/master/small-cl-pgms/botergo"
  "The URL where the sources of this ircbot can be found.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Configuration.
;;;
;;; Defaults are overridden by the sexp plist in the configuration file
;;; (default /home/ircbot/etc/botergo.conf, or $BOTERGO_CONFIG), which is in
;;; turn overridden by the BOTERGO_* environment variables.
;;;

(defparameter *default-configuration*
  (list :server              "irc.libera.chat"
        ;; :default would let cl-irc pick the port, but cl-irc 0.9.2 has
        ;; :ssl 6679 in *default-irc-server-port* -- a typo for 6697 -- so
        ;; the connection just hangs in SYN_SENT.  Be explicit.
        :port                6697
        :connection-security :ssl
        ;; Not "ergobot": that nick belongs to another, unrelated ergo
        ;; project, and is registered with NickServ on Libera.
        :nickname            "botergo"
        :realname            "ergo bot -- ask me about ergo"
        ;; NickServ password; NIL or "" means: do not identify.
        :nickserv-password   nil
        :channel             "#clergo"
        ;; Nicks allowed to use the administrative commands.
        :administrators      (list "pjb")
        ;; Where the ergo documentation checkout lives.
        :documentation-directory "/home/ircbot/src/ergo/"
        ;; Files of that checkout to index, relative to it.
        :documentation-files (list "documentation/ergo-specifications.org"
                                   "documentation/ergo-user-manual.org"
                                   "README.md")
        :index-file          "/home/ircbot/var/botergo/index.sexp"
        :ollama-url          "http://127.0.0.1:11434"
        :chat-model          "gemma3:4b"
        :embedding-model     "bge-m3"
        ;; Generating an answer on a CPU-only host takes tens of seconds;
        ;; drakma's default 20 s would cut the connection in the middle.
        :request-timeout     300
        ;; Number of documentation chunks given to the model as context.
        :context-chunks      3
        ;; Upper bound on the length of an answer, in tokens.
        :max-answer-tokens   140
        ;; Do not answer the same nick more often than this, in seconds.
        :cooldown-seconds    15
        ;; At most that many IRC lines per answer.
        :max-answer-lines    4)
  "The built-in configuration.")

(defvar *configuration* '()
  "The effective configuration, a plist.")

(defun config (key)
  (getf *configuration* key (getf *default-configuration* key)))

(defun irc-port ()
  "The port to connect to; works around the :SSL 6679 typo of cl-irc 0.9.2."
  (let ((port (config :port)))
    (if (eq port :default)
        (if (eq (config :connection-security) :ssl) 6697 6667)
        port)))

(defun default-configuration-file ()
  (or (uiop:getenv "BOTERGO_CONFIG") "/home/ircbot/etc/botergo.conf"))

(defun read-configuration-file (path)
  "Reads the sexp plist in PATH.  Returns NIL when PATH does not exist."
  (let ((path (probe-file path)))
    (when path
      (with-open-file (input path :external-format :utf-8)
        (let ((*read-eval* nil)
              (*package* (find-package "KEYWORD")))
          (read input nil nil))))))

(defun environment-overrides ()
  "Collects the BOTERGO_* environment variables into a plist."
  (loop :for (key variable parse)
          :in '((:server            "BOTERGO_SERVER"          nil)
                (:port              "BOTERGO_PORT"            :integer)
                (:nickname          "BOTERGO_NICKNAME"        nil)
                (:nickserv-password "BOTERGO_NICKSERV_PASSWORD" nil)
                (:channel           "BOTERGO_CHANNEL"         nil)
                (:ollama-url        "BOTERGO_OLLAMA_URL"      nil)
                (:chat-model        "BOTERGO_CHAT_MODEL"      nil)
                (:embedding-model   "BOTERGO_EMBEDDING_MODEL" nil)
                (:index-file        "BOTERGO_INDEX_FILE"      nil)
                (:documentation-directory "BOTERGO_DOCUMENTATION_DIRECTORY" nil))
        :for value = (uiop:getenv variable)
        :when (and value (string/= "" value))
          :nconc (list key (if (eq parse :integer)
                               (parse-integer value :junk-allowed t)
                               value))))

(defun load-configuration (&optional (path (default-configuration-file)))
  "Computes *CONFIGURATION* from the defaults, the configuration file and
the environment, in increasing order of precedence."
  (setf *configuration*
        (append (environment-overrides)
                (read-configuration-file path)
                (copy-seq *default-configuration*)))
  ;; :connection-security is read from the file as a keyword or a string.
  (let ((security (config :connection-security)))
    (when (stringp security)
      (setf (getf *configuration* :connection-security)
            (if (member security '("ssl" "tls") :test #'string-equal) :ssl :none))))
  *configuration*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Logging.
;;;

(defvar *log-lock* (bordeaux-threads:make-lock "botergo-log"))

(defun say (control &rest arguments)
  "Writes a timestamped line on *STANDARD-OUTPUT* (the screen window)."
  (bordeaux-threads:with-lock-held (*log-lock*)
    (multiple-value-bind (se mi ho da mo ye) (decode-universal-time (get-universal-time))
      (format t "~&~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D ~?~%"
              ye mo da ho mi se control arguments))
    (finish-output)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Talking to ollama.
;;;
;;; Requests are built as JSON text by hand (the payloads are tiny and fully
;;; under our control); responses are decoded with cl-json.
;;;

(defun json-escape (string)
  (with-output-to-string (out)
    (loop :for ch :across string
          :do (case ch
                ((#\") (write-string "\\\"" out))
                ((#\\) (write-string "\\\\" out))
                ((#\Newline) (write-string "\\n" out))
                ((#\Return)  (write-string "\\r" out))
                ((#\Tab)     (write-string "\\t" out))
                (otherwise
                 (if (< (char-code ch) 32)
                     (format out "\\u~4,'0X" (char-code ch))
                     (write-char ch out)))))))

(define-condition ollama-error (error)
  ((status  :initarg :status  :reader ollama-error-status)
   (message :initarg :message :reader ollama-error-message))
  (:report (lambda (condition stream)
             (format stream "ollama returned ~A: ~A"
                     (ollama-error-status condition)
                     (ollama-error-message condition)))))

(defun ollama-post (path json)
  "POSTs the JSON string to the ollama endpoint PATH; returns the decoded response."
  (multiple-value-bind (body status)
      (drakma:http-request (concatenate 'string (config :ollama-url) path)
                           :method :post
                           :content-type "application/json"
                           :content (babel:string-to-octets json :encoding :utf-8)
                           :force-binary t
                           ;; Under CCL this bounds the read too, not just the
                           ;; connect; the default of 20 s is far too short.
                           :connection-timeout (config :request-timeout))
    (let ((text (if (stringp body) body (babel:octets-to-string body :encoding :utf-8))))
      (unless (<= 200 status 299)
        (error 'ollama-error :status status :message text))
      (let ((cl-json:*json-symbols-package* (find-package "KEYWORD")))
        (cl-json:decode-json-from-string text)))))

(defun normalize-vector (vector)
  (let ((norm (sqrt (reduce (function +) vector :key (lambda (x) (* x x))
                                                :initial-value 0.0d0))))
    (if (zerop norm)
        vector
        (map 'vector (lambda (x) (/ x norm)) vector))))

(defun dot-product (a b)
  (loop :for i :below (min (length a) (length b))
        :sum (* (aref a i) (aref b i)) :into total :of-type double-float
        :finally (return total)))

(defun embed-texts (texts)
  "Returns the list of embedding vectors (simple-vectors of double-floats,
normalised to unit length) of the list of strings TEXTS."
  (let* ((json (format nil "{\"model\":\"~A\",\"keep_alive\":\"1h\",\"input\":[~{\"~A\"~^,~}]}"
                       (json-escape (config :embedding-model))
                       (mapcar (function json-escape) texts)))
         (response (ollama-post "/api/embed" json))
         (embeddings (cdr (assoc :embeddings response))))
    (unless embeddings
      (error 'ollama-error :status 200
                           :message (format nil "no embeddings in ~S" response)))
    (mapcar (function normalize-vector)
            (mapcar (lambda (embedding)
                      (map 'vector (lambda (x) (coerce x 'double-float)) embedding))
                    embeddings))))

(defun chat (system-prompt user-prompt)
  "Asks the chat model; returns the answer as a string."
  (let* ((json (format nil "{\"model\":\"~A\",\"stream\":false,\"think\":false,~
                            \"keep_alive\":\"1h\",~
                            \"options\":{\"temperature\":0.2,\"num_predict\":~D},~
                            \"messages\":[~
                              {\"role\":\"system\",\"content\":\"~A\"},~
                              {\"role\":\"user\",\"content\":\"~A\"}]}"
                       (json-escape (config :chat-model))
                       (config :max-answer-tokens)
                       (json-escape system-prompt)
                       (json-escape user-prompt)))
         (response (ollama-post "/api/chat" json)))
    (or (cdr (assoc :content (cdr (assoc :message response))))
        "")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The documentation index.
;;;
;;; The org-mode sources are cut into chunks of at most *CHUNK-CHARACTERS*
;;; characters, on section then on line boundaries; each chunk is prefixed
;;; with its section path so that the retrieved text carries its context.
;;;

(defparameter *chunk-characters* 1200
  "Upper bound on the size of a documentation chunk, in characters.")

(defun split-string (string separator)
  (loop :with length = (length string)
        :for start = 0 :then (1+ end)
        :for end = (or (position separator string :start start) length)
        :collect (subseq string start end)
        :until (<= length end)))

(defstruct (chunk (:constructor make-chunk (source heading text &optional vector)))
  source heading text vector)

(defun org-heading-level (line)
  "When LINE is an org heading, returns its level, else NIL."
  (let ((stars (or (position #\* line :test-not (function char=)) (length line))))
    (and (plusp stars)
         (< stars (length line))
         (char= #\Space (char line stars))
         stars)))

(defun noexport-p (line)
  (search ":noexport:" line :test (function char-equal)))

(defun skip-line-p (line)
  "Org keywords, comments and drawers carry no documentation."
  (let ((trimmed (string-left-trim " " line)))
    (or (and (<= 2 (length trimmed)) (string= "#+" (subseq trimmed 0 2)))
        (and (<= 2 (length trimmed)) (string= "# " (subseq trimmed 0 2)))
        (string= "#" trimmed)
        (and (<= 1 (length trimmed))
             (char= #\: (char trimmed 0))
             (<= 2 (length trimmed))
             (char= #\: (char trimmed (1- (length trimmed))))))))

(defun read-file-lines (path)
  (with-open-file (input path :external-format :utf-8)
    (loop :for line = (read-line input nil nil)
          :while line :collect line)))

(defun heading-text (line level)
  "Strips the stars and the trailing tags off an org heading LINE."
  (let* ((text (string-trim " " (subseq line (1+ level))))
         (tags (position #\: text :from-end t)))
    (string-right-trim " " (if (and tags (find #\: text :start (1+ tags)))
                               (subseq text 0 tags)
                               text))))

(defun parse-org-file (path source-name)
  "Cuts the org file at PATH into a list of (HEADING-PATH . TEXT) pairs."
  (let ((sections '())
        (path-stack '())     ; list of (LEVEL . HEADING), innermost first
        (skip-level nil)     ; when inside a :noexport: subtree
        (current '()))       ; accumulated lines, reversed
    (labels ((heading-path ()
               (format nil "~{~A~^ > ~}" (reverse (mapcar (function cdr) path-stack))))
             (flush ()
               (let ((text (string-trim '(#\Space #\Newline #\Tab)
                                        (format nil "~{~A~^~%~}" (reverse current)))))
                 (when (plusp (length text))
                   (push (cons (heading-path) text) sections)))
               (setf current '())))
      (dolist (line (read-file-lines path))
        (let ((level (org-heading-level line)))
          (cond
            (level
             (flush)
             ;; leave any subtree we were skipping once we are back above it
             (when (and skip-level (<= level skip-level))
               (setf skip-level nil))
             (loop :while (and path-stack (<= level (car (first path-stack))))
                   :do (pop path-stack))
             (push (cons level (heading-text line level)) path-stack)
             (when (and (null skip-level) (noexport-p line))
               (setf skip-level level)))
            ((or skip-level (skip-line-p line)))
            (t (push line current)))))
      (flush))
    (mapcar (lambda (section)
              (cons (if (plusp (length (car section)))
                        (format nil "~A: ~A" source-name (car section))
                        source-name)
                    (cdr section)))
            (nreverse sections))))

(defun split-long-text (text)
  "Cuts TEXT on line boundaries into pieces of at most *CHUNK-CHARACTERS*."
  (if (<= (length text) *chunk-characters*)
      (list text)
      (let ((pieces '())
            (current '())
            (size 0))
        (dolist (line (split-string text #\Newline))
          (when (and current (< *chunk-characters* (+ size (length line) 1)))
            (push (format nil "~{~A~^~%~}" (reverse current)) pieces)
            (setf current '() size 0))
          (push line current)
          (incf size (1+ (length line))))
        (when current
          (push (format nil "~{~A~^~%~}" (reverse current)) pieces))
        (nreverse pieces))))

(defun documentation-pathnames ()
  (let ((directory (config :documentation-directory)))
    (loop :for name :in (config :documentation-files)
          :for path = (merge-pathnames name directory)
          :when (probe-file path) :collect (probe-file path))))

(defun collect-chunks ()
  "Reads and cuts the whole documentation; returns a list of CHUNKs without vectors."
  (loop :for path :in (documentation-pathnames)
        :for source = (file-namestring path)
        :nconc (loop :for (heading . text) :in (parse-org-file path source)
                     :nconc (loop :for piece :in (split-long-text text)
                                  :collect (make-chunk source heading piece)))))

(defun index-signature ()
  "Identifies the inputs of the index, so a stale cache can be detected."
  (list :version 1
        :embedding-model (config :embedding-model)
        :chunk-characters *chunk-characters*
        :files (loop :for path :in (documentation-pathnames)
                     :collect (list (namestring path)
                                    (file-write-date path)
                                    (with-open-file (input path :element-type '(unsigned-byte 8))
                                      (file-length input))))))

(defvar *index* '()
  "The list of CHUNKs, with their embedding vectors.")

(defun save-index (path chunks)
  (ensure-directories-exist path)
  (with-open-file (output path :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create
                               :external-format :utf-8)
    (let ((*print-readably* nil)
          (*print-circle* nil)
          (*print-length* nil)
          (*print-level* nil))
      (write-line ";; -*- mode:lisp;coding:utf-8 -*-" output)
      (write-line ";; botergo documentation index -- generated, do not edit." output)
      (prin1 (list :signature (index-signature)
                   :chunks (mapcar (lambda (chunk)
                                     (list (chunk-source chunk)
                                           (chunk-heading chunk)
                                           (chunk-text chunk)
                                           (chunk-vector chunk)))
                                   chunks))
             output)
      (terpri output))))

(defun load-index (path)
  "Returns the cached chunks when the cache matches the current inputs, else NIL."
  (let ((path (probe-file path)))
    (when path
      (handler-case
          (let ((cache (with-open-file (input path :external-format :utf-8)
                         (let ((*read-eval* nil)) (read input nil nil)))))
            (when (equal (getf cache :signature) (index-signature))
              (mapcar (lambda (entry)
                        (destructuring-bind (source heading text vector) entry
                          (make-chunk source heading text vector)))
                      (getf cache :chunks))))
        (error (err)
          (say "cannot read the index cache ~A: ~A" path err)
          nil)))))

(defun build-index (&key (force nil))
  "Loads the index from its cache, or recomputes it.  Returns the chunks."
  (let ((path (config :index-file)))
    (let ((cached (and (not force) (load-index path))))
      (if cached
          (progn (say "index: ~D chunks loaded from ~A" (length cached) path)
                 (setf *index* cached))
          (let ((chunks (collect-chunks)))
            (say "index: embedding ~D chunks with ~A (this takes a while)"
                 (length chunks) (config :embedding-model))
            ;; Embed in batches, so that a hiccup costs one batch and the
            ;; request payloads stay reasonable.
            (loop :with batch-size = 8
                  :for rest :on chunks :by (lambda (list) (nthcdr batch-size list))
                  :for batch = (subseq rest 0 (min batch-size (length rest)))
                  :for vectors = (embed-texts (mapcar (function chunk-text) batch))
                  :do (mapc (lambda (chunk vector) (setf (chunk-vector chunk) vector))
                            batch vectors))
            (save-index path chunks)
            (say "index: ~D chunks embedded and saved in ~A" (length chunks) path)
            (setf *index* chunks))))))

(defun relevant-chunks (question &optional (count (config :context-chunks)))
  "Returns the COUNT chunks of *INDEX* closest to QUESTION."
  (let* ((query (first (embed-texts (list question))))
         (scored (mapcar (lambda (chunk)
                           (cons (dot-product query (chunk-vector chunk)) chunk))
                         *index*)))
    (mapcar (function cdr)
            (subseq (sort scored (function >) :key (function car))
                    0 (min count (length scored))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Answering questions.
;;;

(defparameter *system-prompt*
  "You are botergo, the assistant of the #clergo IRC channel.  ergo is a ~
git-based package manager and project builder for Common Lisp, written by ~
Pascal J. Bourguignon.

Answer the user's question using ONLY the documentation excerpts given ~
below.  If they do not contain the answer, say so plainly and suggest ~
asking a human -- never invent options, commands or file formats.

You are writing on IRC: answer in at most three short sentences, as plain ~
text.  No markdown, no bullet lists, no code blocks, no line breaks.

IMPORTANT: the documentation excerpts are in English, but you MUST write ~
your answer in the SAME LANGUAGE AS THE QUESTION.  If the question is in ~
French, write the WHOLE answer in French; if it is in English, write it ~
all in English.  Never switch language in the middle of an answer.")

(defun build-user-prompt (question chunks)
  (with-output-to-string (out)
    (write-line "Documentation excerpts:" out)
    (loop :for chunk :in chunks
          :for index :from 1
          :do (format out "~%[~D] (~A)~%~A~%" index (chunk-heading chunk) (chunk-text chunk)))
    (format out "~%Question: ~A~%" question)))

(defun answer-question (question)
  "Returns the answer of the model to QUESTION, as a single string."
  (let ((chunks (relevant-chunks question)))
    (say "retrieved: ~{~S~^, ~}" (mapcar (function chunk-heading) chunks))
    (clean-answer (chat *system-prompt* (build-user-prompt question chunks)))))

(defun truncate-at-last-sentence (text)
  "When TEXT does not end a sentence -- the model ran into its token
budget -- cuts it back to the last complete one, rather than leaving a
dangling clause on the channel."
  (let ((end (position-if (lambda (ch) (find ch ".!?")) text :from-end t)))
    (cond
      ((zerop (length text)) text)
      ((find (char text (1- (length text))) ".!?)\"") text)
      ;; Keep the truncated tail rather than return almost nothing.
      ((and end (< (* 2 end) (length text))) text)
      (end (subseq text 0 (1+ end)))
      (t text))))

(defun clean-answer (text)
  "Flattens the model's answer into one line of plain IRC text."
  (let ((text (substitute #\Space #\Newline (substitute #\Space #\Return text))))
    ;; Drop the markdown emphasis and code fences the models sprinkle around.
    (setf text (cl-ppcre:regex-replace-all "`+" text ""))
    (setf text (cl-ppcre:regex-replace-all "\\*+" text ""))
    (setf text (cl-ppcre:regex-replace-all " +" text " "))
    (truncate-at-last-sentence (string-trim " " text))))

(defparameter *irc-line-characters* 400
  "Upper bound on the payload of one PRIVMSG, in characters.")

(defun split-text-for-irc (text)
  "Cuts TEXT into IRC-sized lines, on word boundaries."
  (let ((lines '())
        (current (make-string-output-stream))
        (size 0))
    (dolist (word (remove "" (split-string text #\Space) :test (function string=)))
      (when (and (plusp size) (< *irc-line-characters* (+ size 1 (length word))))
        (push (get-output-stream-string current) lines)
        (setf size 0))
      (when (plusp size)
        (write-char #\Space current)
        (incf size))
      (write-string word current)
      (incf size (length word)))
    (let ((last (get-output-stream-string current)))
      (when (plusp (length last))
        (push last lines)))
    (let* ((lines (nreverse lines))
           (maximum (config :max-answer-lines)))
      (if (<= (length lines) maximum)
          lines
          (append (subseq lines 0 maximum) (list "[…]"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A minimal work queue.
;;;
;;; Generating an answer takes seconds; it must not happen in the thread that
;;; reads the IRC connection, or the bot would stop answering PINGs.
;;;

(defstruct (queue (:constructor %make-queue))
  (lock      (bordeaux-threads:make-lock "botergo-queue"))
  (condition (bordeaux-threads:make-condition-variable))
  (items     '()))

(defun make-queue () (%make-queue))

(defun enqueue (queue item)
  (bordeaux-threads:with-lock-held ((queue-lock queue))
    (setf (queue-items queue) (nconc (queue-items queue) (list item)))
    (bordeaux-threads:condition-notify (queue-condition queue)))
  item)

(defun dequeue (queue)
  "Waits for an item and returns it."
  (bordeaux-threads:with-lock-held ((queue-lock queue))
    (loop :until (queue-items queue)
          :do (bordeaux-threads:condition-wait (queue-condition queue)
                                               (queue-lock queue)))
    (pop (queue-items queue))))

(defun dequeue-all (queue)
  "Returns all the pending items at once, without waiting."
  (bordeaux-threads:with-lock-held ((queue-lock queue))
    (prog1 (queue-items queue)
      (setf (queue-items queue) '()))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The IRC bot.
;;;

(defvar *connection* nil)
(defvar *questions* (make-queue) "(TARGET NICK TEXT) triples waiting for an answer.")
(defvar *answers*   (make-queue) "(TARGET . TEXT) pairs waiting to be sent.")
(defvar *worker*    nil)
(defvar *last-answered* (make-hash-table :test (function equal))
  "NICK -> universal time of the last answer, for the cooldown.")

(defun administrator-p (nick)
  (member nick (config :administrators) :test (function string-equal)))

(defun reply (target text)
  (dolist (line (split-text-for-irc text))
    (enqueue *answers* (cons target line))))

(defun cooled-down-p (nick)
  (let ((last (gethash nick *last-answered*))
        (now (get-universal-time)))
    (or (null last) (<= (config :cooldown-seconds) (- now last)))))

(defun strip-address (text nickname)
  "When TEXT is addressed to NICKNAME (\"nick: question\", \"nick, question\")
or introduced by a bang command, returns the question, else NIL."
  (let ((text (string-left-trim " " text)))
    (cond
      ((and (<= (length "!ergo ") (length text))
            (string-equal "!ergo " (subseq text 0 (length "!ergo "))))
       (string-trim " " (subseq text (length "!ergo "))))
      ((and (< (length nickname) (length text))
            (string-equal nickname (subseq text 0 (length nickname)))
            (find (char text (length nickname)) ":,> "))
       (string-trim " :,>" (subseq text (length nickname))))
      (t nil))))

(defparameter *help*
  "Ask me about ergo: \"botergo: how do I declare a dependency?\", or \"!ergo <question>\". ~
Commands: help, version, sources, reindex (admins).")

(defun handle-command (target nick question)
  "Handles the built-in commands; returns T when QUESTION was one of them."
  (let ((word (string-downcase (first (split-string question #\Space)))))
    (cond
      ((string= word "help")    (reply target *help*) t)
      ((string= word "ping")    (reply target (format nil "~A: pong" nick)) t)
      ((string= word "version")
       (reply target (format nil "botergo ~A, chat model ~A, embeddings ~A, ~D doc chunks."
                             *version* (config :chat-model)
                             (config :embedding-model) (length *index*)))
       t)
      ((string= word "sources")
       (reply target (format nil "I answer questions about ergo <https://gitlab.com/ogamita/ergo>; ~
                                  I am AGPL3, my sources are at <~A>." *sources-url*))
       t)
      ((string= word "reindex")
       (if (administrator-p nick)
           (progn (reply target "Re-reading the ergo documentation…")
                  (handler-case
                      (progn (update-documentation)
                             (build-index :force t)
                             (reply target (format nil "Done: ~D chunks." (length *index*))))
                    (error (err) (reply target (format nil "Failed: ~A" err)))))
           (reply target (format nil "~A: only my administrators may do that." nick)))
       t)
      (t nil))))

(defun update-documentation ()
  "git pull in the ergo checkout, so that reindex picks up the current text."
  (let ((directory (config :documentation-directory)))
    (say "updating ~A" directory)
    (uiop:run-program (list "git" "-C" (namestring directory) "pull" "--quiet" "--ff-only")
                      :output t :error-output t :ignore-error-status t)))

(defun worker-loop ()
  "Answers the queued questions, one at a time."
  (loop
    (destructuring-bind (target nick text) (dequeue *questions*)
      (handler-case
          (unless (handle-command target nick text)
            (setf (gethash nick *last-answered*) (get-universal-time))
            (let ((answer (answer-question text)))
              (reply target (if (plusp (length answer))
                                (format nil "~A: ~A" nick answer)
                                (format nil "~A: I have nothing useful to say about that." nick)))))
        (error (err)
          (say "error while answering ~S: ~A" text err)
          (reply target (format nil "~A: sorry, my brain just failed (~A)."
                                nick (type-of err))))))))

(defun msg-hook (message)
  "Queues the questions addressed to the bot."
  (let* ((nickname (config :nickname))
         (source   (irc:source message))
         (target   (first (irc:arguments message)))
         (text     (or (second (irc:arguments message)) "")))
    (cond
      ((string-equal source nickname))   ; ourselves: ignore
      ((string-equal target nickname)    ; private message: always a question
       (say "query from ~A: ~A" source text)
       (enqueue *questions* (list source source (string-trim " " text))))
      (t
       (let ((question (strip-address text nickname)))
         (when (and question (plusp (length question)))
           (if (cooled-down-p source)
               (progn (say "question from ~A on ~A: ~A" source target question)
                      (enqueue *questions* (list target source question)))
               (say "ignored (cooldown) question from ~A" source)))))))
  t)

(defun identify-with-nickserv ()
  (let ((password (config :nickserv-password)))
    (when (and password (stringp password) (plusp (length password)))
      (say "identifying with NickServ")
      (irc:privmsg *connection* "NickServ"
                   (format nil "IDENTIFY ~A ~A" (config :nickname) password))
      ;; Give the service a moment before joining, so that +r channels let us in.
      (sleep 5))))

(defun run-bot ()
  "Connects, joins the channel, and serves questions until killed."
  (unless *worker*
    (setf *worker* (bordeaux-threads:make-thread (function worker-loop)
                                                 :name "botergo-worker")))
  (loop
    (handler-case
        (unwind-protect
             (progn
               (say "connecting to ~A:~A (~A) as ~A"
                    (config :server) (irc-port)
                    (config :connection-security) (config :nickname))
               (setf *connection*
                     (irc:connect :server   (config :server)
                                  :port     (irc-port)
                                  :nickname (config :nickname)
                                  :username (config :nickname)
                                  :realname (config :realname)
                                  :connection-security (config :connection-security)))
               (irc:add-hook *connection* 'irc:irc-privmsg-message (function msg-hook))
               (identify-with-nickserv)
               (say "joining ~A" (config :channel))
               (irc:join *connection* (config :channel))
               (loop
                 ;; READ-MESSAGE has its own timeout, so the answers queued by
                 ;; the worker are flushed regularly.
                 (irc:read-message *connection*)
                 (loop :for (target . line) :in (dequeue-all *answers*)
                       :do (say "-> ~A: ~A" target line)
                           (irc:privmsg *connection* target line))))
          (when *connection*
            (ignore-errors (irc:quit *connection* "brb"))
            (setf *connection* nil)))
      (error (err)
        (say "disconnected: ~A" err)))
    (let ((delay (+ 15 (random 30))))
      (say "reconnecting in ~D seconds" delay)
      (sleep delay))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The main program.
;;;

(defun usage ()
  (format t "~&botergo ~A~@
             ~&Usage: botergo [--config FILE] [OPTION]~@
             ~&  (no option)    connect to IRC and serve ~A on ~A~@
             ~&  --reindex      rebuild the documentation index, then exit~@
             ~&  --ask TEXT     answer TEXT on the standard output, then exit~@
             ~&  --version      print the version, then exit~@
             ~&  --help         print this help, then exit~%"
          *version* (config :channel) (config :server))
  (finish-output))

(defun main ()
  "The main program of the botergo IRC bot."
  (let ((*package* (load-time-value (find-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTERGO")))
        (arguments (rest (uiop:raw-command-line-arguments)))
        (configuration-file nil))
    ;; --config must be processed before the configuration is loaded.
    (let ((position (position "--config" arguments :test (function string=))))
      (when position
        (setf configuration-file (nth (1+ position) arguments)
              arguments (append (subseq arguments 0 position)
                                (nthcdr (+ 2 position) arguments)))))
    (load-configuration (or configuration-file (default-configuration-file)))
    (handler-case
        (let ((option (first arguments)))
          (cond
            ((null option)                 (build-index) (run-bot))
            ((string= option "--reindex")  (update-documentation) (build-index :force t))
            ((string= option "--ask")
             (build-index)
             (format t "~&~A~%" (answer-question (format nil "~{~A~^ ~}" (rest arguments))))
             (finish-output))
            ((string= option "--version")  (format t "~&botergo ~A~%" *version*) (finish-output))
            ((string= option "--help")     (usage))
            (t (format *error-output* "~&botergo: unknown option ~S~%" option)
               (usage)
               (uiop:quit 1))))
      (error (err)
        (say "fatal: ~A" err)
        (uiop:quit 1)))))

;;;; THE END ;;;;
