;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               chatgpt.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A chatgpt command to send requests to chatgpt.
;;;;    Set *chatgpt-openai-key* to your own openai key.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2023-09-16 <PJB> Loosely translated from chatgpt-shell.el
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2023 - 2023
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

(defpackage "COM.INFORMATIMAGO.TOOLS.CHATGPT"
  (:use "COMMON-LISP"
        "SPLIT-SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE")
  (:export "CHATGPT"
           "*CHATGPT-OPENAI-KEY*"
           "*CHATGPT-API-URL-BASE*"
           "*CHATGPT-API-URL-PATH*"
           "*CHATGPT-PROMPT-PREFIX*"
           "*CHATGPT-MODEL-VERSIONS*"
           "*CHATGPT-MODEL-VERSION*"
           "*CHATGPT-MODEL-TEMPERATURE*"
           "*CHATGPT-ADDITIONAL-CURL-OPTIONS*"
           "*CHATGPT-REQUEST-TIMEOUT*"))
(in-package "COM.INFORMATIMAGO.TOOLS.CHATGPT")


(defun sget (entry key &optional default)
  "Like getf, but with strings for key."
  (loop
    :for (k v) :on entry :by (function cddr)
    :when (string= k key)
      :do (return-from sget v))
  default)

(defun authinfo (&key machine port login)
  (dolist (line (string-list-text-file-contents #P"~/.authinfo"))
    (let ((entry (split-sequence #\space line :remove-empty-subseqs t)))
      (when (and
             (or (null machine) (string= machine (sget entry "machine")))
             (or (null port)    (string= port    (sget entry "port")))
             (or (null login)   (string= login   (sget entry "login"))))
        (return-from authinfo (sget entry "password"))))))


(defvar *chatgpt-openai-key*
  (authinfo :machine "api.openai.com" :login "informatimago@gmail.com")
    "OpenAI key as a string or a function that loads and returns it.")

(defvar *chatgpt-api-url-base* "https://api.openai.com")
(defvar *chatgpt-api-url-path* "/v1/chat/completions")

(defun chatgpt-api-url ()
  "The complete URL OpenAI's API.
`chatgpt-shell--api-url' = `chatgpt-shell--api-url-base' + `chatgpt-shell--api-url-path'"
  (concatenate 'string *chatgpt-api-url-base* *chatgpt-api-url-path*))

(defvar *chatgpt-prompt-prefix*
  "The user is a programmer with very limited time.
You treat their time as precious. You do not repeat obvious things, including their query.
You are as concise as possible in responses.
You never apologize for confusions because it would waste their time.
You use markdown liberally to structure responses.
Always show code snippets in markdown blocks with language labels.
Don't explain code snippets.
Whenever you output updated code for the user, only show diffs, instead of entire snippets.")


(defvar *chatgpt-model-versions*
  '("gpt-3.5-turbo"
    "gpt-3.5-turbo-0613"
    "gpt-4"
    "gpt-4-0613")
  "The list of ChatGPT OpenAI models to swap from.

The list of models supported by /v1/chat/completions endpoint is
documented at
https://platform.openai.com/docs/models/model-endpoint-compatibility.")

(defvar *chatgpt-model-version* "gpt-3.5-turbo") ; or "gpt-4"

(defun chatgpt-model-version ()
  "Return active model version."
  (cond ((stringp *chatgpt-model-version*)
         *chatgpt-model-version*)
        ((integerp *chatgpt-model-version*)
         (nth *chatgpt-model-version* *chatgpt-model-versions*))
        (t
         nil)))


(defvar *chatgpt-model-temperature* nil
  "What sampling temperature to use, between 0 and 2, or nil.

Higher values like 0.8 will make the output more random, while
lower values like 0.2 will make it more focused and
deterministic.  Value of nil will not pass this configuration to
the model.

See
https://platform.openai.com/docs/api-reference/completions/create#completions/create-temperature
for details.")

(defvar *chatgpt-additional-curl-options* '()
  "Additional options for `curl' command.")

(defvar *chatgpt-request-timeout* 600
  "How long to wait for a request to time out in seconds.")

(defun chatgpt-make-curl-request-command-list (request-data)
  "Build ChatGPT curl command list using REQUEST-DATA."
  (let ((command
          (append (list "curl" (chatgpt-api-url))
                  *chatgpt-additional-curl-options*
                  (list "--fail-with-body"
                        "--no-progress-meter"
                        "-m" (prin1-to-string *chatgpt-request-timeout*)
                        "-H" "Content-Type: application/json"
                        "-H" (format nil "Authorization: Bearer ~A"
                                     (cond ((stringp *chatgpt-openai-key*)
                                            *chatgpt-openai-key*)
                                           ((functionp *chatgpt-openai-key*)
                                            (handler-case (funcall *chatgpt-openai-key*)
                                              (error () "KEY-NOT-FOUND")))))
                        "-d" (with-output-to-string (out)
                               (cl-json:encode-json request-data out))))))
    command))

(defun chatgpt-make-request-data (messages &key version temperature)
  "Make request data from MESSAGES, VERSION, and TEMPERATURE."
  (let ((request-data `((model . ,(or version (chatgpt-model-version)))
                        (messages . ,(make-array (length messages) :initial-contents messages)))))
    (when (or temperature *chatgpt-model-temperature*)
      (push `(temperature . ,(or temperature *chatgpt-model-temperature*))
            request-data))
    request-data))

(defun chatgpt-post-messages (messages &key version temperature)
  "Make a single ChatGPT request with MESSAGES.
Optionally pass model VERSION and TEMPERATURE.

\(chatgpt-post-messages
 `(((role . \"user\")
    (content . \"hello\")))
 :version \"gpt-3.5-turbo\")"

  (let ((command (chatgpt-make-curl-request-command-list
                  (chatgpt-make-request-data messages
                                             :version version
                                             :temperature temperature))))
    (multiple-value-bind (out err status)
        (uiop:run-program (format nil "~{~S~^ ~}" command)
                          :force-shell t
                          :input nil
                          :output :string
                          :error-output :string)
      (if (zerop status)
          (with-input-from-string (json out)
            (cl-json:decode-json json))
          (error "chatgpt error: ~A" err)))))

(defun chatgpt-post-prompt (prompt &key version temperature)
  "Make a single ChatGPT request with PROMPT.
Optionally pass model VERSION and/or temperature.

For example:

\(chatgpt-post-prompt \"hello\"  :version \"gpt-3.5-turbo\")"
  (let ((result (chatgpt-post-messages `(((role . "user")
                                          (content . ,prompt)))
                                       :version version
                                       :temperature temperature)))
    (cdr (assoc :content (cdr (assoc :message (first (cdr (assoc :choices result)))))))))

(defun chatgpt (prompt &key version temperature)
  (chatgpt-post-prompt prompt :version version :temperature temperature))

;;;; THE END ;;;;
