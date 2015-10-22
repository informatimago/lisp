#!/usr/local/bin/clisp -ansi -q -E utf-8 
;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               html-wrap-document
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This script takes a file containing a <div class="document"> entity
;;;;    and produces a HTML page containing it, with site header and footer.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-10-20 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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


(defmacro redirecting-stdout-to-stderr (&body body)
  (let ((verror  (gensym))
        (voutput (gensym)))
    `(let* ((,verror  nil)
            (,voutput (with-output-to-string (stream)
                        (let ((*standard-output* stream)
                              (*error-output*    stream)
                              (*trace-output*    stream))
                          (handler-case (progn ,@body)
                            (error (err) (setf ,verror err)))))))
       (when ,verror
         (terpri *error-output*)
         (princ ,voutput *error-output*)
         (terpri *error-output*)
         (princ ,verror *error-output*)
         (terpri *error-output*)
         (terpri *error-output*)
         #-testing-script (ext:exit 1)))))

(redirecting-stdout-to-stderr
  (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))

(redirecting-stdout-to-stderr
  (ql:quickload :com.informatimago.common-lisp))

(use-package "COM.INFORMATIMAGO.COMMON-LISP.HTML-PARSER.PARSE-HTML")

(com.informatimago.common-lisp.cesarum.package:add-nickname
 "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML" "<")



(defun clean-text (text)
  (with-output-to-string (*standard-output*)
    (with-input-from-string (*standard-input* text)
      (loop
        :for last-was-space = nil :then (or ch-is-space (char= ch #\newline))
        :for ch = (read-char *standard-input* nil nil)
        :for ch-is-space = (and ch (char= ch #\space))
        :while ch
        :do (if ch-is-space
                (unless last-was-space
                  (write-char ch))
                (write-char ch))))))


#-(and)
(<:with-html-output (*standard-output* :kind :html :encoding :utf-8)
  (<:div (:class "document"
          :id "small-cl-pgms"
          :title       "Common-Lisp Small Programs and Tidbits"
          :author      "Pascal J. Bourguignon"
          :description "Small Common-Lisp Programs and Tidbits"
          :keywords    "software,logiciel,programas,GPL,LGPL,Lisp,Common-Lisp")

    (<:h1 () (<:pcdata "Common-Lisp Small Programs and Tidbits"))
    (<:h2 () (<:pcdata "Downloading the sources"))
    (<:p ()
      (<:pcdata (clean-text "The sources of these small Common-Lisp programs can be downloaded via "))
      (<:a (:href "http://git-scm.com/") (<:pcdata "git"))
      (<:pcdata ". Use the following command to fetch them:"))
    (<:pre ()
      (<:pcdata "
mkdir com
git clone https://git.gitorious.org/com-informatimago/com-informatimago.git com/informatimago
ls com/informatimago/small-cl-pgms
"))

    (dolist (section  *sections*)
      (<:h2 () (<:pcdata (clean-text (first section))))
      (<:p  () (<:pcdata (clean-text (second section))))
      (dolist (soft (cddr section))
        (<:h3 () (<:pcdata (clean-text (first soft))))
        (<:p  () (<:pcdata (clean-text (third soft))))
        (when (second soft)
          (<:ul ()
            (dolist (file (second soft))
              (<:li ()
                (if (stringp file)
                    (<:a (:href file)
                      (<:pcdata (clean-text file)))
                    (<:a (:href (first file))
                      (<:pcdata (clean-text (second file)))))))))))))


(defparameter *newline* (format nil "~%"))

(defun remove-newlines (entity)
  (if (atom entity)
      entity
      `(,(html-tag entity)
        ,(html-attributes entity)
        ,@(mapcan (lambda (child)
                    (unless (equal child *newline*)
                      (list (remove-newlines child))))
                  (html-contents entity)))))


(defun wrap (output title author description keywords language
             class id contents)
  (write-line "<?xml version=\"1.0\" encoding=\"utf-8\" ?>" output)
  (write-line "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" output)
  (write-line "<!-- THIS FILE IS GENERATED BY html-wrap-document -->" output)
  (write-line "<!-- PLEASE DO NOT EDIT THIS FILE! -->" output)
  (format output "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"~A\"~:* lang=\"~A\">~%" language)
  (unparse-html `(:head ()
                   (:title () ,title)
                   (:link (:rel "stylesheet"    :href "/default.css" :type "text/css"))
                   (:link (:rel "icon"          :href "/favicon.ico" :type "image/x-icon"))
                   (:link (:rel "shortcut icon" :href "/favicon.ico" :type "image/x-icon"))
                   (:meta (                     :content "text/html; charset=utf-8" :http-equiv "Content-Type" ))
                   (:meta (:name "author"       :content ,author))
                   (:meta (:name "description"  :content ,description               :http-equiv "Description"))
                   (:meta (:name "keywords"     :content ,keywords)))
                output)
  (write-string "<body>
<!-- TOP-BEGIN -->
<!-- TOP-END -->
<!-- MENU-BEGIN -->
<!-- MENU-END -->
" output)
  (unparse-html  `(:div (:class ,class :id ,id)
                    ,@contents)
                 output)
  (write-string "
<!-- MENU-BEGIN -->
<!-- MENU-END -->
<!-- BOTTOM-BEGIN -->
<!-- BOTTOM-END -->
</body></html>
" output)
  (values))


(defun main (&optional arguments)
  (declare (ignore arguments))
  (let* ((document    (remove-newlines
                       (find-if (function consp)
                                (parse-html-stream *standard-input*))))
         (class       (html-attribute document :class       )))
    (if (and (eq :div (html-tag document))
             (equal class "document"))
        (let ((id          (html-attribute document :id          ))
              (title       (html-attribute document :title       ))
              (author      (html-attribute document :author      ))
              (description (html-attribute document :description ))
              (keywords    (html-attribute document :keywords    ))
              (language    (html-attribute document :language    )))
          (wrap *standard-output*
                title author description keywords language
                class id
                (html-contents document)))
        (error "Not a <div class=\"document\">…</div> input file."))))


#-testing-script
(progn
  (main ext:*args*)
  (ext:exit 0))

(pushnew :testing-script *features*)
(with-open-file (*standard-input* #P"aim-8/aim-8.html.in")
  (parse-html-stream *standard-input*))
;;;; THE END ;;;;
