;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:              readline.lisp
;;;;LANGUAGE:          SBCL Common-Lisp
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;    This module defines the package COM.INFORMATIMAGO.SBCL.READLINE
;;;;    which is a foreign interface to the GNU readline and the GNU history
;;;;    libraries.
;;;;USAGE
;;;;    (READLINE:INITIALIZE)
;;;;    (READLINE:USING-HISTORY)
;;;;    (LOOP FOR LINE = (READLINE:READLINE) UNTIL (NULL LINE)
;;;;          DO (FORMAT T "READ THIS LINE: ~W~%" LINE))
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2010-11-01 <PJB> Removed DEFINE-PACKAGE.
;;;;    2003-05-08 <PJB> Introduced DEFINE-PACKAGE.
;;;;    2003-04-25 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    Copyright Pascal J. Bourguignon 2003 - 2003
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU  General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This script is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this library; see the file COPYING.LIB.
;;;;    If not, write to the Free Software Foundation,
;;;;    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;*****************************************************************************

(in-package "COMMON-LISP-USER")

(defpackage "COM.INFORMATIMAGO.SBCL.READLINE"
  (:documentation
   "This package is a foreign interface to the GNU readline
and GNU history libraries.")
  (:use "COMMON-LISP" "SB-ALIEN" "SB-C" "SB-SYS")
  (:export
   ;; functions
   "C-NULL" "INITIALIZE"
   ;; types:
   "HISTDATA-T" "HIST-ENTRY" "HISTORY-STATE"
   ;; constants:
   "HS-STIFLED"
   ;; variables:
   "HISTORY-BASE" "HISTORY-LENGTH" "HISTORY-MAX-ENTRIES"
   "HISTORY-EXPANSION-CHAR" "HISTORY-SUBST-CHAR"
   "HISTORY-WORD-DELIMITERS" "HISTORY-COMMENT-CHAR"
   "HISTORY-NO-EXPAND-CHARS" "HISTORY-SEARCH-DELIMITER-CHARS"
   "HISTORY-QUOTES-INHIBIT-EXPANSION" "MAX-INPUT-HISTORY"
   "HISTORY-INHIBIT-EXPANSION-FUNCTION"
   ;; functions:
   "USING-HISTORY" "HISTORY-GET-HISTORY-STATE"
   "HISTORY-SET-HISTORY-STATE" "ADD-HISTORY" "REMOVE-HISTORY"
   "REPLACE-HISTORY-ENTRY" "CLEAR-HISTORY" "STIFLE-HISTORY"
   "UNSTIFLE-HISTORY" "HISTORY-IS-STIFLED" "HISTORY-LIST"
   "WHERE-HISTORY" "CURRENT-HISTORY" "HISTORY-GET"
   "HISTORY-TOTAL-BYTES" "HISTORY-SET-POS" "PREVIOUS-HISTORY"
   "NEXT-HISTORY" "HISTORY-SEARCH" "HISTORY-SEARCH-PREFIX"
   "HISTORY-SEARCH-POS" "READ-HISTORY" "READ-HISTORY-RANGE"
   "WRITE-HISTORY" "APPEND-HISTORY" "HISTORY-TRUNCATE-FILE"
   "HISTORY-EXPAND" "HISTORY-ARG-EXTRACT" "GET-HISTORY-EVENT"
   "HISTORY-TOKENIZE"
   ;; types:
   "UNDO-CODE"
   "UNDO-LIST"
   "FUNMAP"
   ;; constants:
   "RL_SYMBOLIC_LINK_HOOK" "RL-PROMPT-START-IGNORE"
   "RL-PROMPT-END-IGNORE" "NO-MATCH" "SINGLE-MATCH" "MULT-MATCH"
   "RL-STATE-NONE" "RL-STATE-INITIALIZING" "RL-STATE-INITIALIZED"
   "RL-STATE-TERMPREPPED" "RL-STATE-READCMD" "RL-STATE-METANEXT"
   "RL-STATE-DISPATCHING" "RL-STATE-MOREINPUT" "RL-STATE-ISEARCH"
   "RL-STATE-NSEARCH" "RL-STATE-SEARCH" "RL-STATE-NUMERICARG"
   "RL-STATE-MACROINPUT" "RL-STATE-MACRODEF" "RL-STATE-OVERWRITE"
   "RL-STATE-COMPLETING" "RL-STATE-SIGHANDLER" "RL-STATE-UNDOING"
   "RL-STATE-INPUTPENDING" "RL-STATE-DONE"
   ;; variables:
   "RL-LIBRARY-VERSION" "RL-GNU-READLINE-P" "RL-READLINE-STATE"
   "RL-EDITING-MODE" "RL-READLINE-NAME" "RL-PROMPT" "RL-LINE-BUFFER"
   "RL-POINT" "RL-END" "RL-MARK" "RL-DONE" "RL-PENDING-INPUT"
   "RL-DISPATCHING" "RL-EXPLICIT-ARG" "RL-NUMERIC-ARG" "RL-LAST-FUNC"
   "RL-TERMINAL-NAME" "RL-INSTREAM" "RL-OUTSTREAM" "RL-STARTUP-HOOK"
   "RL-PRE-INPUT-HOOK" "RL-EVENT-HOOK" "RL-GETC-FUNCTION"
   "RL-REDISPLAY-FUNCTION" "RL-PREP-TERM-FUNCTION"
   "RL-DEPREP-TERM-FUNCTION" "RL-EXECUTING-KEYMAP" "RL-BINDING-KEYMAP"
   "RL-ERASE-EMPTY-LINE" "RL-ALREADY-PROMPTED" "RL-NUM-CHARS-TO-READ"
   "RL-EXECUTING-MACRO" "RL-CATCH-SIGNALS" "RL-CATCH-SIGWINCH"
   "RL-COMPLETION-ENTRY-FUNCTION" "RL-IGNORE-SOME-COMPLETIONS-FUNCTION"
   "RL-ATTEMPTED-COMPLETION-FUNCTION" "RL-BASIC-WORD-BREAK-CHARACTERS"
   "RL-COMPLETER-WORD-BREAK-CHARACTERS" "RL-COMPLETER-QUOTE-CHARACTERS"
   "RL-BASIC-QUOTE-CHARACTERS" "RL-FILENAME-QUOTE-CHARACTERS"
   "RL-SPECIAL-PREFIXES" "RL-DIRECTORY-COMPLETION-HOOK"
   "RL-DIRECTORY-REWRITE-HOOK" "RL-COMPLETION-DISPLAY-MATCHES-HOOK"
   "RL-FILENAME-COMPLETION-DESIRED" "RL-FILENAME-QUOTING-DESIRED"
   "RL-FILENAME-QUOTING-FUNCTION" "RL-FILENAME-DEQUOTING-FUNCTION"
   "RL-CHAR-IS-QUOTED-P" "RL-ATTEMPTED-COMPLETION-OVER"
   "RL-COMPLETION-TYPE" "RL-COMPLETION-APPEND-CHARACTER"
   "RL-COMPLETION-QUERY-ITEMS" "RL-IGNORE-COMPLETION-DUPLICATES"
   "RL-INHIBIT-COMPLETION"
   ;; functions:
   "USING-HISTORY" "HISTORY-GET-HISTORY-STATE"
   "HISTORY-SET-HISTORY-STATE" "ADD-HISTORY" "REMOVE-HISTORY"
   "REPLACE-HISTORY-ENTRY" "CLEAR-HISTORY" "STIFLE-HISTORY"
   "UNSTIFLE-HISTORY" "HISTORY-IS-STIFLED" "HISTORY-LIST" "WHERE-HISTORY"
   "CURRENT-HISTORY" "HISTORY-GET" "HISTORY-TOTAL-BYTES"
   "HISTORY-SET-POS" "PREVIOUS-HISTORY" "NEXT-HISTORY" "HISTORY-SEARCH"
   "HISTORY-SEARCH-PREFIX" "HISTORY-SEARCH-POS" "READ-HISTORY"
   "READ-HISTORY-RANGE" "WRITE-HISTORY" "APPEND-HISTORY"
   "HISTORY-TRUNCATE-FILE" "HISTORY-EXPAND" "HISTORY-ARG-EXTRACT"
   "GET-HISTORY-EVENT" "HISTORY-TOKENIZE" "RL-DIGIT-ARGUMENT"
   "RL-UNIVERSAL-ARGUMENT" "RL-FORWARD" "RL-BACKWARD" "RL-BEG-OF-LINE"
   "RL-END-OF-LINE" "RL-FORWARD-WORD" "RL-BACKWARD-WORD"
   "RL-REFRESH-LINE" "RL-CLEAR-SCREEN" "RL-ARROW-KEYS" "RL-INSERT"
   "RL-QUOTED-INSERT" "RL-TAB-INSERT" "RL-NEWLINE"
   "RL-DO-LOWERCASE-VERSION" "RL-RUBOUT" "RL-DELETE"
   "RL-RUBOUT-OR-DELETE" "RL-DELETE-HORIZONTAL-SPACE"
   "RL-DELETE-OR-SHOW-COMPLETIONS" "RL-INSERT-COMMENT" "RL-UPCASE-WORD"
   "RL-DOWNCASE-WORD" "RL-CAPITALIZE-WORD" "RL-TRANSPOSE-WORDS"
   "RL-TRANSPOSE-CHARS" "RL-CHAR-SEARCH" "RL-BACKWARD-CHAR-SEARCH"
   "RL-BEGINNING-OF-HISTORY" "RL-END-OF-HISTORY" "RL-GET-NEXT-HISTORY"
   "RL-GET-PREVIOUS-HISTORY" "RL-SET-MARK" "RL-EXCHANGE-POINT-AND-MARK"
   "RL-VI-EDITING-MODE" "RL-EMACS-EDITING-MODE" "RL-RE-READ-INIT-FILE"
   "RL-DUMP-FUNCTIONS" "RL-DUMP-MACROS" "RL-DUMP-VARIABLES" "RL-COMPLETE"
   "RL-POSSIBLE-COMPLETIONS" "RL-INSERT-COMPLETIONS" "RL-MENU-COMPLETE"
   "RL-KILL-WORD" "RL-BACKWARD-KILL-WORD" "RL-KILL-LINE"
   "RL-BACKWARD-KILL-LINE" "RL-KILL-FULL-LINE" "RL-UNIX-WORD-RUBOUT"
   "RL-UNIX-LINE-DISCARD" "RL-COPY-REGION-TO-KILL" "RL-KILL-REGION"
   "RL-COPY-FORWARD-WORD" "RL-COPY-BACKWARD-WORD" "RL-YANK" "RL-YANK-POP"
   "RL-YANK-NTH-ARG" "RL-YANK-LAST-ARG" "RL-REVERSE-SEARCH-HISTORY"
   "RL-FORWARD-SEARCH-HISTORY" "RL-START-KBD-MACRO" "RL-END-KBD-MACRO"
   "RL-CALL-LAST-KBD-MACRO" "RL-REVERT-LINE" "RL-UNDO-COMMAND"
   "RL-TILDE-EXPAND" "RL-RESTART-OUTPUT" "RL-STOP-OUTPUT" "RL-ABORT"
   "RL-TTY-STATUS" "RL-HISTORY-SEARCH-FORWARD"
   "RL-HISTORY-SEARCH-BACKWARD" "RL-NONINC-FORWARD-SEARCH"
   "RL-NONINC-REVERSE-SEARCH" "RL-NONINC-FORWARD-SEARCH-AGAIN"
   "RL-NONINC-REVERSE-SEARCH-AGAIN" "RL-INSERT-CLOSE"
   "RL-CALLBACK-HANDLER-INSTALL" "RL-CALLBACK-READ-CHAR"
   "RL-CALLBACK-HANDLER-REMOVE" "RL-VI-REDO" "RL-VI-UNDO"
   "RL-VI-YANK-ARG" "RL-VI-FETCH-HISTORY" "RL-VI-SEARCH-AGAIN"
   "RL-VI-SEARCH" "RL-VI-COMPLETE" "RL-VI-TILDE-EXPAND" "RL-VI-PREV-WORD"
   "RL-VI-NEXT-WORD" "RL-VI-END-WORD" "RL-VI-INSERT-BEG"
   "RL-VI-APPEND-MODE" "RL-VI-APPEND-EOL" "RL-VI-EOF-MAYBE"
   "RL-VI-INSERTION-MODE" "RL-VI-MOVEMENT-MODE" "RL-VI-ARG-DIGIT"
   "RL-VI-CHANGE-CASE" "RL-VI-PUT" "RL-VI-COLUMN" "RL-VI-DELETE-TO"
   "RL-VI-CHANGE-TO" "RL-VI-YANK-TO" "RL-VI-DELETE"
   "RL-VI-BACK-TO-INDENT" "RL-VI-FIRST-PRINT" "RL-VI-CHAR-SEARCH"
   "RL-VI-MATCH" "RL-VI-CHANGE-CHAR" "RL-VI-SUBST" "RL-VI-OVERSTRIKE"
   "RL-VI-OVERSTRIKE-DELETE" "RL-VI-REPLACE" "RL-VI-SET-MARK"
   "RL-VI-GOTO-MARK" "RL-VI-CHECK" "RL-VI-DOMOVE" "RL-VI-BRACKTYPE"
   "RL-VI-FWORD" "RL-VI-BWORD" "RL-VI-EWORD" "RL-VI-FWORD" "RL-VI-BWORD"
   "RL-VI-EWORD" "READLINE" "RL-SET-PROMPT" "RL-EXPAND-PROMPT"
   "RL-INITIALIZE" "RL-DISCARD-ARGUMENT" "RL-ADD-DEFUN" "RL-BIND-KEY"
   "RL-BIND-KEY-IN-MAP" "RL-UNBIND-KEY" "RL-UNBIND-KEY-IN-MAP"
   "RL-UNBIND-FUNCTION-IN-MAP" "RL-UNBIND-COMMAND-IN-MAP" "RL-SET-KEY"
   "RL-GENERIC-BIND" "RL-VARIABLE-BIND" "RL-MACRO-BIND"
   "RL-NAMED-FUNCTION" "RL-FUNCTION-OF-KEYSEQ" "RL-LIST-FUNMAP-NAMES"
   "RL-INVOKING-KEYSEQS-IN-MAP" "RL-INVOKING-KEYSEQS"
   "RL-FUNCTION-DUMPER" "RL-MACRO-DUMPER" "RL-VARIABLE-DUMPER"
   "RL-READ-INIT-FILE" "RL-PARSE-AND-BIND" "RL-MAKE-BARE-KEYMAP"
   "RL-COPY-KEYMAP" "RL-MAKE-KEYMAP" "RL-DISCARD-KEYMAP"
   "RL-GET-KEYMAP-BY-NAME" "RL-GET-KEYMAP-NAME" "RL-SET-KEYMAP"
   "RL-GET-KEYMAP" "RL-SET-KEYMAP-FROM-EDIT-MODE"
   "RL-GET-KEYMAP-NAME-FROM-EDIT-MODE" "RL-ADD-FUNMAP-ENTRY"
   "RL-FUNMAP-NAMES" "RL-PUSH-MACRO-INPUT" "RL-ADD-UNDO"
   "RL-FREE-UNDO-LIST" "RL-DO-UNDO" "RL-BEGIN-UNDO-GROUP"
   "RL-END-UNDO-GROUP" "RL-MODIFYING" "RL-REDISPLAY" "RL-ON-NEW-LINE"
   "RL-ON-NEW-LINE-WITH-PROMPT" "RL-FORCED-UPDATE-DISPLAY"
   "RL-CLEAR-MESSAGE" "RL-RESET-LINE-STATE" "RL-CRLF" "RL-SHOW-CHAR"
   "RL-SAVE-PROMPT" "RL-RESTORE-PROMPT" "RL-INSERT-TEXT" "RL-DELETE-TEXT"
   "RL-KILL-TEXT" "RL-COPY-TEXT" "RL-PREP-TERMINAL" "RL-DEPREP-TERMINAL"
   "RL-TTY-SET-DEFAULT-BINDINGS" "RL-RESET-TERMINAL" "RL-RESIZE-TERMINAL"
   "RL-SET-SCREEN-SIZE" "RL-GET-SCREEN-SIZE" "RL-STUFF-CHAR"
   "RL-EXECUTE-NEXT" "RL-CLEAR-PENDING-INPUT" "RL-READ-KEY" "RL-GETC"
   "RL-SET-KEYBOARD-INPUT-TIMEOUT" "RL-EXTEND-LINE-BUFFER" "RL-DING"
   "RL-ALPHABETIC" "RL-SET-SIGNALS" "RL-CLEAR-SIGNALS"
   "RL-CLEANUP-AFTER-SIGNAL" "RL-RESET-AFTER-SIGNAL" "RL-FREE-LINE-STATE"
   "RL-MAYBE-UNSAVE-LINE" "RL-MAYBE-REPLACE-LINE" "RL-COMPLETE-INTERNAL"
   "RL-DISPLAY-MATCH-LIST" "RL-COMPLETION-MATCHES"
   "RL-USERNAME-COMPLETION-FUNCTION" "RL-FILENAME-COMPLETION-FUNCTION"))
(in-package "COM.INFORMATIMAGO.SBCL.READLINE")



;;   typedef int Function ();

(defun c-null (ptr)
  "
RETURN:     Whether PTR is a C NULL pointer.
"
  (zerop (sap-int (alien-sap ptr)))
  ) ;;C-NULL



(defun initialize ()
  "
DO:         Loads the readline and history libraries.
"
  (load-foreign '("/usr/lib/libreadline.so"
                  "/usr/lib/libhistory.so"))
  ) ;;INITIALIZE






(define-alien-type file  (struct io-file))



;; Bindable functions 
(define-alien-type rl-command-func-t
    (function int int int ))


;; Typedefs for the completion system 

(define-alien-type rl-compentry-func-t
    (function c-string  c-string   int ))

(define-alien-type rl-completion-func-t
    (function (* c-string)  c-string   int   int ))

(define-alien-type rl-quote-func-t
    (function c-string  c-string   int   c-string ))

(define-alien-type rl-dequote-func-t
    (function c-string  c-string   int ))

(define-alien-type rl-compignore-func-t
    (function int  (* c-string) ))

(define-alien-type rl-compdisp-func-t
    (function void  (* c-string)   int   int ))


;; Type for input and pre-read hook functions like rl_event_hook 

(define-alien-type rl-hook-func-t
    (function int))


;; Input function type 

(define-alien-type rl-getc-func-t
    (function int  (* file) ))


;; Generic function that takes a character buffer (which could be the readline
;; line buffer) and an index into it (which could be rl_point) and returns
;; an int. 
(define-alien-type rl-linebuf-func-t
    (function int  c-string   int ))


;; `Generic' function pointer (DEFINE-ALIEN-TYPEs 
(define-alien-type rl-intfunc-t    (function int  int ))
(define-alien-type rl-ivoidfunc-t  (function int))
(define-alien-type rl-icpfunc-t    (function int  c-string ))
(define-alien-type rl-icppfunc-t   (function int  (* c-string) ))
(define-alien-type rl-voidfunc-t   (function void))
(define-alien-type rl-vintfunc-t   (function void  int ))
(define-alien-type rl-vcpfunc-t    (function void  c-string ))
(define-alien-type rl-vcppfunc-t   (function void  (* c-string) ))






(define-alien-type keymap-entry
    ;;  A keymap contains one entry for each key in the ASCII set.
    ;;    Each entry consists of a type and a pointer.
    ;;    FUNCTION is the address of a function to run, or the
    ;;    address of a keymap to indirect through.
    ;;    TYPE says which kind of thing FUNCTION is. 
    (struct keymap-entry
      (type char)
      (function (* rl-command-func-t)))) ;;KEYMAP-ENTRY


(defconstant keymap-size 256
  "This must be large enough to hold bindings for all of the characters
in a desired character set (e.g, 128 for ASCII, 256 for ISO Latin-x,
and so on).")



(define-alien-type keymap-entry-array (array keymap-entry 256))
(define-alien-type keymap (* keymap-entry))

;;  The values that TYPE can have in a keymap entry. 
(defconstant isfunc (code-char 0))
(defconstant iskmap (code-char 1))
(defconstant ismacr (code-char 2))



;; ----------------------------------------------------------------------
;; readline/history.h


(define-alien-type histdata-t (* t))

(define-alien-type hist-entry
    ;; The structure used to store a history entry.
    (struct hist-entry
      (line c-string)
      (data histdata-t))) ;;HIST-ENTRY


(define-alien-type history-state
    ;; A structure used to pass the current state of the history stuff around. 
    (struct hist-state
      (entries (* (* hist-entry))) ;; Pointer to the entries themselves. 
      (offset int) ;; The location pointer within this array. 
      (length int) ;; Number of elements within this array. 
      (size int)   ;; Number of slots allocated to this array. 
      (flags int)
      )) ;;HISTORY-STATE


(defconstant hs-stifled 1
  "Flag values for the `FLAGS' member of HISTORY-STATE.")


;; Initialization and state management. 

(define-alien-routine "using_history" void
  ;; Begin a session in which the history functions might be used.  This
  ;; just initializes the interactive variables.
  )


(define-alien-routine "history_get_history_state" (* history-state)
  ;; Return the current HISTORY_STATE of the history. 
  )


(define-alien-routine "history_set_history_state" void
  ( state (* history-state))
  ;; Set the state of the current history array to STATE.
  )


;; Manage the history list.
  
(define-alien-routine "add_history" void
  (string c-string :in)
  ;; Place STRING at the end of the history list.
  ;; The associated data field (if any) is set to NULL. 
  ) ;;add_history


(define-alien-routine "remove_history" (* hist-entry)
  (which int :in)
  ;; A reasonably useless function, only here for completeness.  WHICH
  ;; is the magic number that tells us which element to delete.  The
  ;; elements are numbered from 0. 
  ) ;;remove_history


(define-alien-routine "replace_history_entry" (* hist-entry)
  (which int        :in)
  (line  c-string   :in)
  (data  histdata-t :in)
  ;; Make the history entry at WHICH have LINE and DATA.  This returns
  ;; the old entry so you can dispose of the data.  In the case of an
  ;; invalid WHICH, a NULL pointer is returned. 
  ) ;;replace_history_entry
   

(define-alien-routine "clear_history" void
  ;; Clear the history list and start over. 
  )


(define-alien-routine "stifle_history" void
  (max int :in)
  ;; Stifle the history list, remembering only MAX number of entries. 
  )


(define-alien-routine "unstifle_history" int
  ;; Stop stifling the history.  This returns the previous amount the
  ;; history was stifled by.  The value is positive if the history was
  ;; stifled, negative if it wasn't.
  ) ;;unstifle_history


(define-alien-routine "history_is_stifled" int
  ;; Return 1 if the history is stifled, 0 if it is not. 
  )


;; Information about the history list. 


(define-alien-routine  "history_list" (* (* hist-entry))
  ;; Return a NULL terminated array of HIST-ENTRY which is the current input
  ;; history.  Element 0 of this list is the beginning of time.  If there
  ;; is no history, return NULL.
  ) ;;history_list


(define-alien-routine "where_history" int
  ;; Returns the number which says what history element we are now
  ;; looking at.  
  )


(define-alien-routine "current_history" (* hist-entry)
  ;; Return the history entry at the current position, as determined by
  ;; history_offset.  If there is no entry there, return a NULL pointer. 
  )


(define-alien-routine "history_get" (* hist-entry)
  (offset int :in)
  ;; Return the history entry which is logically at OFFSET in the history
  ;; array.  OFFSET is relative to history_base.
  ) ;;history_get


(define-alien-routine "history_total_bytes" int
  ;; Return the number of bytes that the primary history entries are using.
  ;; This just adds up the lengths of the_history->lines. 
  )


;; Moving around the history list. 


(define-alien-routine "history_set_pos" int
  (pos int :in)
  ;; Set the position in the history list to POS. 
  )


(define-alien-routine "previous_history" (* hist-entry)
  ;; Back up history_offset to the previous history entry, and return
  ;; a pointer to that entry.  If there is no previous entry, return
  ;; a NULL pointer. 
  ) ;;previous_history


(define-alien-routine "next_history" (* hist-entry)
  ;; Move history_offset forward to the next item in the input_history,
  ;; and return the a pointer to that entry.  If there is no next entry,
  ;; return a NULL pointer. 
  ) ;;next_history


;; Searching the history list. 

(define-alien-routine "history_search" int
  (string    c-string :in)
  (direction int      :in)
  ;; Search the history for STRING, starting at history_offset.
  ;; If DIRECTION < 0, then the search is through previous entries,
  ;; else through subsequent.  If the string is found, then
  ;; current_history () is the history entry, and the value of this function
  ;; is the offset in the line of that history entry that the string was
  ;; found in.  Otherwise, nothing is changed, and a -1 is returned. 
  ) ;;history_search


(define-alien-routine "history_search_prefix" int
  (string    c-string :in)
  (direction int      :in)
  ;; Search the history for STRING, starting at history_offset.
  ;; The search is anchored: matching lines must begin with string.
  ;; DIRECTION is as in history_search(). 
  ) ;;history_search_prefix


(define-alien-routine "history_search_pos" int
  (string c-string :in)
  (dir    int      :in)
  (pos    int      :in)
  ;; Search for STRING in the history list, starting at POS, an
  ;; absolute index into the list.  DIR, if negative, says to search
  ;; backwards from POS, else forwards.
  ;; Returns the absolute index of the history element where STRING
  ;; was found, or -1 otherwise. 
  ) ;;history_search_pos


;; Managing the history file. 

(define-alien-routine "read_history" int
  (filename c-string :in)
  ;; Add the contents of FILENAME to the history list, a line at a time.
  ;; If FILENAME is NULL, then read from ~/.history.  Returns 0 if
  ;; successful, or errno if not. 
  ) ;;read_history


(define-alien-routine "read_history_range" int
  (filename c-string :in)
  (from     int      :in)
  (to       int      :in)
  ;; Read a range of lines from FILENAME, adding them to the history list.
  ;; Start reading at the FROM'th line and end at the TO'th.  If FROM
  ;; is zero, start at the beginning.  If TO is less than FROM, read
  ;; until the end of the file.  If FILENAME is NULL, then read from
  ;; ~/.history.  Returns 0 if successful, or errno if not. 
  ) ;;read_history_range


(define-alien-routine "write_history" int
  (filename c-string :in)
  ;; Write the current history to FILENAME.  If FILENAME is NULL,
  ;; then write the history list to ~/.history.  Values returned
  ;; are as in read_history ().  
  ) ;;write_history


(define-alien-routine "append_history" int
  (nelement int      :in)
  (filename c-string :in)
  ;; Append NELEMENT entries to FILENAME.  The entries appended are from
  ;; the end of the list minus NELEMENTs up to the end of the list. 
  ) ;;append_history


(define-alien-routine "history_truncate_file" int
  (filename c-string :in)
  (nlines   int      :in)
  ;; Truncate the history file, leaving only the last NLINES lines. 
  ) ;;history_truncate_file


;; History expansion. 

(define-alien-routine "history_expand" int
  (string c-string :in)
  (output (* c-string) :in) ;; actually :OUT !
  ;; Expand the string STRING, placing the result into OUTPUT, a pointer
  ;; to a string.  Returns:
  ;;
  ;; 0) If no expansions took place (or, if the only change in
  ;; the text was the de-slashifying of the history expansion
  ;; character)
  ;; 1) If expansions did take place
  ;;-1) If there was an error in expansion.
  ;; 2) If the returned line should just be printed.
  ;;
  ;;If an error ocurred in expansion, then OUTPUT contains a descriptive
  ;;error message. 
  ) ;;history_expand


(define-alien-routine "history_arg_extract" c-string
  (first  int      :in)
  (last   int      :in)
  (string c-string :in)
  ;; Extract a string segment consisting of the FIRST through LAST
  ;; arguments present in STRING.  Arguments are broken up as in
  ;; the shell. 
  ) ;;history_arg_extract


(define-alien-routine "get_history_event" c-string
  (string c-string       :in)
  (index  int            :in-out)
  (delimiting-quote char :in)
  ;; Return the text of the history event beginning at the current
  ;; offset into STRING.  Pass STRING with *INDEX equal to the
  ;; history_expansion_char that begins this specification.
  ;; DELIMITING_QUOTE is a character that is allowed to end the string
  ;; specification for what to search for in addition to the normal
  ;; characters `:', ` ', `\t', `\n', and sometimes `?'. 
  ) ;;get_history_event


(define-alien-routine "history_tokenize" (* c-string)
  (string c-string :in)
  ;; Return an array of tokens, much as the shell might.  The tokens are
  ;; parsed out of STRING. 
  ) ;;history_tokenize


(declaim
 (inline
   using-history history-get-history-state history-set-history-state
   add-history remove-history replace-history-entry clear-history
   stifle-history unstifle-history history-is-stifled history-list
   where-history current-history history-get history-total-bytes
   history-set-pos previous-history next-history history-search
   history-search-prefix history-search-pos read-history
   read-history-range write-history append-history
   history-truncate-file history-expand history-arg-extract
   get-history-event history-tokenize ))

;; Exported history variables.

(define-alien-variable "history_base" int)
(define-alien-variable "history_length" int)
(define-alien-variable "history_max_entries" int)
(define-alien-variable "history_expansion_char" char)
(define-alien-variable "history_subst_char" char)
(define-alien-variable "history_word_delimiters" c-string)
(define-alien-variable "history_comment_char" char)
(define-alien-variable "history_no_expand_chars" c-string)
(define-alien-variable "history_search_delimiter_chars" c-string)
(define-alien-variable "history_quotes_inhibit_expansion" int)


(define-alien-variable "max_input_history" int
  ;; Backwards compatibility
  )


(define-alien-variable "history_inhibit_expansion_function"
    (* rl-linebuf-func-t)
  ;; If set, this function is called to decide whether or not a particular
  ;; history expansion should be treated as a special case for the calling
  ;; application and not expanded. 
  ) ;;history_inhibit_expansion_function




;; ----------------------------------------------------------------------
;; readline/readline.h




;; Readline data structures. 

;; Maintaining the state of undo.  We remember individual deletes and inserts
;; on a chain of things to do. 

(define-alien-type undo-code
    ;; The actions that undo knows how to undo.  Notice that UNDO_DELETE means
    ;; to insert some text, and UNDO_INSERT means to delete some text.   I.e.,
    ;; the code tells undo what to undo, not how to undo it. 
    (enum undo-code
          :undo-delete :undo-insert :undo-begin :undo-end)
  ) ;;UNDO-CODE


(define-alien-type undo-list
    ;; What an element of THE_UNDO_LIST looks like. 
    (struct undo-list
      (next (* (struct undo-list)))
      (start int) ;; Where the change took place.
      (end int)
      (text  c-string) ;; The text to insert, if undoing a delete. 
      (what (enum undo-code)) ;; Delete, Insert, Begin, End. 
      ))                      ;;UNDO-LIST


(define-alien-variable "rl_undo_list" 
    ;; The current undo list for RL_LINE_BUFFER. 
    (* undo-list))


(define-alien-type funmap
    ;; The data structure for mapping textual names to code addresses.
    (struct funmap
      (name c-string)
      (function (* rl-command-func-t)))) ;;FUNMAP

(define-alien-variable "funmap" (* (* funmap)))
;; SHOULD BE AN ARRAY OF (* FUNMAP)...


;; **************************************************************** 
;; 								    
;; 	     Functions available to bind to key sequences	    
;; 								    
;; **************************************************************** 


;; Bindable commands for numeric arguments.

(define-alien-routine "rl_digit_argument" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_universal_argument" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable commands for moving the cursor.

(define-alien-routine "rl_forward" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_backward" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_beg_of_line" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_end_of_line" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_forward_word" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_backward_word" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_refresh_line" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_clear_screen" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_arrow_keys" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable commands for inserting and deleting text. 

(define-alien-routine "rl_insert" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_quoted_insert" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_tab_insert" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_newline" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_do_lowercase_version" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_rubout" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_delete" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_rubout_or_delete" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_delete_horizontal_space" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_delete_or_show_completions" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_insert_comment" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable commands for changing case. 

(define-alien-routine "rl_upcase_word" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_downcase_word" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_capitalize_word" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable commands for transposing characters and words. 

(define-alien-routine "rl_transpose_words" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_transpose_chars" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable commands for searching within a line. 

(define-alien-routine "rl_char_search" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_backward_char_search" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable commands for readline's interface to the command history. 

(define-alien-routine "rl_beginning_of_history" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_end_of_history" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_get_next_history" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_get_previous_history" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable commands for managing the mark and region. 

(define-alien-routine "rl_set_mark" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_exchange_point_and_mark" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable commands to set the editing mode (emacs or vi). 

(define-alien-routine "rl_vi_editing_mode" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_emacs_editing_mode" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable commands for managing key bindings. 

(define-alien-routine "rl_re_read_init_file" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_dump_functions" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_dump_macros" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_dump_variables" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable commands for word completion. 

(define-alien-routine "rl_complete" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_possible_completions" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_insert_completions" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_menu_complete" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable commands for killing and yanking text, and managing the kill ring. 

(define-alien-routine "rl_kill_word" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_backward_kill_word" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_kill_line" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_backward_kill_line" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_kill_full_line" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_unix_word_rubout" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_unix_line_discard" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_copy_region_to_kill" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_kill_region" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_copy_forward_word" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_copy_backward_word" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_yank" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_yank_pop" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_yank_nth_arg" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_yank_last_arg" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable commands for incremental searching. 

(define-alien-routine "rl_reverse_search_history" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_forward_search_history" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable keyboard macro commands. 

(define-alien-routine "rl_start_kbd_macro" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_end_kbd_macro" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_call_last_kbd_macro" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable undo commands. 

(define-alien-routine "rl_revert_line" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_undo_command" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable tilde expansion commands. 

(define-alien-routine "rl_tilde_expand" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable terminal control commands. 

(define-alien-routine "rl_restart_output" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_stop_output" int
  (count  int  :in)
  (key    int  :in)
  )


;; Miscellaneous bindable commands. 

(define-alien-routine "rl_abort" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_tty_status" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable commands for incremental and non-incremental history searching. 

(define-alien-routine "rl_history_search_forward" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_history_search_backward" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_noninc_forward_search" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_noninc_reverse_search" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_noninc_forward_search_again" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_noninc_reverse_search_again" int
  (count  int  :in)
  (key    int  :in)
  )


;; Bindable command used when inserting a matching close character. 

(define-alien-routine "rl_insert_close" int
  (count  int  :in)
  (key    int  :in)
  )


;; Not available unless READLINE_CALLBACKS is defined. 

(define-alien-routine "rl_callback_handler_install" void
  (name      c-string          :in)
  (function  (* rl-vcpfunc-t)  :in)
  )

(define-alien-routine "rl_callback_read_char" void )

(define-alien-routine "rl_callback_handler_remove" void )


;; Things for vi mode. Not available unless readline is compiled -DVI_MODE. 
;; VI-mode bindable commands. 

(define-alien-routine "rl_vi_redo" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_undo" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_yank_arg" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_fetch_history" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_search_again" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_search" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_complete" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_tilde_expand" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_prev_word" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_next_word" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_end_word" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_insert_beg" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_append_mode" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_append_eol" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_eof_maybe" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_insertion_mode" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_movement_mode" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_arg_digit" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_change_case" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_put" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_column" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_delete_to" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_change_to" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_yank_to" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_delete" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_back_to_indent" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_first_print" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_char_search" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_match" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_change_char" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_subst" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_overstrike" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_overstrike_delete" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_replace" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_set_mark" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_goto_mark" int
  (count  int  :in)
  (key    int  :in)
  )


;; VI-mode utility functions. 

(define-alien-routine "rl_vi_check" int)


(define-alien-routine "rl_vi_domove" int
  (a  int  :in)
  (b  int  :in-out)
  )


(define-alien-routine "rl_vi_bracktype" int
  (a  int  :in)
  )


;; VI-mode pseudo-bindable commands, used as utility functions. 

;;; (DEFINE-ALIEN-ROUTINE "rl_vi_fWord" INT
;;;   (COUNT  INT  :IN)
;;;   (KEY    INT  :IN)
;;;  )
;;; 
;;; 
;;; (DEFINE-ALIEN-ROUTINE "rl_vi_bWord" INT
;;;   (COUNT  INT  :IN)
;;;   (KEY    INT  :IN)
;;;  )
;;; 
;;; 
;;; (DEFINE-ALIEN-ROUTINE "rl_vi_eWord" INT
;;;   (COUNT  INT  :IN)
;;;   (KEY    INT  :IN)
;;;  )


(define-alien-routine "rl_vi_fword" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_bword" int
  (count  int  :in)
  (key    int  :in)
  )


(define-alien-routine "rl_vi_eword" int
  (count  int  :in)
  (key    int  :in)
  )


;; **************************************************************** 
;; 								    
;; 			Well Published Functions		    
;; 								    
;; **************************************************************** 

;; Readline functions. 
;; Read a line of input.  Prompt with PROMPT.  A NULL PROMPT means none. 

(define-alien-routine "readline" c-string
  (prompt c-string :in)
  )



(define-alien-routine "rl_set_prompt" int
  (prompt c-string :in)
  )


(define-alien-routine "rl_expand_prompt" int
  (prompt c-string :in)
  )



(define-alien-routine "rl_initialize" int)



;; Undocumented; unused by readline 

(define-alien-routine "rl_discard_argument" int )


;; Utility functions to bind keys to readline commands. 

(define-alien-routine "rl_add_defun" int
  (name     c-string              :in)
  (function (* rl-command-func-t) :in)
  (key      int                   :in)
  ) ;;rl_add_defun


(define-alien-routine "rl_bind_key" int
  (key       int                    :in)
  (function  (* rl-command-func-t)  :in)
  )


(define-alien-routine "rl_bind_key_in_map" int
  (key       int                    :in)
  (function  (* rl-command-func-t)  :in)
  (map       keymap                 :in)
  ) ;;rl_bind_key_in_map


(define-alien-routine "rl_unbind_key" int
  (key  int  :in)
  )


(define-alien-routine "rl_unbind_key_in_map" int
  (key  int  :in)
  (map  keymap  :in)
  )


(define-alien-routine "rl_unbind_function_in_map" int
  (function  (* rl-command-func-t)  :in)
  (map  keymap  :in)
  )


(define-alien-routine "rl_unbind_command_in_map" int
  (command  c-string  :in)
  (map  keymap  :in)
  )


(define-alien-routine "rl_set_key" int
  (keyseq c-string  :in)
  (function  (* rl-command-func-t)  :in)
  (map  keymap  :in)
  ) ;;rl_set_key


(define-alien-routine "rl_generic_bind" int
  (type  int  :in)
  (keyseq  c-string   :in)
  (data  (* char)  :in)
  (map  keymap  :in)
  ) ;;rl_generic_bind


(define-alien-routine "rl_variable_bind" int
  (variable  c-string   :in)
  (value  c-string   :in)
  )


;; Backwards compatibility, use rl_generic_bind instead. 

(define-alien-routine "rl_macro_bind" int
  (keyseq  c-string   :in)
  (macro   c-string   :in)
  (map     keymap     :in)
  ) ;;rl_macro_bind



(define-alien-routine "rl_named_function" (* rl-command-func-t)
  (name  c-string   :in)
  )


(define-alien-routine "rl_function_of_keyseq" (* rl-command-func-t)
  (keyseq  c-string   :in)
  (map  keymap  :in)
  (type int   :out)
  ) ;;rl_function_of_keyseq



(define-alien-routine "rl_list_funmap_names" void )


(define-alien-routine "rl_invoking_keyseqs_in_map" (* (* char))
  (function  (* rl-command-func-t)  :in)
  (map  keymap  :in)
  )


(define-alien-routine "rl_invoking_keyseqs" (* (* char))
  (function  (* rl-command-func-t)  :in)
  )

 

(define-alien-routine "rl_function_dumper" void
  (readable  int  :in)
  )


(define-alien-routine "rl_macro_dumper" void
  (readable  int  :in)
  )


(define-alien-routine "rl_variable_dumper" void
  (readable  int  :in)
  )



(define-alien-routine "rl_read_init_file" int
  (filename  c-string   :in)
  )


(define-alien-routine "rl_parse_and_bind" int
  (line  c-string  :in)
  )


;; Functions for manipulating keymaps. 

(define-alien-routine "rl_make_bare_keymap" keymap  )


(define-alien-routine "rl_copy_keymap" keymap
  (map  keymap  :in)
  )


(define-alien-routine "rl_make_keymap" keymap )


(define-alien-routine "rl_discard_keymap" void
  (map  keymap  :in)
  )



(define-alien-routine "rl_get_keymap_by_name" keymap
  (name  c-string   :in)
  )


(define-alien-routine "rl_get_keymap_name" c-string
  (map  keymap  :in)
  )


(define-alien-routine "rl_set_keymap" void
  (map  keymap  :in)
  )


(define-alien-routine "rl_get_keymap" keymap )

;; Undocumented; used internally only. 

(define-alien-routine "rl_set_keymap_from_edit_mode" void )


(define-alien-routine "rl_get_keymap_name_from_edit_mode" c-string )


;; Functions for manipulating the funmap, which maps command names to functions. 

(define-alien-routine "rl_add_funmap_entry" int
  (name  c-string   :in)
  (function  (* rl-command-func-t)  :in)
  )


(define-alien-routine "rl_funmap_names" (* c-string) )



;; Utility functions for managing keyboard macros. 

(define-alien-routine "rl_push_macro_input" void
  (macro c-string  :in)
  )


;; Functions for undoing, from undo.c 

(define-alien-routine "rl_add_undo" void
  (what (enum undo-code)  :in)
  (start  int    :in)
  (end    int    :in)
  (text c-string :in)
  ) ;;rl_add_undo


(define-alien-routine "rl_free_undo_list" void)


(define-alien-routine "rl_do_undo" int )


(define-alien-routine "rl_begin_undo_group" int )


(define-alien-routine "rl_end_undo_group" int )


(define-alien-routine "rl_modifying" int
  (start  int  :in)
  (end    int  :in)
  )


;; Functions for redisplay. 

(define-alien-routine "rl_redisplay" void)


(define-alien-routine "rl_on_new_line" int )


(define-alien-routine "rl_on_new_line_with_prompt" int )


(define-alien-routine "rl_forced_update_display" int )


(define-alien-routine "rl_clear_message" int )


(define-alien-routine "rl_reset_line_state" int )


(define-alien-routine "rl_crlf" int )


;; extern int rl_message (c-string , ...);


(define-alien-routine "rl_show_char" int
  (c  int  :in)
  )


;; Save and restore internal prompt redisplay information. 

(define-alien-routine "rl_save_prompt" void )


(define-alien-routine "rl_restore_prompt" void )


;; Modifying text. 

(define-alien-routine "rl_insert_text" int
  (text  c-string   :in)
  )


(define-alien-routine "rl_delete_text" int
  (start  int  :in)
  (end    int  :in)
  )


(define-alien-routine "rl_kill_text" int
  (start  int  :in)
  (end    int  :in)
  )


(define-alien-routine "rl_copy_text" c-string
  (start  int  :in)
  (end    int  :in)
  )


;; Terminal and tty mode management. 

(define-alien-routine "rl_prep_terminal" void
  (meta-flag  int  :in)
  )


(define-alien-routine "rl_deprep_terminal" void )


(define-alien-routine "rl_tty_set_default_bindings" void
  (map  keymap  :in)
  )



(define-alien-routine "rl_reset_terminal" int
  (terminal-name  c-string   :in)
  )


(define-alien-routine "rl_resize_terminal" void )


(define-alien-routine "rl_set_screen_size" void
  (rows  int  :in)
  (cols  int  :in)
  )


(define-alien-routine "rl_get_screen_size" void
  (rows  int :out)
  (cols  int :out)
  )


;; Functions for character input. 

(define-alien-routine "rl_stuff_char" int
  (c  int  :in)
  )


(define-alien-routine "rl_execute_next" int
  (c  int  :in)
  )


(define-alien-routine "rl_clear_pending_input" int )


(define-alien-routine "rl_read_key" int )


(define-alien-routine "rl_getc" int
  (stream  (* file)  :in)
  )


(define-alien-routine "rl_set_keyboard_input_timeout" int
  (u  int  :in)
  )


;; `Public' utility functions . 

(define-alien-routine "rl_extend_line_buffer" void
  (len  int  :in)
  )


(define-alien-routine "rl_ding" int )


(define-alien-routine "rl_alphabetic" int
  (c  int  :in)
  )


;; Readline signal handling, from signals.c 

(define-alien-routine "rl_set_signals" int)


(define-alien-routine "rl_clear_signals" int )


(define-alien-routine "rl_cleanup_after_signal" void )


(define-alien-routine "rl_reset_after_signal" void )


(define-alien-routine "rl_free_line_state" void )

 


(define-alien-routine "rl_maybe_unsave_line" int )


(define-alien-routine "rl_maybe_replace_line" int )


;; Completion functions. 

(define-alien-routine "rl_complete_internal" int
  (what-to-do  int  :in)
  )


(define-alien-routine "rl_display_match_list" void
  (matches  (* c-string)  :in)
  (len    int  :in)
  (max    int  :in)
  ) ;;rl_display_match_list



(define-alien-routine "rl_completion_matches" (* c-string)
  (text  c-string   :in)
  (entry-func (* rl-compentry-func-t)  :in)
  )


(define-alien-routine "rl_username_completion_function" c-string
  (text  c-string   :in)
  (state int  :in)
  )


(define-alien-routine "rl_filename_completion_function" c-string
  (text  c-string   :in)
  (state int  :in)
  )



;; **************************************************************** 
;; 								    
;; 			Well Published Variables		    
;; 								    
;; **************************************************************** 

;; The version of this incarnation of the readline library. 
(define-alien-variable "rl_library_version" c-string)


;; True if this is real GNU readline. 
(define-alien-variable "rl_gnu_readline_p" int)


;; Flags word encapsulating the current readline state. 
(define-alien-variable "rl_readline_state" int)


;; Says which editing mode readline is currently using.  1 means emacs mode;
;; 0 means vi mode. 
(define-alien-variable "rl_editing_mode" int)


;; The name of the calling program.  You should initialize this to
;; whatever was in argv[0].  It is used when parsing conditionals. 
(define-alien-variable "rl_readline_name" c-string)


;; The prompt readline uses.  This is set from the argument to
;; readline (), and should not be assigned to directly. 
(define-alien-variable "rl_prompt" c-string)


;; The line buffer that is in use. 
(define-alien-variable "rl_line_buffer" c-string)


;; The location of point, and end. 
(define-alien-variable "rl_point" int)

(define-alien-variable "rl_end" int)


;; The mark, or saved cursor position. 
(define-alien-variable "rl_mark" int)


;; Flag to indicate that readline has finished with the current input
;; line and should return it. 
(define-alien-variable "rl_done" int)


;; If set to a character value, that will be the next keystroke read. 
(define-alien-variable "rl_pending_input" int)


;; Non-zero if we called this function from _rl_dispatch().  It's present
;; so functions can find out whether they were called from a key binding
;; or directly from an application. 
(define-alien-variable "rl_dispatching" int)


;; Non-zero if the user typed a numeric argument before executing the
;; current function. 
(define-alien-variable "rl_explicit_arg" int)


;; The current value of the numeric argument specified by the user. 
(define-alien-variable "rl_numeric_arg" int)


;; The address of the last command function Readline executed. 
(define-alien-variable "rl_last_func" (* rl-command-func-t))


;; The name of the terminal to use. 
(define-alien-variable "rl_terminal_name" c-string)


;; The input and output streams. 
(define-alien-variable "rl_instream" (* file))

(define-alien-variable "rl_outstream" (* file))


;; If non-zero, then this is the address of a function to call just
;; before readline_internal () prints the first prompt. 
(define-alien-variable "rl_startup_hook" (* rl-hook-func-t))


;; If non-zero, this is the address of a function to call just before
;; readline_internal_setup () returns and readline_internal starts
;; reading input characters. 
(define-alien-variable "rl_pre_input_hook" (* rl-hook-func-t))

      
;; The address of a function to call periodically while Readline is
;; awaiting character input, or NULL, for no event handling. 
(define-alien-variable "rl_event_hook" (* rl-hook-func-t))


;; The address of the function to call to fetch a character from the current
;; Readline input stream 
(define-alien-variable "rl_getc_function" (* rl-getc-func-t))


(define-alien-variable "rl_redisplay_function" (* rl-voidfunc-t))


(define-alien-variable "rl_prep_term_function" (* rl-vintfunc-t))

(define-alien-variable "rl_deprep_term_function" (* rl-voidfunc-t))


;; Dispatch variables. 
(define-alien-variable "rl_executing_keymap" keymap)

(define-alien-variable "rl_binding_keymap" keymap)


;; Display variables. 
;; If non-zero, readline will erase the entire line, including any prompt,
;; if the only thing typed on an otherwise-blank line is something bound to
;; rl_newline. 
(define-alien-variable "rl_erase_empty_line" int)


;; If non-zero, the application has already printed the prompt (rl_prompt)
;; before calling readline, so readline should not output it the first time
;; redisplay is done. 
(define-alien-variable "rl_already_prompted" int)


;; A non-zero value means to read only this many characters rather than
;; up to a character bound to accept-line. 
(define-alien-variable "rl_num_chars_to_read" int)


;; The text of a currently-executing keyboard macro. 
(define-alien-variable "rl_executing_macro" (* char))


;; Variables to control readline signal handling. 
;; If non-zero, readline will install its own signal handlers for
;; SIGINT, SIGTERM, SIGQUIT, SIGALRM, SIGTSTP, SIGTTIN, and SIGTTOU. 
(define-alien-variable "rl_catch_signals" int)


;; If non-zero, readline will install a signal handler for SIGWINCH
;; that also attempts to call any calling application's SIGWINCH signal
;; handler.  Note that the terminal is not cleaned up before the
;; application's signal handler is called; use rl_cleanup_after_signal()
;; to do that. 
(define-alien-variable "rl_catch_sigwinch" int)


;; Completion variables. 
;; Pointer to the generator function for completion_matches ().
;; NULL means to use filename_entry_function (), the default filename
;; completer. 
(define-alien-variable "rl_completion_entry_function" (* rl-compentry-func-t))


;; If rl_ignore_some_completions_function is non-NULL it is the address
;; of a function to call after all of the possible matches have been
;; generated, but before the actual completion is done to the input line.
;; The function is called with one argument; a NULL terminated array
;; of (char *).  If your function removes any of the elements, they
;; must be free()'ed. 
(define-alien-variable "rl_ignore_some_completions_function" (* rl-compignore-func-t))


;; Pointer to alternative function to create matches.
;; Function is called with TEXT, START, and END.
;; START and END are indices in RL_LINE_BUFFER saying what the boundaries
;; of TEXT are.
;; If this function exists and returns NULL then call the value of
;; rl_completion_entry_function to try to match, otherwise use the
;; array of strings returned. 
(define-alien-variable "rl_attempted_completion_function" (* rl-completion-func-t))


;; The basic list of characters that signal a break between words for the
;; completer routine.  The initial contents of this variable is what
;; breaks words in the shell, i.e. "n\"\\'`@$>". 
(define-alien-variable "rl_basic_word_break_characters" c-string)


;; The list of characters that signal a break between words for
;; rl_complete_internal.  The default list is the contents of
;; rl_basic_word_break_characters.  
(define-alien-variable "rl_completer_word_break_characters" c-string)


;; List of characters which can be used to quote a substring of the line.
;; Completion occurs on the entire substring, and within the substring   
;; rl_completer_word_break_characters are treated as any other character,
;; unless they also appear within this list. 
(define-alien-variable "rl_completer_quote_characters" c-string)


;; List of quote characters which cause a word break. 
(define-alien-variable "rl_basic_quote_characters" c-string)


;; List of characters that need to be quoted in filenames by the completer. 
(define-alien-variable "rl_filename_quote_characters" c-string)


;; List of characters that are word break characters, but should be left
;; in TEXT when it is passed to the completion function.  The shell uses
;; this to help determine what kind of completing to do. 
(define-alien-variable "rl_special_prefixes" c-string)


;; If non-zero, then this is the address of a function to call when
;; completing on a directory name.  The function is called with
;; the address of a string (the current directory name) as an arg.  It
;; changes what is displayed when the possible completions are printed
;; or inserted. 
(define-alien-variable "rl_directory_completion_hook" (* rl-icppfunc-t))


;; If non-zero, this is the address of a function to call when completing
;; a directory name.  This function takes the address of the directory name
;; to be modified as an argument.  Unlike rl_directory_completion_hook, it
;; only modifies the directory name used in opendir(2), not what is displayed
;; when the possible completions are printed or inserted.  It is called
;; before rl_directory_completion_hook.  I'm not happy with how this works
;; yet, so it's undocumented. 
(define-alien-variable "rl_directory_rewrite_hook" (* rl-icppfunc-t))


;; If non-zero, then this is the address of a function to call when
;; completing a word would normally display the list of possible matches.
;; This function is called instead of actually doing the display.
;; It takes three arguments: (char **matches, int num_matches, int max_length)
;; where MATCHES is the array of strings that matched, NUM_MATCHES is the
;; number of strings in that array, and MAX_LENGTH is the length of the
;; longest string in that array. 
(define-alien-variable "rl_completion_display_matches_hook" (* rl-compdisp-func-t))


;; Non-zero means that the results of the matches are to be treated
;; as filenames.  This is ALWAYS zero on entry, and can only be changed
;; within a completion entry finder function. 
(define-alien-variable "rl_filename_completion_desired" int)


;; Non-zero means that the results of the matches are to be quoted using
;; double quotes (or an application-specific quoting mechanism) if the
;; filename contains any characters in rl_word_break_chars.  This is
;; ALWAYS non-zero on entry, and can only be changed within a completion
;; entry finder function. 
(define-alien-variable "rl_filename_quoting_desired" int)


;; Set to a function to quote a filename in an application-specific fashion.
;; Called with the text to quote, the type of match found (single or multiple)
;; and a pointer to the quoting character to be used, which the function can
;; reset if desired. 
(define-alien-variable "rl_filename_quoting_function" (* rl-quote-func-t))


;; Function to call to remove quoting characters from a filename.  Called
;; before completion is attempted, so the embedded quotes do not interfere
;; with matching names in the file system. 
(define-alien-variable "rl_filename_dequoting_function" (* rl-dequote-func-t))


;; Function to call to decide whether or not a word break character is
;; quoted.  If a character is quoted, it does not break words for the
;; completer. 
(define-alien-variable "rl_char_is_quoted_p" (* rl-linebuf-func-t))


;; Non-zero means to suppress normal filename completion after the
;; user-specified completion function has been called. 
(define-alien-variable "rl_attempted_completion_over" int)


;; Set to a character describing the type of completion being attempted by
;; rl_complete_internal; available for use by application completion
;; functions. 
(define-alien-variable "rl_completion_type" int)


;; Character appended to completed words when at the end of the line.  The
;; default is a space.  Nothing is added if this is '\0'. 
(define-alien-variable "rl_completion_append_character" int)


;; Up to this many items will be displayed in response to a
;; possible-completions call.  After that, we ask the user if she
;; is sure she wants to see them all.  The default value is 100. 
(define-alien-variable "rl_completion_query_items" int)


;; If non-zero, then disallow duplicates in the matches. 
(define-alien-variable "rl_ignore_completion_duplicates" int)


;; If this is non-zero, completion is (temporarily) inhibited, and the
;; completion character will be inserted as any other. 
(define-alien-variable "rl_inhibit_completion" int)

   
;; Definitions available for use by readline clients. 
(defconstant rl-prompt-start-ignore (code-char 1))
(defconstant rl-prompt-end-ignore   (code-char 2))



;; Possible values for do-replace argument to rl-filename-quoting-function,
;; called by rl-complete-internal. 
(defconstant no-match     0)
(defconstant single-match 1)
(defconstant mult-match   2)



;; Possible state values for rl-readline-state 
(defconstant rl-state-none #x00000  "no state; before first call ")
(defconstant rl-state-initializing #x00001  "initializing ")
(defconstant rl-state-initialized #x00002  "initialization done ")
(defconstant rl-state-termprepped #x00004  "terminal is prepped ")
(defconstant rl-state-readcmd #x00008  "reading a command key ")
(defconstant rl-state-metanext #x00010  "reading input after ESC ")
(defconstant rl-state-dispatching #x00020  "dispatching to a command ")
(defconstant rl-state-moreinput #x00040  "reading more input in a command function ")
(defconstant rl-state-isearch #x00080  "doing incremental search ")
(defconstant rl-state-nsearch #x00100  "doing non-inc search ")
(defconstant rl-state-search #x00200  "doing a history search ")
(defconstant rl-state-numericarg #x00400  "reading numeric argument ")
(defconstant rl-state-macroinput #x00800  "getting input from a macro ")
(defconstant rl-state-macrodef #x01000  "defining keyboard macro ")
(defconstant rl-state-overwrite #x02000  "overwrite mode ")
(defconstant rl-state-completing #x04000  "doing completion ")
(defconstant rl-state-sighandler #x08000  "in readline sighandler ")
(defconstant rl-state-undoing #x10000  "doing an undo ")
(defconstant rl-state-inputpending #x20000  "rl-execute-next called ")
(defconstant rl-state-done #x80000  "done; accepted line ")



(defun rl-setstate (x)
  (declare (fixnum x))
  (setf rl-readline-state (logior rl-readline-state x)))

(defun rl-unsetstate (x)
  (declare (fixnum x))
  (setf rl-readline-state (logand rl-readline-state (lognot x))))

(defun rl-isstate (x)
  (declare (fixnum x))
  (/= 0 (logand rl-readline-state x)))


(declaim
 (inline
   using-history history-get-history-state history-set-history-state
   add-history remove-history replace-history-entry clear-history
   stifle-history unstifle-history history-is-stifled history-list
   where-history current-history history-get history-total-bytes
   history-set-pos previous-history next-history history-search
   history-search-prefix history-search-pos read-history
   read-history-range write-history append-history
   history-truncate-file history-expand history-arg-extract
   get-history-event history-tokenize rl-digit-argument
   rl-universal-argument rl-forward rl-backward rl-beg-of-line
   rl-end-of-line rl-forward-word rl-backward-word rl-refresh-line
   rl-clear-screen rl-arrow-keys rl-insert rl-quoted-insert
   rl-tab-insert rl-newline rl-do-lowercase-version rl-rubout rl-delete
   rl-rubout-or-delete rl-delete-horizontal-space
   rl-delete-or-show-completions rl-insert-comment rl-upcase-word
   rl-downcase-word rl-capitalize-word rl-transpose-words
   rl-transpose-chars rl-char-search rl-backward-char-search
   rl-beginning-of-history rl-end-of-history rl-get-next-history
   rl-get-previous-history rl-set-mark rl-exchange-point-and-mark
   rl-vi-editing-mode rl-emacs-editing-mode rl-re-read-init-file
   rl-dump-functions rl-dump-macros rl-dump-variables rl-complete
   rl-possible-completions rl-insert-completions rl-menu-complete
   rl-kill-word rl-backward-kill-word rl-kill-line
   rl-backward-kill-line rl-kill-full-line rl-unix-word-rubout
   rl-unix-line-discard rl-copy-region-to-kill rl-kill-region
   rl-copy-forward-word rl-copy-backward-word rl-yank rl-yank-pop
   rl-yank-nth-arg rl-yank-last-arg rl-reverse-search-history
   rl-forward-search-history rl-start-kbd-macro rl-end-kbd-macro
   rl-call-last-kbd-macro rl-revert-line rl-undo-command
   rl-tilde-expand rl-restart-output rl-stop-output rl-abort
   rl-tty-status rl-history-search-forward rl-history-search-backward
   rl-noninc-forward-search rl-noninc-reverse-search
   rl-noninc-forward-search-again rl-noninc-reverse-search-again
   rl-insert-close rl-callback-handler-install rl-callback-read-char
   rl-callback-handler-remove rl-vi-redo rl-vi-undo rl-vi-yank-arg
   rl-vi-fetch-history rl-vi-search-again rl-vi-search rl-vi-complete
   rl-vi-tilde-expand rl-vi-prev-word rl-vi-next-word rl-vi-end-word
   rl-vi-insert-beg rl-vi-append-mode rl-vi-append-eol rl-vi-eof-maybe
   rl-vi-insertion-mode rl-vi-movement-mode rl-vi-arg-digit
   rl-vi-change-case rl-vi-put rl-vi-column rl-vi-delete-to
   rl-vi-change-to rl-vi-yank-to rl-vi-delete rl-vi-back-to-indent
   rl-vi-first-print rl-vi-char-search rl-vi-match rl-vi-change-char
   rl-vi-subst rl-vi-overstrike rl-vi-overstrike-delete rl-vi-replace
   rl-vi-set-mark rl-vi-goto-mark rl-vi-check rl-vi-domove
   rl-vi-bracktype rl-vi-fword rl-vi-bword rl-vi-eword rl-vi-fword
   rl-vi-bword rl-vi-eword readline rl-set-prompt rl-expand-prompt
   rl-initialize rl-discard-argument rl-add-defun rl-bind-key
   rl-bind-key-in-map rl-unbind-key rl-unbind-key-in-map
   rl-unbind-function-in-map rl-unbind-command-in-map rl-set-key
   rl-generic-bind rl-variable-bind rl-macro-bind rl-named-function
   rl-function-of-keyseq rl-list-funmap-names
   rl-invoking-keyseqs-in-map rl-invoking-keyseqs rl-function-dumper
   rl-macro-dumper rl-variable-dumper rl-read-init-file
   rl-parse-and-bind rl-make-bare-keymap rl-copy-keymap rl-make-keymap
   rl-discard-keymap rl-get-keymap-by-name rl-get-keymap-name
   rl-set-keymap rl-get-keymap rl-set-keymap-from-edit-mode
   rl-get-keymap-name-from-edit-mode rl-add-funmap-entry
   rl-funmap-names rl-push-macro-input rl-add-undo rl-free-undo-list
   rl-do-undo rl-begin-undo-group rl-end-undo-group rl-modifying
   rl-redisplay rl-on-new-line rl-on-new-line-with-prompt
   rl-forced-update-display rl-clear-message rl-reset-line-state
   rl-crlf rl-show-char rl-save-prompt rl-restore-prompt rl-insert-text
   rl-delete-text rl-kill-text rl-copy-text rl-prep-terminal
   rl-deprep-terminal rl-tty-set-default-bindings rl-reset-terminal
   rl-resize-terminal rl-set-screen-size rl-get-screen-size
   rl-stuff-char rl-execute-next rl-clear-pending-input rl-read-key
   rl-getc rl-set-keyboard-input-timeout rl-extend-line-buffer rl-ding
   rl-alphabetic rl-set-signals rl-clear-signals
   rl-cleanup-after-signal rl-reset-after-signal rl-free-line-state
   rl-maybe-unsave-line rl-maybe-replace-line rl-complete-internal
   rl-display-match-list rl-completion-matches
   rl-username-completion-function rl-filename-completion-function
   rl-setstate rl-unsetstate rl-isstate ))



;;;; readline.lisp                    --                     --          ;;;;
