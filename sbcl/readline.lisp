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
  (:DOCUMENTATION
   "This package is a foreign interface to the GNU readline
and GNU history libraries.")
  (:use "COMMON-LISP" "SB-ALIEN" "SB-C" "SB-SYS")
  (:EXPORT
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

(DEFUN C-NULL (PTR)
  "
RETURN:     Whether PTR is a C NULL pointer.
"
  (ZEROP (SAP-INT (ALIEN-SAP PTR)))
  ) ;;C-NULL



(DEFUN INITIALIZE ()
  "
DO:         Loads the readline and history libraries.
"
  (LOAD-FOREIGN '("/usr/lib/libreadline.so"
                  "/usr/lib/libhistory.so"))
  ) ;;INITIALIZE






(DEFINE-ALIEN-TYPE FILE  (STRUCT IO-FILE))



;; Bindable functions 
(DEFINE-ALIEN-TYPE RL-COMMAND-FUNC-T
    (FUNCTION INT INT INT ))


;; Typedefs for the completion system 

(DEFINE-ALIEN-TYPE RL-COMPENTRY-FUNC-T
    (FUNCTION C-STRING  C-STRING   INT ))

(DEFINE-ALIEN-TYPE RL-COMPLETION-FUNC-T
    (FUNCTION (* C-STRING)  C-STRING   INT   INT ))

(DEFINE-ALIEN-TYPE RL-QUOTE-FUNC-T
    (FUNCTION C-STRING  C-STRING   INT   C-STRING ))

(DEFINE-ALIEN-TYPE RL-DEQUOTE-FUNC-T
    (FUNCTION C-STRING  C-STRING   INT ))

(DEFINE-ALIEN-TYPE RL-COMPIGNORE-FUNC-T
    (FUNCTION INT  (* C-STRING) ))

(DEFINE-ALIEN-TYPE RL-COMPDISP-FUNC-T
    (FUNCTION VOID  (* C-STRING)   INT   INT ))


;; Type for input and pre-read hook functions like rl_event_hook 

(DEFINE-ALIEN-TYPE RL-HOOK-FUNC-T
    (FUNCTION INT))


;; Input function type 

(DEFINE-ALIEN-TYPE RL-GETC-FUNC-T
    (FUNCTION INT  (* FILE) ))


;; Generic function that takes a character buffer (which could be the readline
;; line buffer) and an index into it (which could be rl_point) and returns
;; an int. 
(DEFINE-ALIEN-TYPE RL-LINEBUF-FUNC-T
    (FUNCTION INT  C-STRING   INT ))


;; `Generic' function pointer (DEFINE-ALIEN-TYPEs 
(DEFINE-ALIEN-TYPE RL-INTFUNC-T    (FUNCTION INT  INT ))
(DEFINE-ALIEN-TYPE RL-IVOIDFUNC-T  (FUNCTION INT))
(DEFINE-ALIEN-TYPE RL-ICPFUNC-T    (FUNCTION INT  C-STRING ))
(DEFINE-ALIEN-TYPE RL-ICPPFUNC-T   (FUNCTION INT  (* C-STRING) ))
(DEFINE-ALIEN-TYPE RL-VOIDFUNC-T   (FUNCTION VOID))
(DEFINE-ALIEN-TYPE RL-VINTFUNC-T   (FUNCTION VOID  INT ))
(DEFINE-ALIEN-TYPE RL-VCPFUNC-T    (FUNCTION VOID  C-STRING ))
(DEFINE-ALIEN-TYPE RL-VCPPFUNC-T   (FUNCTION VOID  (* C-STRING) ))






(DEFINE-ALIEN-TYPE KEYMAP-ENTRY
    ;;  A keymap contains one entry for each key in the ASCII set.
    ;;    Each entry consists of a type and a pointer.
    ;;    FUNCTION is the address of a function to run, or the
    ;;    address of a keymap to indirect through.
    ;;    TYPE says which kind of thing FUNCTION is. 
    (STRUCT KEYMAP-ENTRY
      (TYPE CHAR)
      (FUNCTION (* RL-COMMAND-FUNC-T)))) ;;KEYMAP-ENTRY


(DEFCONSTANT KEYMAP-SIZE 256
  "This must be large enough to hold bindings for all of the characters
in a desired character set (e.g, 128 for ASCII, 256 for ISO Latin-x,
and so on).")



(DEFINE-ALIEN-TYPE KEYMAP-ENTRY-ARRAY (ARRAY KEYMAP-ENTRY 256))
(DEFINE-ALIEN-TYPE KEYMAP (* KEYMAP-ENTRY))

;;  The values that TYPE can have in a keymap entry. 
(DEFCONSTANT ISFUNC (CODE-CHAR 0))
(DEFCONSTANT ISKMAP (CODE-CHAR 1))
(DEFCONSTANT ISMACR (CODE-CHAR 2))



;; ----------------------------------------------------------------------
;; readline/history.h


(DEFINE-ALIEN-TYPE HISTDATA-T (* T))

(DEFINE-ALIEN-TYPE HIST-ENTRY
    ;; The structure used to store a history entry.
    (STRUCT HIST-ENTRY
      (LINE C-STRING)
      (DATA HISTDATA-T))) ;;HIST-ENTRY


(DEFINE-ALIEN-TYPE HISTORY-STATE
    ;; A structure used to pass the current state of the history stuff around. 
    (STRUCT HIST-STATE
      (ENTRIES (* (* HIST-ENTRY))) ;; Pointer to the entries themselves. 
      (OFFSET INT) ;; The location pointer within this array. 
      (LENGTH INT) ;; Number of elements within this array. 
      (SIZE INT)   ;; Number of slots allocated to this array. 
      (FLAGS INT)
      )) ;;HISTORY-STATE


(DEFCONSTANT HS-STIFLED 1
  "Flag values for the `FLAGS' member of HISTORY-STATE.")


;; Initialization and state management. 

(DEFINE-ALIEN-ROUTINE "using_history" VOID
  ;; Begin a session in which the history functions might be used.  This
  ;; just initializes the interactive variables.
  )


(DEFINE-ALIEN-ROUTINE "history_get_history_state" (* HISTORY-STATE)
  ;; Return the current HISTORY_STATE of the history. 
  )


(DEFINE-ALIEN-ROUTINE "history_set_history_state" VOID
  ( STATE (* HISTORY-STATE))
  ;; Set the state of the current history array to STATE.
  )


;; Manage the history list.
  
(DEFINE-ALIEN-ROUTINE "add_history" VOID
  (STRING C-STRING :IN)
  ;; Place STRING at the end of the history list.
  ;; The associated data field (if any) is set to NULL. 
  ) ;;add_history


(DEFINE-ALIEN-ROUTINE "remove_history" (* HIST-ENTRY)
  (WHICH INT :IN)
  ;; A reasonably useless function, only here for completeness.  WHICH
  ;; is the magic number that tells us which element to delete.  The
  ;; elements are numbered from 0. 
  ) ;;remove_history


(DEFINE-ALIEN-ROUTINE "replace_history_entry" (* HIST-ENTRY)
  (WHICH INT        :IN)
  (LINE  C-STRING   :IN)
  (DATA  HISTDATA-T :IN)
  ;; Make the history entry at WHICH have LINE and DATA.  This returns
  ;; the old entry so you can dispose of the data.  In the case of an
  ;; invalid WHICH, a NULL pointer is returned. 
  ) ;;replace_history_entry
   

(DEFINE-ALIEN-ROUTINE "clear_history" VOID
  ;; Clear the history list and start over. 
  )


(DEFINE-ALIEN-ROUTINE "stifle_history" VOID
  (MAX INT :IN)
  ;; Stifle the history list, remembering only MAX number of entries. 
  )


(DEFINE-ALIEN-ROUTINE "unstifle_history" INT
  ;; Stop stifling the history.  This returns the previous amount the
  ;; history was stifled by.  The value is positive if the history was
  ;; stifled, negative if it wasn't.
  ) ;;unstifle_history


(DEFINE-ALIEN-ROUTINE "history_is_stifled" INT
  ;; Return 1 if the history is stifled, 0 if it is not. 
  )


;; Information about the history list. 


(DEFINE-ALIEN-ROUTINE  "history_list" (* (* HIST-ENTRY))
  ;; Return a NULL terminated array of HIST-ENTRY which is the current input
  ;; history.  Element 0 of this list is the beginning of time.  If there
  ;; is no history, return NULL.
  ) ;;history_list


(DEFINE-ALIEN-ROUTINE "where_history" INT
  ;; Returns the number which says what history element we are now
  ;; looking at.  
  )


(DEFINE-ALIEN-ROUTINE "current_history" (* HIST-ENTRY)
  ;; Return the history entry at the current position, as determined by
  ;; history_offset.  If there is no entry there, return a NULL pointer. 
  )


(DEFINE-ALIEN-ROUTINE "history_get" (* HIST-ENTRY)
  (OFFSET INT :IN)
  ;; Return the history entry which is logically at OFFSET in the history
  ;; array.  OFFSET is relative to history_base.
  ) ;;history_get


(DEFINE-ALIEN-ROUTINE "history_total_bytes" INT
  ;; Return the number of bytes that the primary history entries are using.
  ;; This just adds up the lengths of the_history->lines. 
  )


;; Moving around the history list. 


(DEFINE-ALIEN-ROUTINE "history_set_pos" INT
  (POS INT :IN)
  ;; Set the position in the history list to POS. 
  )


(DEFINE-ALIEN-ROUTINE "previous_history" (* HIST-ENTRY)
  ;; Back up history_offset to the previous history entry, and return
  ;; a pointer to that entry.  If there is no previous entry, return
  ;; a NULL pointer. 
  ) ;;previous_history


(DEFINE-ALIEN-ROUTINE "next_history" (* HIST-ENTRY)
  ;; Move history_offset forward to the next item in the input_history,
  ;; and return the a pointer to that entry.  If there is no next entry,
  ;; return a NULL pointer. 
  ) ;;next_history


;; Searching the history list. 

(DEFINE-ALIEN-ROUTINE "history_search" INT
  (STRING    C-STRING :IN)
  (DIRECTION INT      :IN)
  ;; Search the history for STRING, starting at history_offset.
  ;; If DIRECTION < 0, then the search is through previous entries,
  ;; else through subsequent.  If the string is found, then
  ;; current_history () is the history entry, and the value of this function
  ;; is the offset in the line of that history entry that the string was
  ;; found in.  Otherwise, nothing is changed, and a -1 is returned. 
  ) ;;history_search


(DEFINE-ALIEN-ROUTINE "history_search_prefix" INT
  (STRING    C-STRING :IN)
  (DIRECTION INT      :IN)
  ;; Search the history for STRING, starting at history_offset.
  ;; The search is anchored: matching lines must begin with string.
  ;; DIRECTION is as in history_search(). 
  ) ;;history_search_prefix


(DEFINE-ALIEN-ROUTINE "history_search_pos" INT
  (STRING C-STRING :IN)
  (DIR    INT      :IN)
  (POS    INT      :IN)
  ;; Search for STRING in the history list, starting at POS, an
  ;; absolute index into the list.  DIR, if negative, says to search
  ;; backwards from POS, else forwards.
  ;; Returns the absolute index of the history element where STRING
  ;; was found, or -1 otherwise. 
  ) ;;history_search_pos


;; Managing the history file. 

(DEFINE-ALIEN-ROUTINE "read_history" INT
  (FILENAME C-STRING :IN)
  ;; Add the contents of FILENAME to the history list, a line at a time.
  ;; If FILENAME is NULL, then read from ~/.history.  Returns 0 if
  ;; successful, or errno if not. 
  ) ;;read_history


(DEFINE-ALIEN-ROUTINE "read_history_range" INT
  (FILENAME C-STRING :IN)
  (FROM     INT      :IN)
  (TO       INT      :IN)
  ;; Read a range of lines from FILENAME, adding them to the history list.
  ;; Start reading at the FROM'th line and end at the TO'th.  If FROM
  ;; is zero, start at the beginning.  If TO is less than FROM, read
  ;; until the end of the file.  If FILENAME is NULL, then read from
  ;; ~/.history.  Returns 0 if successful, or errno if not. 
  ) ;;read_history_range


(DEFINE-ALIEN-ROUTINE "write_history" INT
  (FILENAME C-STRING :IN)
  ;; Write the current history to FILENAME.  If FILENAME is NULL,
  ;; then write the history list to ~/.history.  Values returned
  ;; are as in read_history ().  
  ) ;;write_history


(DEFINE-ALIEN-ROUTINE "append_history" INT
  (NELEMENT INT      :IN)
  (FILENAME C-STRING :IN)
  ;; Append NELEMENT entries to FILENAME.  The entries appended are from
  ;; the end of the list minus NELEMENTs up to the end of the list. 
  ) ;;append_history


(DEFINE-ALIEN-ROUTINE "history_truncate_file" INT
  (FILENAME C-STRING :IN)
  (NLINES   INT      :IN)
  ;; Truncate the history file, leaving only the last NLINES lines. 
  ) ;;history_truncate_file


;; History expansion. 

(DEFINE-ALIEN-ROUTINE "history_expand" INT
  (STRING C-STRING :IN)
  (OUTPUT (* C-STRING) :IN) ;; actually :OUT !
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


(DEFINE-ALIEN-ROUTINE "history_arg_extract" C-STRING
  (FIRST  INT      :IN)
  (LAST   INT      :IN)
  (STRING C-STRING :IN)
  ;; Extract a string segment consisting of the FIRST through LAST
  ;; arguments present in STRING.  Arguments are broken up as in
  ;; the shell. 
  ) ;;history_arg_extract


(DEFINE-ALIEN-ROUTINE "get_history_event" C-STRING
  (STRING C-STRING       :IN)
  (INDEX  INT            :IN-OUT)
  (DELIMITING-QUOTE CHAR :IN)
  ;; Return the text of the history event beginning at the current
  ;; offset into STRING.  Pass STRING with *INDEX equal to the
  ;; history_expansion_char that begins this specification.
  ;; DELIMITING_QUOTE is a character that is allowed to end the string
  ;; specification for what to search for in addition to the normal
  ;; characters `:', ` ', `\t', `\n', and sometimes `?'. 
  ) ;;get_history_event


(DEFINE-ALIEN-ROUTINE "history_tokenize" (* C-STRING)
  (STRING C-STRING :IN)
  ;; Return an array of tokens, much as the shell might.  The tokens are
  ;; parsed out of STRING. 
  ) ;;history_tokenize


(DECLAIM
 (INLINE
   USING-HISTORY HISTORY-GET-HISTORY-STATE HISTORY-SET-HISTORY-STATE
   ADD-HISTORY REMOVE-HISTORY REPLACE-HISTORY-ENTRY CLEAR-HISTORY
   STIFLE-HISTORY UNSTIFLE-HISTORY HISTORY-IS-STIFLED HISTORY-LIST
   WHERE-HISTORY CURRENT-HISTORY HISTORY-GET HISTORY-TOTAL-BYTES
   HISTORY-SET-POS PREVIOUS-HISTORY NEXT-HISTORY HISTORY-SEARCH
   HISTORY-SEARCH-PREFIX HISTORY-SEARCH-POS READ-HISTORY
   READ-HISTORY-RANGE WRITE-HISTORY APPEND-HISTORY
   HISTORY-TRUNCATE-FILE HISTORY-EXPAND HISTORY-ARG-EXTRACT
   GET-HISTORY-EVENT HISTORY-TOKENIZE ))

;; Exported history variables.

(DEFINE-ALIEN-VARIABLE "history_base" INT)
(DEFINE-ALIEN-VARIABLE "history_length" INT)
(DEFINE-ALIEN-VARIABLE "history_max_entries" INT)
(DEFINE-ALIEN-VARIABLE "history_expansion_char" CHAR)
(DEFINE-ALIEN-VARIABLE "history_subst_char" CHAR)
(DEFINE-ALIEN-VARIABLE "history_word_delimiters" C-STRING)
(DEFINE-ALIEN-VARIABLE "history_comment_char" CHAR)
(DEFINE-ALIEN-VARIABLE "history_no_expand_chars" C-STRING)
(DEFINE-ALIEN-VARIABLE "history_search_delimiter_chars" C-STRING)
(DEFINE-ALIEN-VARIABLE "history_quotes_inhibit_expansion" INT)


(DEFINE-ALIEN-VARIABLE "max_input_history" INT
  ;; Backwards compatibility
  )


(DEFINE-ALIEN-VARIABLE "history_inhibit_expansion_function"
    (* RL-LINEBUF-FUNC-T)
  ;; If set, this function is called to decide whether or not a particular
  ;; history expansion should be treated as a special case for the calling
  ;; application and not expanded. 
  ) ;;history_inhibit_expansion_function




;; ----------------------------------------------------------------------
;; readline/readline.h




;; Readline data structures. 

;; Maintaining the state of undo.  We remember individual deletes and inserts
;; on a chain of things to do. 

(DEFINE-ALIEN-TYPE UNDO-CODE
    ;; The actions that undo knows how to undo.  Notice that UNDO_DELETE means
    ;; to insert some text, and UNDO_INSERT means to delete some text.   I.e.,
    ;; the code tells undo what to undo, not how to undo it. 
    (ENUM UNDO-CODE
          :UNDO-DELETE :UNDO-INSERT :UNDO-BEGIN :UNDO-END)
  ) ;;UNDO-CODE


(DEFINE-ALIEN-TYPE UNDO-LIST
    ;; What an element of THE_UNDO_LIST looks like. 
    (STRUCT UNDO-LIST
      (NEXT (* (STRUCT UNDO-LIST)))
      (START INT) ;; Where the change took place.
      (END INT)
      (TEXT  C-STRING) ;; The text to insert, if undoing a delete. 
      (WHAT (ENUM UNDO-CODE)) ;; Delete, Insert, Begin, End. 
      ))                      ;;UNDO-LIST


(DEFINE-ALIEN-VARIABLE "rl_undo_list" 
    ;; The current undo list for RL_LINE_BUFFER. 
    (* UNDO-LIST))


(DEFINE-ALIEN-TYPE FUNMAP
    ;; The data structure for mapping textual names to code addresses.
    (STRUCT FUNMAP
      (NAME C-STRING)
      (FUNCTION (* RL-COMMAND-FUNC-T)))) ;;FUNMAP

(DEFINE-ALIEN-VARIABLE "funmap" (* (* FUNMAP)))
;; SHOULD BE AN ARRAY OF (* FUNMAP)...


;; **************************************************************** 
;; 								    
;; 	     Functions available to bind to key sequences	    
;; 								    
;; **************************************************************** 


;; Bindable commands for numeric arguments.

(DEFINE-ALIEN-ROUTINE "rl_digit_argument" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_universal_argument" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable commands for moving the cursor.

(DEFINE-ALIEN-ROUTINE "rl_forward" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_backward" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_beg_of_line" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_end_of_line" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_forward_word" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_backward_word" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_refresh_line" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_clear_screen" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_arrow_keys" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable commands for inserting and deleting text. 

(DEFINE-ALIEN-ROUTINE "rl_insert" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_quoted_insert" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_tab_insert" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_newline" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_do_lowercase_version" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_rubout" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_delete" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_rubout_or_delete" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_delete_horizontal_space" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_delete_or_show_completions" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_insert_comment" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable commands for changing case. 

(DEFINE-ALIEN-ROUTINE "rl_upcase_word" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_downcase_word" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_capitalize_word" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable commands for transposing characters and words. 

(DEFINE-ALIEN-ROUTINE "rl_transpose_words" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_transpose_chars" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable commands for searching within a line. 

(DEFINE-ALIEN-ROUTINE "rl_char_search" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_backward_char_search" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable commands for readline's interface to the command history. 

(DEFINE-ALIEN-ROUTINE "rl_beginning_of_history" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_end_of_history" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_get_next_history" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_get_previous_history" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable commands for managing the mark and region. 

(DEFINE-ALIEN-ROUTINE "rl_set_mark" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_exchange_point_and_mark" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable commands to set the editing mode (emacs or vi). 

(DEFINE-ALIEN-ROUTINE "rl_vi_editing_mode" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_emacs_editing_mode" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable commands for managing key bindings. 

(DEFINE-ALIEN-ROUTINE "rl_re_read_init_file" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_dump_functions" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_dump_macros" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_dump_variables" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable commands for word completion. 

(DEFINE-ALIEN-ROUTINE "rl_complete" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_possible_completions" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_insert_completions" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_menu_complete" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable commands for killing and yanking text, and managing the kill ring. 

(DEFINE-ALIEN-ROUTINE "rl_kill_word" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_backward_kill_word" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_kill_line" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_backward_kill_line" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_kill_full_line" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_unix_word_rubout" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_unix_line_discard" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_copy_region_to_kill" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_kill_region" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_copy_forward_word" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_copy_backward_word" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_yank" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_yank_pop" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_yank_nth_arg" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_yank_last_arg" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable commands for incremental searching. 

(DEFINE-ALIEN-ROUTINE "rl_reverse_search_history" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_forward_search_history" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable keyboard macro commands. 

(DEFINE-ALIEN-ROUTINE "rl_start_kbd_macro" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_end_kbd_macro" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_call_last_kbd_macro" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable undo commands. 

(DEFINE-ALIEN-ROUTINE "rl_revert_line" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_undo_command" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable tilde expansion commands. 

(DEFINE-ALIEN-ROUTINE "rl_tilde_expand" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable terminal control commands. 

(DEFINE-ALIEN-ROUTINE "rl_restart_output" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_stop_output" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Miscellaneous bindable commands. 

(DEFINE-ALIEN-ROUTINE "rl_abort" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_tty_status" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable commands for incremental and non-incremental history searching. 

(DEFINE-ALIEN-ROUTINE "rl_history_search_forward" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_history_search_backward" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_noninc_forward_search" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_noninc_reverse_search" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_noninc_forward_search_again" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_noninc_reverse_search_again" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Bindable command used when inserting a matching close character. 

(DEFINE-ALIEN-ROUTINE "rl_insert_close" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; Not available unless READLINE_CALLBACKS is defined. 

(DEFINE-ALIEN-ROUTINE "rl_callback_handler_install" VOID
  (NAME      C-STRING          :IN)
  (FUNCTION  (* RL-VCPFUNC-T)  :IN)
  )

(DEFINE-ALIEN-ROUTINE "rl_callback_read_char" VOID )

(DEFINE-ALIEN-ROUTINE "rl_callback_handler_remove" VOID )


;; Things for vi mode. Not available unless readline is compiled -DVI_MODE. 
;; VI-mode bindable commands. 

(DEFINE-ALIEN-ROUTINE "rl_vi_redo" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_undo" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_yank_arg" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_fetch_history" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_search_again" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_search" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_complete" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_tilde_expand" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_prev_word" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_next_word" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_end_word" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_insert_beg" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_append_mode" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_append_eol" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_eof_maybe" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_insertion_mode" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_movement_mode" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_arg_digit" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_change_case" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_put" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_column" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_delete_to" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_change_to" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_yank_to" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_delete" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_back_to_indent" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_first_print" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_char_search" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_match" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_change_char" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_subst" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_overstrike" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_overstrike_delete" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_replace" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_set_mark" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_goto_mark" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; VI-mode utility functions. 

(DEFINE-ALIEN-ROUTINE "rl_vi_check" INT)


(DEFINE-ALIEN-ROUTINE "rl_vi_domove" INT
  (A  INT  :IN)
  (B  INT  :IN-OUT)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_bracktype" INT
  (A  INT  :IN)
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


(DEFINE-ALIEN-ROUTINE "rl_vi_fword" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_bword" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_vi_eword" INT
  (COUNT  INT  :IN)
  (KEY    INT  :IN)
  )


;; **************************************************************** 
;; 								    
;; 			Well Published Functions		    
;; 								    
;; **************************************************************** 

;; Readline functions. 
;; Read a line of input.  Prompt with PROMPT.  A NULL PROMPT means none. 

(DEFINE-ALIEN-ROUTINE "readline" C-STRING
  (PROMPT C-STRING :IN)
  )



(DEFINE-ALIEN-ROUTINE "rl_set_prompt" INT
  (PROMPT C-STRING :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_expand_prompt" INT
  (PROMPT C-STRING :IN)
  )



(DEFINE-ALIEN-ROUTINE "rl_initialize" INT)



;; Undocumented; unused by readline 

(DEFINE-ALIEN-ROUTINE "rl_discard_argument" INT )


;; Utility functions to bind keys to readline commands. 

(DEFINE-ALIEN-ROUTINE "rl_add_defun" INT
  (NAME     C-STRING              :IN)
  (FUNCTION (* RL-COMMAND-FUNC-T) :IN)
  (KEY      INT                   :IN)
  ) ;;rl_add_defun


(DEFINE-ALIEN-ROUTINE "rl_bind_key" INT
  (KEY       INT                    :IN)
  (FUNCTION  (* RL-COMMAND-FUNC-T)  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_bind_key_in_map" INT
  (KEY       INT                    :IN)
  (FUNCTION  (* RL-COMMAND-FUNC-T)  :IN)
  (MAP       KEYMAP                 :IN)
  ) ;;rl_bind_key_in_map


(DEFINE-ALIEN-ROUTINE "rl_unbind_key" INT
  (KEY  INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_unbind_key_in_map" INT
  (KEY  INT  :IN)
  (MAP  KEYMAP  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_unbind_function_in_map" INT
  (FUNCTION  (* RL-COMMAND-FUNC-T)  :IN)
  (MAP  KEYMAP  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_unbind_command_in_map" INT
  (COMMAND  C-STRING  :IN)
  (MAP  KEYMAP  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_set_key" INT
  (KEYSEQ C-STRING  :IN)
  (FUNCTION  (* RL-COMMAND-FUNC-T)  :IN)
  (MAP  KEYMAP  :IN)
  ) ;;rl_set_key


(DEFINE-ALIEN-ROUTINE "rl_generic_bind" INT
  (TYPE  INT  :IN)
  (KEYSEQ  C-STRING   :IN)
  (DATA  (* CHAR)  :IN)
  (MAP  KEYMAP  :IN)
  ) ;;rl_generic_bind


(DEFINE-ALIEN-ROUTINE "rl_variable_bind" INT
  (VARIABLE  C-STRING   :IN)
  (VALUE  C-STRING   :IN)
  )


;; Backwards compatibility, use rl_generic_bind instead. 

(DEFINE-ALIEN-ROUTINE "rl_macro_bind" INT
  (KEYSEQ  C-STRING   :IN)
  (MACRO   C-STRING   :IN)
  (MAP     KEYMAP     :IN)
  ) ;;rl_macro_bind



(DEFINE-ALIEN-ROUTINE "rl_named_function" (* RL-COMMAND-FUNC-T)
  (NAME  C-STRING   :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_function_of_keyseq" (* RL-COMMAND-FUNC-T)
  (KEYSEQ  C-STRING   :IN)
  (MAP  KEYMAP  :IN)
  (TYPE INT   :OUT)
  ) ;;rl_function_of_keyseq



(DEFINE-ALIEN-ROUTINE "rl_list_funmap_names" VOID )


(DEFINE-ALIEN-ROUTINE "rl_invoking_keyseqs_in_map" (* (* CHAR))
  (FUNCTION  (* RL-COMMAND-FUNC-T)  :IN)
  (MAP  KEYMAP  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_invoking_keyseqs" (* (* CHAR))
  (FUNCTION  (* RL-COMMAND-FUNC-T)  :IN)
  )

 

(DEFINE-ALIEN-ROUTINE "rl_function_dumper" VOID
  (READABLE  INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_macro_dumper" VOID
  (READABLE  INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_variable_dumper" VOID
  (READABLE  INT  :IN)
  )



(DEFINE-ALIEN-ROUTINE "rl_read_init_file" INT
  (FILENAME  C-STRING   :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_parse_and_bind" INT
  (LINE  C-STRING  :IN)
  )


;; Functions for manipulating keymaps. 

(DEFINE-ALIEN-ROUTINE "rl_make_bare_keymap" KEYMAP  )


(DEFINE-ALIEN-ROUTINE "rl_copy_keymap" KEYMAP
  (MAP  KEYMAP  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_make_keymap" KEYMAP )


(DEFINE-ALIEN-ROUTINE "rl_discard_keymap" VOID
  (MAP  KEYMAP  :IN)
  )



(DEFINE-ALIEN-ROUTINE "rl_get_keymap_by_name" KEYMAP
  (NAME  C-STRING   :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_get_keymap_name" C-STRING
  (MAP  KEYMAP  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_set_keymap" VOID
  (MAP  KEYMAP  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_get_keymap" KEYMAP )

;; Undocumented; used internally only. 

(DEFINE-ALIEN-ROUTINE "rl_set_keymap_from_edit_mode" VOID )


(DEFINE-ALIEN-ROUTINE "rl_get_keymap_name_from_edit_mode" C-STRING )


;; Functions for manipulating the funmap, which maps command names to functions. 

(DEFINE-ALIEN-ROUTINE "rl_add_funmap_entry" INT
  (NAME  C-STRING   :IN)
  (FUNCTION  (* RL-COMMAND-FUNC-T)  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_funmap_names" (* C-STRING) )



;; Utility functions for managing keyboard macros. 

(DEFINE-ALIEN-ROUTINE "rl_push_macro_input" VOID
  (MACRO C-STRING  :IN)
  )


;; Functions for undoing, from undo.c 

(DEFINE-ALIEN-ROUTINE "rl_add_undo" VOID
  (WHAT (ENUM UNDO-CODE)  :IN)
  (START  INT    :IN)
  (END    INT    :IN)
  (TEXT C-STRING :IN)
  ) ;;rl_add_undo


(DEFINE-ALIEN-ROUTINE "rl_free_undo_list" VOID)


(DEFINE-ALIEN-ROUTINE "rl_do_undo" INT )


(DEFINE-ALIEN-ROUTINE "rl_begin_undo_group" INT )


(DEFINE-ALIEN-ROUTINE "rl_end_undo_group" INT )


(DEFINE-ALIEN-ROUTINE "rl_modifying" INT
  (START  INT  :IN)
  (END    INT  :IN)
  )


;; Functions for redisplay. 

(DEFINE-ALIEN-ROUTINE "rl_redisplay" VOID)


(DEFINE-ALIEN-ROUTINE "rl_on_new_line" INT )


(DEFINE-ALIEN-ROUTINE "rl_on_new_line_with_prompt" INT )


(DEFINE-ALIEN-ROUTINE "rl_forced_update_display" INT )


(DEFINE-ALIEN-ROUTINE "rl_clear_message" INT )


(DEFINE-ALIEN-ROUTINE "rl_reset_line_state" INT )


(DEFINE-ALIEN-ROUTINE "rl_crlf" INT )


;; extern int rl_message (c-string , ...);


(DEFINE-ALIEN-ROUTINE "rl_show_char" INT
  (C  INT  :IN)
  )


;; Save and restore internal prompt redisplay information. 

(DEFINE-ALIEN-ROUTINE "rl_save_prompt" VOID )


(DEFINE-ALIEN-ROUTINE "rl_restore_prompt" VOID )


;; Modifying text. 

(DEFINE-ALIEN-ROUTINE "rl_insert_text" INT
  (TEXT  C-STRING   :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_delete_text" INT
  (START  INT  :IN)
  (END    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_kill_text" INT
  (START  INT  :IN)
  (END    INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_copy_text" C-STRING
  (START  INT  :IN)
  (END    INT  :IN)
  )


;; Terminal and tty mode management. 

(DEFINE-ALIEN-ROUTINE "rl_prep_terminal" VOID
  (META-FLAG  INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_deprep_terminal" VOID )


(DEFINE-ALIEN-ROUTINE "rl_tty_set_default_bindings" VOID
  (MAP  KEYMAP  :IN)
  )



(DEFINE-ALIEN-ROUTINE "rl_reset_terminal" INT
  (TERMINAL-NAME  C-STRING   :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_resize_terminal" VOID )


(DEFINE-ALIEN-ROUTINE "rl_set_screen_size" VOID
  (ROWS  INT  :IN)
  (COLS  INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_get_screen_size" VOID
  (ROWS  INT :OUT)
  (COLS  INT :OUT)
  )


;; Functions for character input. 

(DEFINE-ALIEN-ROUTINE "rl_stuff_char" INT
  (C  INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_execute_next" INT
  (C  INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_clear_pending_input" INT )


(DEFINE-ALIEN-ROUTINE "rl_read_key" INT )


(DEFINE-ALIEN-ROUTINE "rl_getc" INT
  (STREAM  (* FILE)  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_set_keyboard_input_timeout" INT
  (U  INT  :IN)
  )


;; `Public' utility functions . 

(DEFINE-ALIEN-ROUTINE "rl_extend_line_buffer" VOID
  (LEN  INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_ding" INT )


(DEFINE-ALIEN-ROUTINE "rl_alphabetic" INT
  (C  INT  :IN)
  )


;; Readline signal handling, from signals.c 

(DEFINE-ALIEN-ROUTINE "rl_set_signals" INT)


(DEFINE-ALIEN-ROUTINE "rl_clear_signals" INT )


(DEFINE-ALIEN-ROUTINE "rl_cleanup_after_signal" VOID )


(DEFINE-ALIEN-ROUTINE "rl_reset_after_signal" VOID )


(DEFINE-ALIEN-ROUTINE "rl_free_line_state" VOID )

 


(DEFINE-ALIEN-ROUTINE "rl_maybe_unsave_line" INT )


(DEFINE-ALIEN-ROUTINE "rl_maybe_replace_line" INT )


;; Completion functions. 

(DEFINE-ALIEN-ROUTINE "rl_complete_internal" INT
  (WHAT-TO-DO  INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_display_match_list" VOID
  (MATCHES  (* C-STRING)  :IN)
  (LEN    INT  :IN)
  (MAX    INT  :IN)
  ) ;;rl_display_match_list



(DEFINE-ALIEN-ROUTINE "rl_completion_matches" (* C-STRING)
  (TEXT  C-STRING   :IN)
  (ENTRY-FUNC (* RL-COMPENTRY-FUNC-T)  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_username_completion_function" C-STRING
  (TEXT  C-STRING   :IN)
  (STATE INT  :IN)
  )


(DEFINE-ALIEN-ROUTINE "rl_filename_completion_function" C-STRING
  (TEXT  C-STRING   :IN)
  (STATE INT  :IN)
  )



;; **************************************************************** 
;; 								    
;; 			Well Published Variables		    
;; 								    
;; **************************************************************** 

;; The version of this incarnation of the readline library. 
(DEFINE-ALIEN-VARIABLE "rl_library_version" C-STRING)


;; True if this is real GNU readline. 
(DEFINE-ALIEN-VARIABLE "rl_gnu_readline_p" INT)


;; Flags word encapsulating the current readline state. 
(DEFINE-ALIEN-VARIABLE "rl_readline_state" INT)


;; Says which editing mode readline is currently using.  1 means emacs mode;
;; 0 means vi mode. 
(DEFINE-ALIEN-VARIABLE "rl_editing_mode" INT)


;; The name of the calling program.  You should initialize this to
;; whatever was in argv[0].  It is used when parsing conditionals. 
(DEFINE-ALIEN-VARIABLE "rl_readline_name" C-STRING)


;; The prompt readline uses.  This is set from the argument to
;; readline (), and should not be assigned to directly. 
(DEFINE-ALIEN-VARIABLE "rl_prompt" C-STRING)


;; The line buffer that is in use. 
(DEFINE-ALIEN-VARIABLE "rl_line_buffer" C-STRING)


;; The location of point, and end. 
(DEFINE-ALIEN-VARIABLE "rl_point" INT)

(DEFINE-ALIEN-VARIABLE "rl_end" INT)


;; The mark, or saved cursor position. 
(DEFINE-ALIEN-VARIABLE "rl_mark" INT)


;; Flag to indicate that readline has finished with the current input
;; line and should return it. 
(DEFINE-ALIEN-VARIABLE "rl_done" INT)


;; If set to a character value, that will be the next keystroke read. 
(DEFINE-ALIEN-VARIABLE "rl_pending_input" INT)


;; Non-zero if we called this function from _rl_dispatch().  It's present
;; so functions can find out whether they were called from a key binding
;; or directly from an application. 
(DEFINE-ALIEN-VARIABLE "rl_dispatching" INT)


;; Non-zero if the user typed a numeric argument before executing the
;; current function. 
(DEFINE-ALIEN-VARIABLE "rl_explicit_arg" INT)


;; The current value of the numeric argument specified by the user. 
(DEFINE-ALIEN-VARIABLE "rl_numeric_arg" INT)


;; The address of the last command function Readline executed. 
(DEFINE-ALIEN-VARIABLE "rl_last_func" (* RL-COMMAND-FUNC-T))


;; The name of the terminal to use. 
(DEFINE-ALIEN-VARIABLE "rl_terminal_name" C-STRING)


;; The input and output streams. 
(DEFINE-ALIEN-VARIABLE "rl_instream" (* FILE))

(DEFINE-ALIEN-VARIABLE "rl_outstream" (* FILE))


;; If non-zero, then this is the address of a function to call just
;; before readline_internal () prints the first prompt. 
(DEFINE-ALIEN-VARIABLE "rl_startup_hook" (* RL-HOOK-FUNC-T))


;; If non-zero, this is the address of a function to call just before
;; readline_internal_setup () returns and readline_internal starts
;; reading input characters. 
(DEFINE-ALIEN-VARIABLE "rl_pre_input_hook" (* RL-HOOK-FUNC-T))

      
;; The address of a function to call periodically while Readline is
;; awaiting character input, or NULL, for no event handling. 
(DEFINE-ALIEN-VARIABLE "rl_event_hook" (* RL-HOOK-FUNC-T))


;; The address of the function to call to fetch a character from the current
;; Readline input stream 
(DEFINE-ALIEN-VARIABLE "rl_getc_function" (* RL-GETC-FUNC-T))


(DEFINE-ALIEN-VARIABLE "rl_redisplay_function" (* RL-VOIDFUNC-T))


(DEFINE-ALIEN-VARIABLE "rl_prep_term_function" (* RL-VINTFUNC-T))

(DEFINE-ALIEN-VARIABLE "rl_deprep_term_function" (* RL-VOIDFUNC-T))


;; Dispatch variables. 
(DEFINE-ALIEN-VARIABLE "rl_executing_keymap" KEYMAP)

(DEFINE-ALIEN-VARIABLE "rl_binding_keymap" KEYMAP)


;; Display variables. 
;; If non-zero, readline will erase the entire line, including any prompt,
;; if the only thing typed on an otherwise-blank line is something bound to
;; rl_newline. 
(DEFINE-ALIEN-VARIABLE "rl_erase_empty_line" INT)


;; If non-zero, the application has already printed the prompt (rl_prompt)
;; before calling readline, so readline should not output it the first time
;; redisplay is done. 
(DEFINE-ALIEN-VARIABLE "rl_already_prompted" INT)


;; A non-zero value means to read only this many characters rather than
;; up to a character bound to accept-line. 
(DEFINE-ALIEN-VARIABLE "rl_num_chars_to_read" INT)


;; The text of a currently-executing keyboard macro. 
(DEFINE-ALIEN-VARIABLE "rl_executing_macro" (* CHAR))


;; Variables to control readline signal handling. 
;; If non-zero, readline will install its own signal handlers for
;; SIGINT, SIGTERM, SIGQUIT, SIGALRM, SIGTSTP, SIGTTIN, and SIGTTOU. 
(DEFINE-ALIEN-VARIABLE "rl_catch_signals" INT)


;; If non-zero, readline will install a signal handler for SIGWINCH
;; that also attempts to call any calling application's SIGWINCH signal
;; handler.  Note that the terminal is not cleaned up before the
;; application's signal handler is called; use rl_cleanup_after_signal()
;; to do that. 
(DEFINE-ALIEN-VARIABLE "rl_catch_sigwinch" INT)


;; Completion variables. 
;; Pointer to the generator function for completion_matches ().
;; NULL means to use filename_entry_function (), the default filename
;; completer. 
(DEFINE-ALIEN-VARIABLE "rl_completion_entry_function" (* RL-COMPENTRY-FUNC-T))


;; If rl_ignore_some_completions_function is non-NULL it is the address
;; of a function to call after all of the possible matches have been
;; generated, but before the actual completion is done to the input line.
;; The function is called with one argument; a NULL terminated array
;; of (char *).  If your function removes any of the elements, they
;; must be free()'ed. 
(DEFINE-ALIEN-VARIABLE "rl_ignore_some_completions_function" (* RL-COMPIGNORE-FUNC-T))


;; Pointer to alternative function to create matches.
;; Function is called with TEXT, START, and END.
;; START and END are indices in RL_LINE_BUFFER saying what the boundaries
;; of TEXT are.
;; If this function exists and returns NULL then call the value of
;; rl_completion_entry_function to try to match, otherwise use the
;; array of strings returned. 
(DEFINE-ALIEN-VARIABLE "rl_attempted_completion_function" (* RL-COMPLETION-FUNC-T))


;; The basic list of characters that signal a break between words for the
;; completer routine.  The initial contents of this variable is what
;; breaks words in the shell, i.e. "n\"\\'`@$>". 
(DEFINE-ALIEN-VARIABLE "rl_basic_word_break_characters" C-STRING)


;; The list of characters that signal a break between words for
;; rl_complete_internal.  The default list is the contents of
;; rl_basic_word_break_characters.  
(DEFINE-ALIEN-VARIABLE "rl_completer_word_break_characters" C-STRING)


;; List of characters which can be used to quote a substring of the line.
;; Completion occurs on the entire substring, and within the substring   
;; rl_completer_word_break_characters are treated as any other character,
;; unless they also appear within this list. 
(DEFINE-ALIEN-VARIABLE "rl_completer_quote_characters" C-STRING)


;; List of quote characters which cause a word break. 
(DEFINE-ALIEN-VARIABLE "rl_basic_quote_characters" C-STRING)


;; List of characters that need to be quoted in filenames by the completer. 
(DEFINE-ALIEN-VARIABLE "rl_filename_quote_characters" C-STRING)


;; List of characters that are word break characters, but should be left
;; in TEXT when it is passed to the completion function.  The shell uses
;; this to help determine what kind of completing to do. 
(DEFINE-ALIEN-VARIABLE "rl_special_prefixes" C-STRING)


;; If non-zero, then this is the address of a function to call when
;; completing on a directory name.  The function is called with
;; the address of a string (the current directory name) as an arg.  It
;; changes what is displayed when the possible completions are printed
;; or inserted. 
(DEFINE-ALIEN-VARIABLE "rl_directory_completion_hook" (* RL-ICPPFUNC-T))


;; If non-zero, this is the address of a function to call when completing
;; a directory name.  This function takes the address of the directory name
;; to be modified as an argument.  Unlike rl_directory_completion_hook, it
;; only modifies the directory name used in opendir(2), not what is displayed
;; when the possible completions are printed or inserted.  It is called
;; before rl_directory_completion_hook.  I'm not happy with how this works
;; yet, so it's undocumented. 
(DEFINE-ALIEN-VARIABLE "rl_directory_rewrite_hook" (* RL-ICPPFUNC-T))


;; If non-zero, then this is the address of a function to call when
;; completing a word would normally display the list of possible matches.
;; This function is called instead of actually doing the display.
;; It takes three arguments: (char **matches, int num_matches, int max_length)
;; where MATCHES is the array of strings that matched, NUM_MATCHES is the
;; number of strings in that array, and MAX_LENGTH is the length of the
;; longest string in that array. 
(DEFINE-ALIEN-VARIABLE "rl_completion_display_matches_hook" (* RL-COMPDISP-FUNC-T))


;; Non-zero means that the results of the matches are to be treated
;; as filenames.  This is ALWAYS zero on entry, and can only be changed
;; within a completion entry finder function. 
(DEFINE-ALIEN-VARIABLE "rl_filename_completion_desired" INT)


;; Non-zero means that the results of the matches are to be quoted using
;; double quotes (or an application-specific quoting mechanism) if the
;; filename contains any characters in rl_word_break_chars.  This is
;; ALWAYS non-zero on entry, and can only be changed within a completion
;; entry finder function. 
(DEFINE-ALIEN-VARIABLE "rl_filename_quoting_desired" INT)


;; Set to a function to quote a filename in an application-specific fashion.
;; Called with the text to quote, the type of match found (single or multiple)
;; and a pointer to the quoting character to be used, which the function can
;; reset if desired. 
(DEFINE-ALIEN-VARIABLE "rl_filename_quoting_function" (* RL-QUOTE-FUNC-T))


;; Function to call to remove quoting characters from a filename.  Called
;; before completion is attempted, so the embedded quotes do not interfere
;; with matching names in the file system. 
(DEFINE-ALIEN-VARIABLE "rl_filename_dequoting_function" (* RL-DEQUOTE-FUNC-T))


;; Function to call to decide whether or not a word break character is
;; quoted.  If a character is quoted, it does not break words for the
;; completer. 
(DEFINE-ALIEN-VARIABLE "rl_char_is_quoted_p" (* RL-LINEBUF-FUNC-T))


;; Non-zero means to suppress normal filename completion after the
;; user-specified completion function has been called. 
(DEFINE-ALIEN-VARIABLE "rl_attempted_completion_over" INT)


;; Set to a character describing the type of completion being attempted by
;; rl_complete_internal; available for use by application completion
;; functions. 
(DEFINE-ALIEN-VARIABLE "rl_completion_type" INT)


;; Character appended to completed words when at the end of the line.  The
;; default is a space.  Nothing is added if this is '\0'. 
(DEFINE-ALIEN-VARIABLE "rl_completion_append_character" INT)


;; Up to this many items will be displayed in response to a
;; possible-completions call.  After that, we ask the user if she
;; is sure she wants to see them all.  The default value is 100. 
(DEFINE-ALIEN-VARIABLE "rl_completion_query_items" INT)


;; If non-zero, then disallow duplicates in the matches. 
(DEFINE-ALIEN-VARIABLE "rl_ignore_completion_duplicates" INT)


;; If this is non-zero, completion is (temporarily) inhibited, and the
;; completion character will be inserted as any other. 
(DEFINE-ALIEN-VARIABLE "rl_inhibit_completion" INT)

   
;; Definitions available for use by readline clients. 
(DEFCONSTANT RL-PROMPT-START-IGNORE (CODE-CHAR 1))
(DEFCONSTANT RL-PROMPT-END-IGNORE   (CODE-CHAR 2))



;; Possible values for do-replace argument to rl-filename-quoting-function,
;; called by rl-complete-internal. 
(DEFCONSTANT NO-MATCH     0)
(DEFCONSTANT SINGLE-MATCH 1)
(DEFCONSTANT MULT-MATCH   2)



;; Possible state values for rl-readline-state 
(DEFCONSTANT RL-STATE-NONE #X00000  "no state; before first call ")
(DEFCONSTANT RL-STATE-INITIALIZING #X00001  "initializing ")
(DEFCONSTANT RL-STATE-INITIALIZED #X00002  "initialization done ")
(DEFCONSTANT RL-STATE-TERMPREPPED #X00004  "terminal is prepped ")
(DEFCONSTANT RL-STATE-READCMD #X00008  "reading a command key ")
(DEFCONSTANT RL-STATE-METANEXT #X00010  "reading input after ESC ")
(DEFCONSTANT RL-STATE-DISPATCHING #X00020  "dispatching to a command ")
(DEFCONSTANT RL-STATE-MOREINPUT #X00040  "reading more input in a command function ")
(DEFCONSTANT RL-STATE-ISEARCH #X00080  "doing incremental search ")
(DEFCONSTANT RL-STATE-NSEARCH #X00100  "doing non-inc search ")
(DEFCONSTANT RL-STATE-SEARCH #X00200  "doing a history search ")
(DEFCONSTANT RL-STATE-NUMERICARG #X00400  "reading numeric argument ")
(DEFCONSTANT RL-STATE-MACROINPUT #X00800  "getting input from a macro ")
(DEFCONSTANT RL-STATE-MACRODEF #X01000  "defining keyboard macro ")
(DEFCONSTANT RL-STATE-OVERWRITE #X02000  "overwrite mode ")
(DEFCONSTANT RL-STATE-COMPLETING #X04000  "doing completion ")
(DEFCONSTANT RL-STATE-SIGHANDLER #X08000  "in readline sighandler ")
(DEFCONSTANT RL-STATE-UNDOING #X10000  "doing an undo ")
(DEFCONSTANT RL-STATE-INPUTPENDING #X20000  "rl-execute-next called ")
(DEFCONSTANT RL-STATE-DONE #X80000  "done; accepted line ")



(DEFUN RL-SETSTATE (X)
  (DECLARE (FIXNUM X))
  (SETF RL-READLINE-STATE (LOGIOR RL-READLINE-STATE X)))

(DEFUN RL-UNSETSTATE (X)
  (DECLARE (FIXNUM X))
  (SETF RL-READLINE-STATE (LOGAND RL-READLINE-STATE (LOGNOT X))))

(DEFUN RL-ISSTATE (X)
  (DECLARE (FIXNUM X))
  (/= 0 (LOGAND RL-READLINE-STATE X)))


(DECLAIM
 (INLINE
   USING-HISTORY HISTORY-GET-HISTORY-STATE HISTORY-SET-HISTORY-STATE
   ADD-HISTORY REMOVE-HISTORY REPLACE-HISTORY-ENTRY CLEAR-HISTORY
   STIFLE-HISTORY UNSTIFLE-HISTORY HISTORY-IS-STIFLED HISTORY-LIST
   WHERE-HISTORY CURRENT-HISTORY HISTORY-GET HISTORY-TOTAL-BYTES
   HISTORY-SET-POS PREVIOUS-HISTORY NEXT-HISTORY HISTORY-SEARCH
   HISTORY-SEARCH-PREFIX HISTORY-SEARCH-POS READ-HISTORY
   READ-HISTORY-RANGE WRITE-HISTORY APPEND-HISTORY
   HISTORY-TRUNCATE-FILE HISTORY-EXPAND HISTORY-ARG-EXTRACT
   GET-HISTORY-EVENT HISTORY-TOKENIZE RL-DIGIT-ARGUMENT
   RL-UNIVERSAL-ARGUMENT RL-FORWARD RL-BACKWARD RL-BEG-OF-LINE
   RL-END-OF-LINE RL-FORWARD-WORD RL-BACKWARD-WORD RL-REFRESH-LINE
   RL-CLEAR-SCREEN RL-ARROW-KEYS RL-INSERT RL-QUOTED-INSERT
   RL-TAB-INSERT RL-NEWLINE RL-DO-LOWERCASE-VERSION RL-RUBOUT RL-DELETE
   RL-RUBOUT-OR-DELETE RL-DELETE-HORIZONTAL-SPACE
   RL-DELETE-OR-SHOW-COMPLETIONS RL-INSERT-COMMENT RL-UPCASE-WORD
   RL-DOWNCASE-WORD RL-CAPITALIZE-WORD RL-TRANSPOSE-WORDS
   RL-TRANSPOSE-CHARS RL-CHAR-SEARCH RL-BACKWARD-CHAR-SEARCH
   RL-BEGINNING-OF-HISTORY RL-END-OF-HISTORY RL-GET-NEXT-HISTORY
   RL-GET-PREVIOUS-HISTORY RL-SET-MARK RL-EXCHANGE-POINT-AND-MARK
   RL-VI-EDITING-MODE RL-EMACS-EDITING-MODE RL-RE-READ-INIT-FILE
   RL-DUMP-FUNCTIONS RL-DUMP-MACROS RL-DUMP-VARIABLES RL-COMPLETE
   RL-POSSIBLE-COMPLETIONS RL-INSERT-COMPLETIONS RL-MENU-COMPLETE
   RL-KILL-WORD RL-BACKWARD-KILL-WORD RL-KILL-LINE
   RL-BACKWARD-KILL-LINE RL-KILL-FULL-LINE RL-UNIX-WORD-RUBOUT
   RL-UNIX-LINE-DISCARD RL-COPY-REGION-TO-KILL RL-KILL-REGION
   RL-COPY-FORWARD-WORD RL-COPY-BACKWARD-WORD RL-YANK RL-YANK-POP
   RL-YANK-NTH-ARG RL-YANK-LAST-ARG RL-REVERSE-SEARCH-HISTORY
   RL-FORWARD-SEARCH-HISTORY RL-START-KBD-MACRO RL-END-KBD-MACRO
   RL-CALL-LAST-KBD-MACRO RL-REVERT-LINE RL-UNDO-COMMAND
   RL-TILDE-EXPAND RL-RESTART-OUTPUT RL-STOP-OUTPUT RL-ABORT
   RL-TTY-STATUS RL-HISTORY-SEARCH-FORWARD RL-HISTORY-SEARCH-BACKWARD
   RL-NONINC-FORWARD-SEARCH RL-NONINC-REVERSE-SEARCH
   RL-NONINC-FORWARD-SEARCH-AGAIN RL-NONINC-REVERSE-SEARCH-AGAIN
   RL-INSERT-CLOSE RL-CALLBACK-HANDLER-INSTALL RL-CALLBACK-READ-CHAR
   RL-CALLBACK-HANDLER-REMOVE RL-VI-REDO RL-VI-UNDO RL-VI-YANK-ARG
   RL-VI-FETCH-HISTORY RL-VI-SEARCH-AGAIN RL-VI-SEARCH RL-VI-COMPLETE
   RL-VI-TILDE-EXPAND RL-VI-PREV-WORD RL-VI-NEXT-WORD RL-VI-END-WORD
   RL-VI-INSERT-BEG RL-VI-APPEND-MODE RL-VI-APPEND-EOL RL-VI-EOF-MAYBE
   RL-VI-INSERTION-MODE RL-VI-MOVEMENT-MODE RL-VI-ARG-DIGIT
   RL-VI-CHANGE-CASE RL-VI-PUT RL-VI-COLUMN RL-VI-DELETE-TO
   RL-VI-CHANGE-TO RL-VI-YANK-TO RL-VI-DELETE RL-VI-BACK-TO-INDENT
   RL-VI-FIRST-PRINT RL-VI-CHAR-SEARCH RL-VI-MATCH RL-VI-CHANGE-CHAR
   RL-VI-SUBST RL-VI-OVERSTRIKE RL-VI-OVERSTRIKE-DELETE RL-VI-REPLACE
   RL-VI-SET-MARK RL-VI-GOTO-MARK RL-VI-CHECK RL-VI-DOMOVE
   RL-VI-BRACKTYPE RL-VI-FWORD RL-VI-BWORD RL-VI-EWORD RL-VI-FWORD
   RL-VI-BWORD RL-VI-EWORD READLINE RL-SET-PROMPT RL-EXPAND-PROMPT
   RL-INITIALIZE RL-DISCARD-ARGUMENT RL-ADD-DEFUN RL-BIND-KEY
   RL-BIND-KEY-IN-MAP RL-UNBIND-KEY RL-UNBIND-KEY-IN-MAP
   RL-UNBIND-FUNCTION-IN-MAP RL-UNBIND-COMMAND-IN-MAP RL-SET-KEY
   RL-GENERIC-BIND RL-VARIABLE-BIND RL-MACRO-BIND RL-NAMED-FUNCTION
   RL-FUNCTION-OF-KEYSEQ RL-LIST-FUNMAP-NAMES
   RL-INVOKING-KEYSEQS-IN-MAP RL-INVOKING-KEYSEQS RL-FUNCTION-DUMPER
   RL-MACRO-DUMPER RL-VARIABLE-DUMPER RL-READ-INIT-FILE
   RL-PARSE-AND-BIND RL-MAKE-BARE-KEYMAP RL-COPY-KEYMAP RL-MAKE-KEYMAP
   RL-DISCARD-KEYMAP RL-GET-KEYMAP-BY-NAME RL-GET-KEYMAP-NAME
   RL-SET-KEYMAP RL-GET-KEYMAP RL-SET-KEYMAP-FROM-EDIT-MODE
   RL-GET-KEYMAP-NAME-FROM-EDIT-MODE RL-ADD-FUNMAP-ENTRY
   RL-FUNMAP-NAMES RL-PUSH-MACRO-INPUT RL-ADD-UNDO RL-FREE-UNDO-LIST
   RL-DO-UNDO RL-BEGIN-UNDO-GROUP RL-END-UNDO-GROUP RL-MODIFYING
   RL-REDISPLAY RL-ON-NEW-LINE RL-ON-NEW-LINE-WITH-PROMPT
   RL-FORCED-UPDATE-DISPLAY RL-CLEAR-MESSAGE RL-RESET-LINE-STATE
   RL-CRLF RL-SHOW-CHAR RL-SAVE-PROMPT RL-RESTORE-PROMPT RL-INSERT-TEXT
   RL-DELETE-TEXT RL-KILL-TEXT RL-COPY-TEXT RL-PREP-TERMINAL
   RL-DEPREP-TERMINAL RL-TTY-SET-DEFAULT-BINDINGS RL-RESET-TERMINAL
   RL-RESIZE-TERMINAL RL-SET-SCREEN-SIZE RL-GET-SCREEN-SIZE
   RL-STUFF-CHAR RL-EXECUTE-NEXT RL-CLEAR-PENDING-INPUT RL-READ-KEY
   RL-GETC RL-SET-KEYBOARD-INPUT-TIMEOUT RL-EXTEND-LINE-BUFFER RL-DING
   RL-ALPHABETIC RL-SET-SIGNALS RL-CLEAR-SIGNALS
   RL-CLEANUP-AFTER-SIGNAL RL-RESET-AFTER-SIGNAL RL-FREE-LINE-STATE
   RL-MAYBE-UNSAVE-LINE RL-MAYBE-REPLACE-LINE RL-COMPLETE-INTERNAL
   RL-DISPLAY-MATCH-LIST RL-COMPLETION-MATCHES
   RL-USERNAME-COMPLETION-FUNCTION RL-FILENAME-COMPLETION-FUNCTION
   RL-SETSTATE RL-UNSETSTATE RL-ISSTATE ))



;;;; readline.lisp                    --                     --          ;;;;
