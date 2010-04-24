;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               ecma048-gen.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package loads the ECMA-048 standard text and from
;;;;    this specifications can generate functions returning
;;;;    the control sequences defined in the standard.
;;;;    The user must call for example:
;;;;
;;;;       (generate-all-functions :verbose *compile-verbose*
;;;;                               :compile t
;;;;                               :export  t
;;;;                               :8-bit   t
;;;;                               :print   nil
;;;;                               :result-type '(vector (unsigned-byte 8))))
;;;;
;;;;    to get the code function defined in the current package (*package*).
;;;;
;;;;    Note: ECMA-048 should be identical to ISO-6429.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-07-11 <PJB> Removed the code functions from this package, this
;;;;                     exporting them dynamically displeased to sbcl.
;;;;    2005-09-01 <PJB> Added ISO6429 control codes.
;;;;    2005-03-09 <PJB> Removed dependency on clisp specific REGEXP package.
;;;;    2004-10-01 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2006
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************
#|
 (eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package "COM.INFORMATIMAGO.COMMON-LISP.ECMA048")
    (delete-package "COM.INFORMATIMAGO.COMMON-LISP.ECMA048")))
|#

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ECMA048"
  (:NICKNAMES "ISO6429" "ECMA048" "COM.INFORMATIMAGO.COMMON-LISP.ISO6429")
  (:DOCUMENTATION
   "
    This package loads the ECMA-048 standard text and from
    this specifications can generate functions returning
    the control sequences defined in the standard.
    The user must call GENERATE-ALL-FUNCTIONS
    to get the code function defined in the current package (*package*).

    Note: ECMA-048 should be identical to ISO-6429.
    This package loads the Ecam-048 standard text and from it genrates
    functions returning the control sequences defined in the standard.
    Note: ECMA-048 should be the same as ISO-6429.


    Copyright Pascal J. Bourguignon 2004 - 2006
    
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version
    2 of the License, or (at your option) any later version.
   ")
  (:USE "COMMON-LISP" "COM.INFORMATIMAGO.COMMON-LISP.UTILITY")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.UTILITY" "DEFENUM")
  (:shadow "ED")
  (:EXPORT
   "CODE" "MAKE-CODE" "COPY-CODE"
   "CODE-REFERENCE" "CODE-NAME" "CODE-TITLE" "CODE-NOTATION"
   "CODE-REPRESENTATION" "CODE-DESCRIPTION" "CODE-DEFAULT"
   ;; The ECMA048 codes:
   "*CODES*"
   ;; Not yet: "LOAD-CODE-SPECIFICATIONS"
   "GENERATE-CODE-FUNCTION"       ; generate the function of one code.
   "GENERATE-ALL-FUNCTIONS"  ; generate the functions of all *CODES*. 
   "GENERATE-SHELL-FUNCTION" ; generate one shell funcntion of one code.
   "PRINT-SHELL-FUNCTIONS"  ; generate and prints all shell functions.
   "PRINT-DOCUMENTATION"      ; print the documentation of all *CODES*
   "GENERATE-ALL-FUNCTIONS-IN-ECMA048"
   
   #+sbcl"ACK"  #+sbcl"APC"  #+sbcl"BEL"  #+sbcl"BPH"  #+sbcl"BS"
   #+sbcl"CAN"  #+sbcl"CBT"  #+sbcl"CCH"  #+sbcl"CHA"  #+sbcl"CHT"
   #+sbcl"CMD"  #+sbcl"CNL"  #+sbcl"CPL"  #+sbcl"CPR"  #+sbcl"CR"
   #+sbcl"CSI"  #+sbcl"CTC"  #+sbcl"CUB"  #+sbcl"CUD"  #+sbcl"CUF"
   #+sbcl"CUP"  #+sbcl"CUU"  #+sbcl"CVT"  #+sbcl"DA"   #+sbcl"DAQ"
   #+sbcl"DC1"  #+sbcl"DC2"  #+sbcl"DC3"  #+sbcl"DC4"  #+sbcl"DCH"
   #+sbcl"DCS"  #+sbcl"DL"   #+sbcl"DLE"  #+sbcl"DMI"  #+sbcl"DSR"
   #+sbcl"DTA"  #+sbcl"EA"   #+sbcl"ECH"  #+sbcl"ED"   #+sbcl"EF"
   #+sbcl"EM"   #+sbcl"EMI"  #+sbcl"ENQ"  #+sbcl"EOT"  #+sbcl"EPA"
   #+sbcl"ESA"  #+sbcl"ESC"  #+sbcl"ETB"  #+sbcl"ETX"  #+sbcl"FF"
   #+sbcl"FNK"  #+sbcl"FNT"  #+sbcl"GCC"  #+sbcl"GSM"  #+sbcl"GSS"
   #+sbcl"HPA"  #+sbcl"HPB"  #+sbcl"HPR"  #+sbcl"HT"   #+sbcl"HTJ"
   #+sbcl"HTS"  #+sbcl"HVP"  #+sbcl"ICH"  #+sbcl"IDCS" #+sbcl"IGS"
   #+sbcl"IL"   #+sbcl"INT"  #+sbcl"IS1"  #+sbcl"IS2"  #+sbcl"IS3"
   #+sbcl"IS4"  #+sbcl"JFY"  #+sbcl"LF"   #+sbcl"LS0"  #+sbcl"LS1"
   #+sbcl"LS2"  #+sbcl"LS3"  #+sbcl"MC"   #+sbcl"MW"   #+sbcl"NAK"
   #+sbcl"NBH"  #+sbcl"NEL"  #+sbcl"NP"   #+sbcl"NUL"  #+sbcl"OSC"
   #+sbcl"PEC"  #+sbcl"PFS"  #+sbcl"PLD"  #+sbcl"PLU"  #+sbcl"PM"
   #+sbcl"PP"   #+sbcl"PPA"  #+sbcl"PPB"  #+sbcl"PPR"  #+sbcl"PTX"
   #+sbcl"PU1"  #+sbcl"PU2"  #+sbcl"QUAD" #+sbcl"REP"  #+sbcl"RI"
   #+sbcl"RIS"  #+sbcl"RM"   #+sbcl"SACS" #+sbcl"SAPV" #+sbcl"SCI"
   #+sbcl"SCO"  #+sbcl"SCP"  #+sbcl"SCS"  #+sbcl"SD"   #+sbcl"SDS"
   #+sbcl"SEE"  #+sbcl"SEF"  #+sbcl"SGR"  #+sbcl"SHS"  #+sbcl"SI"
   #+sbcl"SIMD" #+sbcl"SL"   #+sbcl"SLH"  #+sbcl"SLL"  #+sbcl"SLS"
   #+sbcl"SM"   #+sbcl"SO"   #+sbcl"SOH"  #+sbcl"SOS"  #+sbcl"SPA"
   #+sbcl"SPH"  #+sbcl"SPI"  #+sbcl"SPL"  #+sbcl"SPQR" #+sbcl"SR"
   #+sbcl"SRCS" #+sbcl"SRS"  #+sbcl"SS2"  #+sbcl"SS3"  #+sbcl"SSA"
   #+sbcl"SSU"  #+sbcl"SSW"  #+sbcl"ST"   #+sbcl"STAB" #+sbcl"STS"
   #+sbcl"STX"  #+sbcl"SU"   #+sbcl"SUB"  #+sbcl"SVS"  #+sbcl"SYN"
   #+sbcl"TAC"  #+sbcl"TALE" #+sbcl"TATE" #+sbcl"TBC"  #+sbcl"TCC"
   #+sbcl"TSR"  #+sbcl"TSS"  #+sbcl"VPA"  #+sbcl"VPB"  #+sbcl"VPR"
   #+sbcl"VT"   #+sbcl"VTS"  #+sbcl"SPD"  #+sbcl"EL"))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ECMA048")




;; http://kjell.haxx.se/man/man.cgi?page=Term::ANSIColor&section=3perl
;; Jean Delvare provided the following table of different common terminal
;; emulators and their support for the various attributes and others have
;; helped me flesh it out:
;; 
;;               clear    bold     dark    under    blink   reverse  conceal
;;  ------------------------------------------------------------------------
;;  xterm         yes      yes      no      yes     bold      yes      yes
;;  linux         yes      yes      yes    bold      yes      yes      no
;;  rxvt          yes      yes      no      yes  bold/black   yes      no
;;  dtterm        yes      yes      yes     yes    reverse    yes      yes
;;  teraterm      yes    reverse    no      yes    rev/red    yes      no
;;  aixterm      kinda   normal     no      yes      no       yes      yes
;;  PuTTY         yes     color     no      yes      no       yes      no
;;  Windows       yes      no       no      no       no       yes      no
;;  Cygwin SSH    yes      yes      no     color    color    color     yes
;;  Mac Terminal  yes      yes      no      yes      yes      yes      yes

;; From: Thomas Dickey <dickey@saltmine.radix.net>
;; Subject: Re: Text console
;; Newsgroups: comp.lang.lisp
;; Date: Thu, 05 Jan 2006 11:30:06 -0000
;; Organization: RadixNet Internet Services
;; 
;; John Thingstad <john.thingstad@chello.no> wrote:
;; > exerpt from  
;; > http://kjell.haxx.se/man/man.cgi?page=Term::ANSIColor&section=3perl
;; 
;; ...
;; 
;; >  Jean Delvare provided the following table of different common  terminal
;; >  emulators and their support for the various attributes and others  have
;; >  helped me flesh it out:
;; 
;; >                clear    bold     dark    under    blink   reverse   conceal
;; >   ------------------------------------------------------------------------
;; >   xterm         yes      yes      no      yes     bold      yes       yes
;; 
;; xterm implements blink (has done so for a few years).
;; 
;; >   linux         yes      yes      yes    bold      yes      yes       no
;; >   rxvt          yes      yes      no      yes  bold/black   yes       no
;; >   dtterm        yes      yes      yes     yes    reverse    yes       yes
;; >   teraterm      yes    reverse    no      yes    rev/red    yes       no
;; 
;; hmm - "dark" isn't mentioned in the lisp code.  Perhaps you mean "dim".
;; 
;; >   aixterm      kinda   normal     no      yes      no       yes       yes
;; >   PuTTY         yes     color     no      yes      no       yes       no
;; >   Windows       yes      no       no      no       no       yes       no
;; >   Cygwin SSH    yes      yes      no     color    color    color      yes
;; >   Mac Terminal  yes      yes      no      yes      yes      yes       yes
;; 
;; Cygwin SSH is not a terminal emulator.  There's a console window terminal
;; emulator within which one can run telnet or ssh.
;; 
;; PuTTY does bold.
;; 
;; Tera Term uses color (blue) for bold.
;; 
;; The comments about "clear" don't fit dtterm (it doesn't implement the same
;; color model as xterm, etc).
;; 
;; -- 
;; Thomas E. Dickey
;; http://invisible-island.net
;; ftp://invisible-island.net

#-sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow "ED"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The following  names and codes don't appear in ECMA048.
  ;; Some older ASCII names:
  (defconstant FS   #x1c "IS4 -- file separator")
  (defconstant GS   #x1d "IS3 -- group separator")
  (defconstant RS   #x1e "IS2 -- record separator")
  (defconstant US   #x1f "IS1 -- unit separator")
  (defconstant DEL  #x7f " -- delete")
  ;; Where are these defined?
  (defconstant PAD  #x80 " -- PADDING CHARACTER") 
  (defconstant HOP  #x81 " -- HIGH OCTET PRESET") 
  (defconstant IND  #x84 " -- INDEX") 
  (defconstant SGCI #x99 " -- SINGLE GRAPHIC CHARACTER INTRODUCER") 
  );;eval-when


#||
;; LOAD-CODE-SPECIFICATIONS uses the clisp-specific package REGEXP.
;; We'll uncomment it when COM.INFORMATIMAGO.COMMON-LISP.REGEXP will work.

(DEFUN LOAD-CODE-SPECIFICATIONS (&OPTIONAL (PATH "ecma048.txt"))
  "This function converts ecma048.txt files (basically, copy-pasted
   text from ecma048.pdf), to the following *CODES* s-expression."
  (defparameter *codes* '())
  (WITH-OPEN-FILE (IN PATH :DIRECTION :INPUT :IF-DOES-NOT-EXIST :ERROR)
    (LET ((CURRENT NIL)
          ALL REF NAME TITLE NOTATION REPRESENTATION DESCRIPTION DEFAULT)
      (FLET ((FINISH-CODE ()
               (WHEN CURRENT (PUSH CURRENT *CODES*))
               (SETF CURRENT (MAKE-CODE))))
        (LOOP FOR LINE = (READ-LINE IN NIL NIL)
           WHILE (AND LINE (STRING/= "BEGIN" (STRING-TRIM " " LINE))))
        (LOOP FOR LINE = (READ-LINE IN NIL NIL)
           WHILE LINE
           DO (COND
                ((STRING= "FINE" (STRING-TRIM " " LINE))
                 (FINISH-CODE)
                 (LOOP-FINISH))
                ((PROGN
                   (MULTIPLE-VALUE-SETQ (ALL REF NAME TITLE)
                     (REGEXP:MATCH
                      "^\\(8.3.[0-9]\\+\\) \\([A-Z]\\+[0-9]*\\) - \\(.*\\)"
                      LINE))
                   ALL)
                 (FINISH-CODE)
                 (SETF (CODE-REFERENCE CURRENT) (REGEXP:MATCH-STRING LINE REF)
                       (CODE-NAME CURRENT) (REGEXP:MATCH-STRING LINE NAME)
                       (CODE-TITLE CURRENT)
                       (STRING-TRIM " " (REGEXP:MATCH-STRING LINE TITLE))))
                ((PROGN
                   (MULTIPLE-VALUE-SETQ (ALL NOTATION)
                     (REGEXP:MATCH "^Notation: \\(.*\\)" LINE))  ALL)
                 (SETF (CODE-NOTATION CURRENT)
                       (STRING-TRIM
                        " " (REGEXP:MATCH-STRING LINE NOTATION))))
                ((PROGN
                   (MULTIPLE-VALUE-SETQ (ALL REPRESENTATION)
                     (REGEXP:MATCH "^Representation: \\(.*\\)" LINE))  ALL)
                 (SETF (CODE-REPRESENTATION CURRENT)
                       (STRING-TRIM
                        " " (REGEXP:MATCH-STRING LINE REPRESENTATION))))
                ((PROGN
                   (MULTIPLE-VALUE-SETQ (ALL DESCRIPTION)
                     (REGEXP:MATCH "^Description: \\(.*\\)" LINE))  ALL)
                 (SETF (CODE-DESCRIPTION CURRENT)
                       (STRING-TRIM
                        " " (REGEXP:MATCH-STRING LINE DESCRIPTION))))
                ((PROGN
                   (MULTIPLE-VALUE-SETQ (ALL DEFAULT)
                     (REGEXP:MATCH "^Parameter default value: \\(.*\\)" LINE))
                   ALL)
                 (SETF (CODE-DEFAULT CURRENT)
                       (STRING-TRIM
                        " " (REGEXP:MATCH-STRING LINE DEFAULT)))))))))
  (DOLIST (C *CODES*)
    (LET ((DEFA (CODE-DEFAULT C)))
      (WHEN DEFA
        (SETF (CODE-DEFAULT C)
              (CDR (ASSOC
                    DEFA
                    '( ("Ps1 = 0; Ps2 =0"       . ((PS1 . 0) (PS2 . 0)))
                      ("Pn1 = 100; Pn2 = 100"  . ((PN1 . 100) (PN2 . 100)))
                      ("Pn1 = 1; Pn2 = 1"      . ((PN1 . 1) (PN2 . 1)))
                      ("Ps1 = 0; Ps2 = 0"      . ((PS1 . 0) (PS2 . 0)))
                      ("Pn = 0"                . ((PN . 0)))
                      ("Ps = 0"                . ((PS . 0)))
                      ("NONE for Pn1"          . ((PN1 . :NONE)))
                      ("NONE"                  . ((* . :NONE))) 
                      ("Pn = 1"                . ((PN . 1))) )
                    :TEST (FUNCTION STRING=))))))
    (LET ((NOTA (CODE-NOTATION C)))
      (WHEN NOTA
        (SETF (CODE-NOTATION C)
              (CDR (ASSOC
                    NOTA
                    '( ("(Fs)"      . (FS)) 
                      ("(Ps...)"   . (PS...)) 
                      ("(Ps1;Ps2)" . (PS1 PS2)) 
                      ("(Ps)"      . (PS)) 
                      ("(Pn1;Pn2)" . (PN1 PN2)) 
                      ("(Pn)"      . (PN)) 
                      ("(C0)"      . (C0)) 
                      ("(C1)"      . (C1)) )
                    :TEST (FUNCTION STRING=))))))
    (LET ((REPR (CODE-REPRESENTATION C)))
      (WHEN REPR
        (SETF (CODE-REPRESENTATION C)
              (MAPCAR (LAMBDA (S) (REGEXP:REGEXP-SPLIT " " S))
                      (REGEXP:REGEXP-SPLIT " or "  REPR))))) )
  (SETF *CODES* (NREVERSE *CODES*))
  (values))

;; At compile-time or when loading the source without compiling,
;; but not when loading the compiled file, let's load the specifications:
;;
(eval-when (:compile-toplevel :execute)
  (load-code-specifications))

||#


(DEFSTRUCT CODE
  REFERENCE NAME TITLE NOTATION REPRESENTATION DESCRIPTION DEFAULT)

(DEFPARAMETER *CODES* 
  (mapcar
   (lambda (data) (apply (function make-code) data))
   '((:REFERENCE "8.3.1" :NAME "ACK" :TITLE "ACKNOWLEDGE" :NOTATION (C0)
      :REPRESENTATION (("00/06"))
      :DESCRIPTION
      "ACK is transmitted by a receiver as an affirmative response
    to the sender. The use of ACK is defined in ISO 1745."
      :DEFAULT NIL)
     (:REFERENCE "8.3.2" :NAME "APC" :TITLE "APPLICATION PROGRAM COMMAND"
      :NOTATION (C1) :REPRESENTATION (("09/15") ("ESC" "05/15"))
      :DESCRIPTION
      "APC is used as the opening delimiter of a control string for
    application program use. The command string following may consist of
    bit combinations in the range 00/08 to 00/13 and 02/00 to 07/14. The
    control string is closed by the terminating delimiter STRING
    TERMINATOR (ST). The interpretation of the command string depends on
    the relevant application program."
      :DEFAULT NIL)
     (:REFERENCE "8.3.3" :NAME "BEL" :TITLE "BELL" :NOTATION (C0)
      :REPRESENTATION (("00/07"))
      :DESCRIPTION
      "BEL is used when there is a need to call for attention; it
    may control alarm or attention devices."
      :DEFAULT NIL)
     (:REFERENCE "8.3.4" :NAME "BPH" :TITLE "BREAK PERMITTED HERE"
      :NOTATION (C1) :REPRESENTATION (("08/02") ("ESC" "04/02"))
      :DESCRIPTION
      "BPH is used to indicate a point where a line break may occur
    when text is formatted. BPH may occur between two graphic characters,
    either or both of which may be SPACE."
      :DEFAULT NIL)
     (:REFERENCE "8.3.5" :NAME "BS" :TITLE "BACKSPACE" :NOTATION (C0)
      :REPRESENTATION (("00/08"))
      :DESCRIPTION
      "BS causes the active data position to be moved one character
    position in the data component in the direction opposite to that of
    the implicit movement. The direction of the implicit movement depends
    on the parameter value of SELECT IMPLICIT MOVEMENT DIRECTION (SIMD)."
      :DEFAULT NIL)
     (:REFERENCE "8.3.6" :NAME "CAN" :TITLE "CANCEL" :NOTATION (C0)
      :REPRESENTATION (("01/08"))
      :DESCRIPTION
      "CAN is used to indicate that the data preceding it in the
    data stream is in error. As a result, this data shall be ignored. The
    specific meaning of this control function shall be defined for each
    application and/or between sender and recipient."
      :DEFAULT NIL)
     (:REFERENCE "8.3.7" :NAME "CBT" :TITLE "CURSOR BACKWARD TABULATION"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "05/10"))
      :DESCRIPTION
      "CBT causes the active presentation position to be moved to
    the character position corresponding to the n-th preceding character
    tabulation stop in the presentation component, according to the
    character path, where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.8" :NAME "CCH" :TITLE "CANCEL CHARACTER" :NOTATION (C1)
      :REPRESENTATION (("09/04") ("ESC" "05/04"))
      :DESCRIPTION
      "CCH is used to indicate that both the preceding graphic
    character in the data stream, (represented by one or more bit
    combinations) including SPACE, and the control function CCH itself are
    to be ignored for further interpretation of the data stream. If the
    character preceding CCH in the data stream is a control function
    (represented by one or more bit combinations), the effect of CCH is
    not defined by this Standard."
      :DEFAULT NIL)
     (:REFERENCE "8.3.9" :NAME "CHA" :TITLE "CURSOR CHARACTER ABSOLUTE"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "04/07"))
      :DESCRIPTION
      "CHA causes the active presentation position to be moved to
    character position n in the active line in the presentation component,
    where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.10" :NAME "CHT" :TITLE "CURSOR FORWARD TABULATION"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "04/09"))
      :DESCRIPTION
      "CHT causes the active presentation position to be moved to
    the character position corresponding to the n-th following character
    tabulation stop in the presentation component, according to the
    character path, where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.11" :NAME "CMD" :TITLE "CODING METHOD DELIMITER"
      :NOTATION (FS) :REPRESENTATION (("ESC" "06/04"))
      :DESCRIPTION
      "CMD is used as the delimiter of a string of data coded
    according to Standard ECMA-35 and to switch to a general level of
    control. The use of CMD is not mandatory if the higher level protocol
    defines means of delimiting the string, for instance, by specifying
    the length of the string."
      :DEFAULT NIL)
     (:REFERENCE "8.3.12" :NAME "CNL" :TITLE "CURSOR NEXT LINE"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "04/05"))
      :DESCRIPTION
      "CNL causes  the active presentation  position to be  moved to
    the first  character position  of the n-th  following line  in the
    presentation component, where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.13" :NAME "CPL" :TITLE "CURSOR PRECEDING LINE"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "04/06"))
      :DESCRIPTION
      "CPL causes the active presentation position to be moved to
    the first character position of the n-th preceding line in the
    presentation component, where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.14" :NAME "CPR" :TITLE "ACTIVE POSITION REPORT"
      :NOTATION (PN1 PN2) :REPRESENTATION (("CSI" "Pn1;Pn2" "05/02"))
      :DESCRIPTION
      "If the DEVICE COMPONENT SELECT MODE (DCSM) is set to
    PRESENTATION, CPR is used to report the active presentation position
    of the sending device as residing in the presentation component at the
    n-th line position according to the line progression and at the m-th
    character position according to the character path, where n equals the
    value of Pn1 and m equals the value of Pn2. If the DEVICE COMPONENT
    SELECT MODE (DCSM) is set to DATA, CPR is used to report the active
    data position of the sending device as residing in the data component
    at the n-th line position according to the line progression and at the
    m-th character position according to the character progression, where
    n equals the value of Pn1 and m equals the value of Pn2. CPR may be
    solicited by a DEVICE STATUS REPORT (DSR) or be sent unsolicited."
      :DEFAULT ((PN1 . 1) (PN2 . 1)))
     (:REFERENCE "8.3.15" :NAME "CR" :TITLE "CARRIAGE RETURN" :NOTATION (C0)
      :REPRESENTATION (("00/13"))
      :DESCRIPTION
      "The effect of CR depends on the setting of the DEVICE
    COMPONENT SELECT MODE (DCSM) and on the parameter value of SELECT
    IMPLICIT MOVEMENT DIRECTION (SIMD). If the DEVICE COMPONENT SELECT
    MODE (DCSM) is set to PRESENTATION and with the parameter value of
    SIMD equal to 0, CR causes the active presentation position to be
    moved to the line home position of the same line in the presentation
    component. The line home position is established by the parameter
    value of SET LINE HOME (SLH). With a parameter value of SIMD equal to
    1, CR causes the active presentation position to be moved to the line
    limit position of the same line in the presentation component. The
    line limit position is established by the parameter value of SET LINE
    LIMIT (SLL). If the DEVICE COMPONENT SELECT MODE (DCSM) is set to DATA
    and with a parameter value of SIMD equal to 0, CR causes the active
    data position to be moved to the line home position of the same line
    in the data component. The line home position is established by the
    parameter value of SET LINE HOME (SLH). With a parameter value of SIMD
    equal to 1, CR causes the active data position to be moved to the line
    limit position of the same line in the data component. The line limit
    position is established by the parameter value of SET LINE LIMIT
    (SLL)."
      :DEFAULT NIL)
     (:REFERENCE "8.3.16" :NAME "CSI" :TITLE "CONTROL SEQUENCE INTRODUCER"
      :NOTATION (C1) :REPRESENTATION (("09/11") ("ESC" "05/11"))
      :DESCRIPTION
      "CSI is used as the first character of a control sequence, see 5.4."
      :DEFAULT NIL)
     (:REFERENCE "8.3.17" :NAME "CTC" :TITLE "CURSOR TABULATION CONTROL"
      :NOTATION (PS...) :REPRESENTATION (("CSI" "Ps..." "05/07"))
      :DESCRIPTION
      "CTC causes one or more tabulation stops to be set or cleared
    in the presentation component, depending on the parameter values:"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.18" :NAME "CUB" :TITLE "CURSOR LEFT" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "04/04"))
      :DESCRIPTION
      "CUB causes the active presentation position to be moved
    leftwards in the presentation component by n character positions if
    the character path is horizontal, or by n line positions if the
    character path is vertical, where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.19" :NAME "CUD" :TITLE "CURSOR DOWN" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "04/02"))
      :DESCRIPTION
      "CUD causes the active presentation position to be moved
    downwards in the presentation component by n line positions if the
    character path is horizontal, or by n character positions if the
    character path is vertical, where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.20" :NAME "CUF" :TITLE "CURSOR RIGHT" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "04/03"))
      :DESCRIPTION
      "CUF causes the active presentation position to be moved
    rightwards in the presentation component by n character positions if
    the character path is horizontal, or by n line positions if the
    character path is vertical, where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.21" :NAME "CUP" :TITLE "CURSOR POSITION"
      :NOTATION (PN1 PN2) :REPRESENTATION (("CSI" "Pn1;Pn2" "04/08"))
      :DESCRIPTION
      "CUP causes the active presentation position to be moved in
    the presentation component to the n-th line position according to the
    line progression and to the m-th character position according to the
    character path, where n equals the value of Pn1 and m equals the value
    of Pn2."
      :DEFAULT ((PN1 . 1) (PN2 . 1)))
     (:REFERENCE "8.3.22" :NAME "CUU" :TITLE "CURSOR UP" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "04/01"))
      :DESCRIPTION
      "CUU causes the active presentation position to be moved
    upwards in the presentation component by n line positions if the
    character path is horizontal, or by n character positions if the
    character path is vertical, where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.23" :NAME "CVT" :TITLE "CURSOR LINE TABULATION"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "05/09"))
      :DESCRIPTION
      "CVT causes the active presentation position to be moved to
    the corresponding character position of the line corresponding to the
    n-th following line tabulation stop in the presentation component,
    where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.24" :NAME "DA" :TITLE "DEVICE ATTRIBUTES"
      :NOTATION (PS) :REPRESENTATION (("CSI" "Ps" "06/03"))
      :DESCRIPTION
      "With a parameter value not equal to 0, DA is used to identify
    the device which sends the DA. The parameter value is a device type
    identification code according to a register which is to be
    established. If the parameter value is 0, DA is used to request an
    identifying DA from a device."
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.25" :NAME "DAQ" :TITLE "DEFINE AREA QUALIFICATION"
      :NOTATION (PS...) :REPRESENTATION (("CSI" "Ps..." "06/15"))
      :DESCRIPTION
      "DAQ is used to indicate that the active presentation position
    in the presentation component is the first character position of a
    qualified area. The last character position of the qualified area is
    the character position in the presentation component immediately
    preceding the first character position of the following qualified
    area. The parameter value designates the type of qualified area:"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.26" :NAME "DCH" :TITLE "DELETE CHARACTER"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "05/00"))
      :DESCRIPTION
      "If the DEVICE COMPONENT SELECT MODE (DCSM) is set to
    PRESENTATION, DCH causes the contents of the active presentation
    position and, depending on the setting of the CHARACTER EDITING MODE
    (HEM), the contents of the n-1 preceding or following character
    positions to be removed from the presentation component, where n
    equals the value of Pn. The resulting gap is closed by shifting the
    contents of the adjacent character positions towards the active
    presentation position. At the other end of the shifted part, n
    character positions are put into the erased state. The extent of the
    shifted part is established by SELECT EDITING EXTENT (SEE). The effect
    of DCH on the start or end of a selected area, the start or end of a
    qualified area, or a tabulation stop in the shifted part is not
    defined by this Standard. If the DEVICE COMPONENT SELECT MODE (DCSM)
    is set to DATA, DCH causes the contents of the active data position
    and, depending on the setting of the CHARACTER EDITING MODE (HEM), the
    contents of the n-1 preceding or following character positions to be
    removed from the data component, where n equals the value of Pn. The
    resulting gap is closed by shifting the contents of the adjacent
    character positions towards the active data position. At the other end
    of the shifted part, n character positions are put into the erased
    state."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.27" :NAME "DCS" :TITLE "DEVICE CONTROL STRING"
      :NOTATION (C1) :REPRESENTATION (("09/00") ("ESC" "05/00"))
      :DESCRIPTION
      "DCS is used as the opening delimiter of a control string for
    device control use. The command string following may consist of
    bit combinations in the range 00/08 to 00/13 and 02/00 to
    07/14. The control string is closed by the terminating delimiter
    STRING TERMINATOR (ST). The command string represents either one
    or more commands for the receiving device, or one or more status
    reports from the sending device. The purpose and the format of the
    command string are specified by the most recent occurrence of
    IDENTIFY DEVICE CONTROL STRING (IDCS), if any, or depend on the
    sending and/or the receiving device."
      :DEFAULT NIL)
     (:REFERENCE "8.3.28" :NAME "DC1" :TITLE "DEVICE CONTROL ONE"
      :NOTATION (C0) :REPRESENTATION (("01/01"))
      :DESCRIPTION
      "DC1 is primarily intended for turning on or starting an
    ancillary device. If it is not required for this purpose, it may be
    used to restore a device to the basic mode of operation (see also DC2
    and DC3), or any other device control function not provided by other
    DCs."
      :DEFAULT NIL)
     (:REFERENCE "8.3.29" :NAME "DC2" :TITLE "DEVICE CONTROL TWO"
      :NOTATION (C0) :REPRESENTATION (("01/02"))
      :DESCRIPTION
      "DC2 is primarily intended for turning on or starting an
    ancillary device. If it is not required for this purpose, it may be
    used to set a device to a special mode of operation (in which case DC1
    is used to restore the device to the basic mode), or for any other
    device control function not provided by other DCs."
      :DEFAULT NIL)
     (:REFERENCE "8.3.30" :NAME "DC3" :TITLE "DEVICE CONTROL THREE"
      :NOTATION (C0) :REPRESENTATION (("01/03"))
      :DESCRIPTION
      "DC3 is primarily intended for turning off or stopping an
    ancillary device. This function may be a secondary level stop, for
    example wait, pause, stand-by or halt (in which case DC1 is used to
    restore normal operation). If it is not required for this purpose, it
    may be used for any other device control function not provided by
    other DCs."
      :DEFAULT NIL)
     (:REFERENCE "8.3.31" :NAME "DC4" :TITLE "DEVICE CONTROL FOUR"
      :NOTATION (C0) :REPRESENTATION (("01/04"))
      :DESCRIPTION
      "DC4 is primarily intended for turning off, stopping or
    interrupting an ancillary device. If it is not required for this
    purpose, it may be used for any other device control function not
    provided by other DCs."
      :DEFAULT NIL)
     (:REFERENCE "8.3.32" :NAME "DL" :TITLE "DELETE LINE" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "04/13"))
      :DESCRIPTION
      "If the DEVICE COMPONENT SELECT MODE (DCSM) is set to
    PRESENTATION, DL causes the contents of the active line (the line that
    contains the active presentation position) and, depending on the
    setting of the LINE EDITING MODE (VEM), the contents of the n-1
    preceding or following lines to be removed from the presentation
    component, where n equals the value of Pn. The resulting gap is closed
    by shifting the contents of a number of adjacent lines towards the
    active line. At the other end of the shifted part, n lines are put
    into the erased state. The active presentation position is moved to
    the line home position in the active line. The line home position is
    established by the parameter value of SET LINE HOME (SLH). If the
    TABULATION STOP MODE (TSM) is set to SINGLE, character tabulation
    stops are cleared in the lines that are put into the erased state. The
    extent of the shifted part is established by SELECT EDITING EXTENT
    (SEE). Any occurrences of the start or end of a selected area, the
    start or end of a qualified area, or a tabulation stop in the shifted
    part, are also shifted. If the DEVICE COMPONENT SELECT MODE (DCSM) is
    set to DATA, DL causes the contents of the active line (the line that
    contains the active data position) and, depending on the setting of
    the LINE EDITING MODE (VEM), the contents of the n-1 preceding or
    following lines to be removed from the data component, where n equals
    the value of Pn. The resulting gap is closed by shifting the contents
    of a number of adjacent lines towards the active line. At the other
    end of the shifted part, n lines are put into the erased state. The
    active data position is moved to the line home position in the active
    line. The line home position is established by the parameter value of
    SET LINE HOME (SLH)."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.33" :NAME "DLE" :TITLE "DATA LINK ESCAPE"
      :NOTATION (C0) :REPRESENTATION (("01/00"))
      :DESCRIPTION
      "DLE is used exclusively to provide supplementary transmission
    control functions. The use of DLE is defined in ISO 1745."
      :DEFAULT NIL)
     (:REFERENCE "8.3.34" :NAME "DMI" :TITLE "DISABLE MANUAL INPUT"
      :NOTATION (FS) :REPRESENTATION (("ESC" "06/00"))
      :DESCRIPTION
      "DMI causes the manual input facilities of a device to be disabled."
      :DEFAULT NIL)
     (:REFERENCE "8.3.35" :NAME "DSR" :TITLE "DEVICE STATUS REPORT"
      :NOTATION (PS) :REPRESENTATION (("CSI" "Ps" "06/14"))
      :DESCRIPTION
      "DSR is used either to report the status of the sending device
    or to request a status report from the receiving device, depending on
    the parameter values:"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.36" :NAME "DTA" :TITLE "DIMENSION TEXT AREA"
      :NOTATION (PN1 PN2) :REPRESENTATION (("CSI" "Pn1;Pn2" "02/00" "05/04"))
      :DESCRIPTION
      "DTA is used to establish the dimensions of the text area for
    subsequent pages. The established dimensions remain in effect until
    the next occurrence of DTA in the data stream. Pn1 specifies the
    dimension in the direction perpendicular to the line orientation Pn2
    specifies the dimension in the direction parallel to the line
    orientation The unit in which the parameter value is expressed is that
    established by the parameter value of SELECT SIZE UNIT (SSU)."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.37" :NAME "EA" :TITLE "ERASE IN AREA" :NOTATION (PS)
      :REPRESENTATION (("CSI" "Ps" "04/15"))
      :DESCRIPTION
      "If the DEVICE COMPONENT SELECT MODE (DCSM) is set to
    PRESENTATION, EA causes some or all character positions in the active
    qualified area (the qualified area in the presentation component which
    contains the active presentation position) to be put into the erased
    state, depending on the parameter values:"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.38" :NAME "ECH" :TITLE "ERASE CHARACTER" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "05/08"))
      :DESCRIPTION
      "If the DEVICE COMPONENT SELECT MODE (DCSM) is set to
    PRESENTATION, ECH causes the active presentation position and the n-1
    following character positions in the presentation component to be put
    into the erased state, where n equals the value of Pn. If the DEVICE
    COMPONENT SELECT MODE (DCSM) is set to DATA, ECH causes the active
    data position and the n-1 following character positions in the data
    component to be put into the erased state, where n equals the value of
    Pn. Whether the character positions of protected areas are put into
    the erased state, or the character positions of unprotected areas
    only, depends on the setting of the ERASURE MODE (ERM)."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.39" :NAME "ED" :TITLE "ERASE IN PAGE" :NOTATION (PS)
      :REPRESENTATION (("CSI" "Ps" "04/10"))
      :DESCRIPTION
      "If the DEVICE COMPONENT SELECT MODE (DCSM) is set to
    PRESENTATION, ED causes some or all character positions of the active
    page (the page which contains the active presentation position in the
    presentation component) to be put into the erased state, depending on
    the parameter values:"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.40" :NAME "EF" :TITLE "ERASE IN FIELD" :NOTATION (PS)
      :REPRESENTATION (("CSI" "Ps" "04/14"))
      :DESCRIPTION
      "If the DEVICE COMPONENT SELECT MODE (DCSM) is set to
    PRESENTATION, EF causes some or all character positions of the active
    field (the field which contains the active presentation position in
    the presentation component) to be put into the erased state, depending
    on the parameter values:"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.41" :NAME "EL" :TITLE "ERASE IN LINE" :NOTATION (PS)
      :REPRESENTATION (("CSI" "Ps" "04/11"))
      :DESCRIPTION
      "If the DEVICE COMPONENT SELECT MODE (DCSM) is set to
    PRESENTATION, EL causes some or all character positions of the active
    line (the line which contains the active presentation position in the
    presentation component) to be put into the erased state, depending on
    the parameter values:"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.42" :NAME "EM" :TITLE "END OF MEDIUM" :NOTATION (C0)
      :REPRESENTATION (("01/09"))
      :DESCRIPTION
      "EM is used to identify the physical end of a medium, or the
    end of the used portion of a medium, or the end of the wanted portion
    of data recorded on a medium."
      :DEFAULT NIL)
     (:REFERENCE "8.3.43" :NAME "EMI" :TITLE "ENABLE MANUAL INPUT"
      :NOTATION (FS) :REPRESENTATION (("ESC" "06/02"))
      :DESCRIPTION
      "EMI is used to enable the manual input facilities of a device."
      :DEFAULT NIL)
     (:REFERENCE "8.3.44" :NAME "ENQ" :TITLE "ENQUIRY" :NOTATION (C0)
      :REPRESENTATION (("00/05"))
      :DESCRIPTION
      "ENQ is transmitted by a sender as a request for a response
    from a receiver. The use of ENQ is defined in ISO 1745."
      :DEFAULT NIL)
     (:REFERENCE "8.3.45" :NAME "EOT" :TITLE "END OF TRANSMISSION"
      :NOTATION (C0) :REPRESENTATION (("00/04"))
      :DESCRIPTION
      "EOT is used to indicate the conclusion of the transmission of
    one or more texts. The use of EOT is defined in ISO 1745."
      :DEFAULT NIL)
     (:REFERENCE "8.3.46" :NAME "EPA" :TITLE "END OF GUARDED AREA"
      :NOTATION (C1) :REPRESENTATION (("09/07") ("ESC" "05/07"))
      :DESCRIPTION
      "EPA is used to indicate that the active presentation position
    is the last of a string of character positions in the presentation
    component, the contents of which are protected against manual
    alteration, are guarded against transmission or transfer, depending on
    the setting of the GUARDED AREA TRANSFER MODE (GATM), and may be
    protected against erasure, depending on the setting of the ERASURE
    MODE (ERM). The beginning of this string is indicated by START OF
    GUARDED AREA (SPA)."
      :DEFAULT NIL)
     (:REFERENCE "8.3.47" :NAME "ESA" :TITLE "END OF SELECTED AREA"
      :NOTATION (C1) :REPRESENTATION (("08/07") ("ESC" "04/07"))
      :DESCRIPTION
      "ESA is used to indicate that the active presentation position
    is the last of a string of character positions in the presentation
    component, the contents of which are eligible to be transmitted in the
    form of a data stream or transferred to an auxiliary input/output
    device. The beginning of this string is indicated by START OF SELECTED
    AREA (SSA)."
      :DEFAULT NIL)
     (:REFERENCE "8.3.48" :NAME "ESC" :TITLE "ESCAPE" :NOTATION (C0)
      :REPRESENTATION (("01/11"))
      :DESCRIPTION
      "ESC is used for code extension purposes. It causes the
    meanings of a limited number of bit combinations following it in the
    data stream to be changed. The use of ESC is defined in Standard
    ECMA-35."
      :DEFAULT NIL)
     (:REFERENCE "8.3.49" :NAME "ETB" :TITLE "END OF TRANSMISSION BLOCK"
      :NOTATION (C0) :REPRESENTATION (("01/07"))
      :DESCRIPTION
      "ETB is used to indicate the end of a block of data where the
    data are divided into such blocks for transmission purposes. The use
    of ETB is defined in ISO 1745."
      :DEFAULT NIL)
     (:REFERENCE "8.3.50" :NAME "ETX" :TITLE "END OF TEXT" :NOTATION (C0)
      :REPRESENTATION (("00/03"))
      :DESCRIPTION
      "ETX is used to indicate the end of a text. The use of ETX is
    defined in ISO 1745."
      :DEFAULT NIL)
     (:REFERENCE "8.3.51" :NAME "FF" :TITLE "FORM FEED" :NOTATION (C0)
      :REPRESENTATION (("00/12"))
      :DESCRIPTION
      "FF causes the active presentation position to be moved to the
    corresponding character position of the line at the page home position
    of the next form or page in the presentation component. The page home
    position is established by the parameter value of SET PAGE HOME
    (SPH)."
      :DEFAULT NIL)
     (:REFERENCE "8.3.52" :NAME "FNK" :TITLE "FUNCTION KEY" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "02/00" "05/07"))
      :DESCRIPTION
      "FNK is a control function in which the parameter value
    identifies the function key which has been operated."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.53" :NAME "FNT" :TITLE "FONT SELECTION"
      :NOTATION (PS1 PS2) :REPRESENTATION (("CSI" "Ps1;Ps2" "02/00" "04/04"))
      :DESCRIPTION
      "FNT is used to identify the character font to be selected as
    primary or alternative font by subsequent occurrences of SELECT
    GRAPHIC RENDITION (SGR) in the data stream. Ps"
      :DEFAULT ((PS1 . 0) (PS2 . 0)))
     (:REFERENCE "8.3.54" :NAME "GCC" :TITLE "GRAPHIC CHARACTER COMBINATION"
      :NOTATION (PS) :REPRESENTATION (("CSI" "Ps" "02/00" "05/15"))
      :DESCRIPTION
      "GCC is used to indicate that two or more graphic characters
    are to be imaged as one single graphic symbol. GCC with a parameter
    value of"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.55" :NAME "GSM" :TITLE "GRAPHIC SIZE MODIFICATION"
      :NOTATION (PN1 PN2) :REPRESENTATION (("CSI" "Pn1;Pn2" "02/00" "04/02"))
      :DESCRIPTION
      "GSM is used to modify for subsequent text the height and/or
    the width of all primary and alternative fonts identified by FONT
    SELECTION (FNT) and established by GRAPHIC SIZE SELECTION (GSS). The
    established values remain in effect until the next occurrence of GSM
    or GSS in the data steam. Pn"
      :DEFAULT ((PN1 . 100) (PN2 . 100)))
     (:REFERENCE "8.3.56" :NAME "GSS" :TITLE "GRAPHIC SIZE SELECTION"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "02/00" "04/03"))
      :DESCRIPTION
      "GSS is used to establish for subsequent text the height and
    the width of all primary and alternative fonts identified by FONT
    SELECTION (FNT). The established values remain in effect until the
    next occurrence of GSS in the data stream. Pn specifies the height,
    the width is implicitly defined by the height. The unit in which the
    parameter value is expressed is that established by the parameter
    value of SELECT SIZE UNIT (SSU)."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.57" :NAME "HPA" :TITLE "CHARACTER POSITION ABSOLUTE"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "06/00"))
      :DESCRIPTION
      "HPA causes the active data position to be moved to character
    position n in the active line (the line in the data component that
    contains the active data position), where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.58" :NAME "HPB" :TITLE "CHARACTER POSITION BACKWARD"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "06/10"))
      :DESCRIPTION
      "HPB causes the active data position to be moved by n
    character positions in the data component in the direction opposite to
    that of the character progression, where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.59" :NAME "HPR" :TITLE "CHARACTER POSITION FORWARD"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "06/01"))
      :DESCRIPTION
      "HPR causes the active data position to be moved by n
    character positions in the data component in the direction of the
    character progression, where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.60" :NAME "HT" :TITLE "CHARACTER TABULATION"
      :NOTATION (C0) :REPRESENTATION (("00/09"))
      :DESCRIPTION
      "HT causes the active presentation position to be moved to the
    following character tabulation stop in the presentation component. In
    addition, if that following character tabulation stop has been set by
    TABULATION ALIGN CENTRE (TAC), TABULATION ALIGN LEADING EDGE (TALE),
    TABULATION ALIGN TRAILING EDGE (TATE) or TABULATION CENTRED ON
    CHARACTER (TCC), HT indicates the beginning of a string of text which
    is to be positioned within a line according to the properties of that
    tabulation stop. The end of the string is indicated by the next
    occurrence of HT or CARRIAGE RETURN (CR) or NEXT LINE (NEL) in the
    data stream."
      :DEFAULT NIL)
     (:REFERENCE "8.3.61" :NAME "HTJ"
      :TITLE "CHARACTER TABULATION WITH JUSTIFICATION" :NOTATION (C1)
      :REPRESENTATION (("08/09") ("ESC" "04/09"))
      :DESCRIPTION
      "HTJ causes the contents of the active field (the field in the
    presentation component that contains the active presentation position)
    to be shifted forward so that it ends at the character position
    preceding the following character tabulation stop. The active
    presentation position is moved to that following character tabulation
    stop. The character positions which precede the beginning of the
    shifted string are put into the erased state."
      :DEFAULT NIL)
     (:REFERENCE "8.3.62" :NAME "HTS" :TITLE "CHARACTER TABULATION SET"
      :NOTATION (C1) :REPRESENTATION (("08/08") ("ESC" "04/08"))
      :DESCRIPTION
      "HTS causes a character tabulation stop to be set at the
    active presentation position in the presentation component. The number
    of lines affected depends on the setting of the TABULATION STOP MODE
    (TSM)."
      :DEFAULT NIL)
     (:REFERENCE "8.3.63" :NAME "HVP" :TITLE "CHARACTER AND LINE POSITION"
      :NOTATION (PN1 PN2) :REPRESENTATION (("CSI" "Pn1;Pn2" "06/06"))
      :DESCRIPTION
      "HVP causes the active data position to be moved in the data
    component to the n-th line position according to the line progression
    and to the m-th character position according to the character
    progression, where n equals the value of Pn1 and m equals the value of
    Pn2."
      :DEFAULT ((PN1 . 1) (PN2 . 1)))
     (:REFERENCE "8.3.64" :NAME "ICH" :TITLE "INSERT CHARACTER"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "04/00"))
      :DESCRIPTION
      "If the DEVICE COMPONENT SELECT MODE (DCSM) is set to
    PRESENTATION, ICH is used to prepare the insertion of n characters, by
    putting into the erased state the active presentation position and,
    depending on the setting of the CHARACTER EDITING MODE (HEM), the n-1
    preceding or following character positions in the presentation
    component, where n equals the value of Pn. The previous contents of
    the active presentation position and an adjacent string of character
    positions are shifted away from the active presentation position. The
    contents of n character positions at the other end of the shifted part
    are removed. The active presentation position is moved to the line
    home position in the active line. The line home position is
    established by the parameter value of SET LINE HOME (SLH). The extent
    of the shifted part is established by SELECT EDITING EXTENT (SEE). The
    effect of ICH on the start or end of a selected area, the start or end
    of a qualified area, or a tabulation stop in the shifted part, is not
    defined by this Standard. If the DEVICE COMPONENT SELECT MODE (DCSM)
    is set to DATA, ICH is used to prepare the insertion of n characters,
    by putting into the erased state the active data position and,
    depending on the setting of the CHARACTER EDITING MODE (HEM), the n-1
    preceding or following character positions in the data component,
    where n equals the value of Pn. The previous contents of the active
    data position and an adjacent string of character positions are
    shifted away from the active data position. The contents of n
    character positions at the other end of the shifted part are
    removed. The active data position is moved to the line home position
    in the active line. The line home position is established by the
    parameter value of SET LINE HOME (SLH)."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.65" :NAME "IDCS"
      :TITLE "IDENTIFY DEVICE CONTROL STRING" :NOTATION (PS)
      :REPRESENTATION (("CSI" "Ps" "02/00" "04/15"))
      :DESCRIPTION
      "IDCS is used to specify the purpose and format of the command
    string of subsequent DEVICE CONTROL STRINGs (DCS). The specified
    purpose and format remain in effect until the next occurrence of IDCS
    in the data stream. The parameter values are"
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.66" :NAME "IGS" :TITLE "IDENTIFY GRAPHIC SUBREPERTOIRE"
      :NOTATION (PS) :REPRESENTATION (("CSI" "Ps" "02/00" "04/13"))
      :DESCRIPTION
      "IGS is used to indicate that a repertoire of the graphic
    characters of ISO/IEC 10367 is used in the subsequent text. The
    parameter value of IGS identifies a graphic character repertoire
    registered in accordance with ISO/IEC 7350."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.67" :NAME "IL" :TITLE "INSERT LINE" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "04/12"))
      :DESCRIPTION
      "If the DEVICE COMPONENT SELECT MODE (DCSM) is set to
    PRESENTATION, IL is used to prepare the insertion of n lines, by
    putting into the erased state in the presentation component the active
    line (the line that contains the active presentation position) and,
    depending on the setting of the LINE EDITING MODE (VEM), the n-"
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.68" :NAME "INT" :TITLE "INTERRUPT" :NOTATION (FS)
      :REPRESENTATION (("ESC" "06/01"))
      :DESCRIPTION
      "INT is used to indicate to the receiving device that the
    current process is to be interrupted and an agreed procedure is to be
    initiated. This control function is applicable to either direction of
    transmission."
      :DEFAULT NIL)
     (:REFERENCE "8.3.69" :NAME "IS1"
      :TITLE "INFORMATION SEPARATOR ONE (US - UNIT SEPARATOR)" :NOTATION (C0)
      :REPRESENTATION (("01/15"))
      :DESCRIPTION
      "IS1 is used to separate and qualify data logically; its
    specific meaning has to be defined for each application. If this
    control function is used in hierarchical order, it may delimit a data
    item called a unit, see"
      :DEFAULT NIL)
     (:REFERENCE "8.3.70" :NAME "IS2"
      :TITLE "INFORMATION SEPARATOR TWO (RS - RECORD SEPARATOR)" 
      :NOTATION (C0)
      :REPRESENTATION (("01/14"))
      :DESCRIPTION
      "IS2 is used to separate and qualify data logically; its
    specific meaning has to be defined for each application. If this
    control function is used in hierarchical order, it may delimit a data
    item called a record, see"
      :DEFAULT NIL)
     (:REFERENCE "8.3.71" :NAME "IS3"
      :TITLE "INFORMATION SEPARATOR THREE (GS - GROUP SEPARATOR)"
      :NOTATION (C0)
      :REPRESENTATION (("01/13"))
      :DESCRIPTION
      "IS3 is used to separate and qualify data logically; its
    specific meaning has to be defined for each application. If this
    control function is used in hierarchical order, it may delimit a data
    item called a group, see"
      :DEFAULT NIL)
     (:REFERENCE "8.3.72" :NAME "IS4"
      :TITLE "INFORMATION SEPARATOR FOUR (FS - FILE SEPARATOR)" :NOTATION (C0)
      :REPRESENTATION (("01/12"))
      :DESCRIPTION
      "IS4 is used to separate and qualify data logically; its
    specific meaning has to be defined for each application. If this
    control function is used in hierarchical order, it may delimit a data
    item called a file, see"
      :DEFAULT NIL)
     (:REFERENCE "8.3.73" :NAME "JFY" :TITLE "JUSTIFY" :NOTATION (PS...)
      :REPRESENTATION (("CSI" "Ps..." "02/00" "04/06"))
      :DESCRIPTION
      "JFY is used to indicate the beginning of a string of graphic
    characters in the presentation component that are to be justified
    according to the layout specified by the parameter values, see annex
    C:"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.74" :NAME "LF" :TITLE "LINE FEED" :NOTATION (C0)
      :REPRESENTATION (("00/10"))
      :DESCRIPTION
      "If the DEVICE COMPONENT SELECT MODE (DCSM) is set to
    PRESENTATION, LF causes the active presentation position to be moved
    to the corresponding character position of the following line in the
    presentation component. If the DEVICE COMPONENT SELECT MODE (DCSM) is
    set to DATA, LF causes the active data position to be moved to the
    corresponding character position of the following line in the data
    component."
      :DEFAULT NIL)
     (:REFERENCE "8.3.75" :NAME "LS0" :TITLE "LOCKING-SHIFT ZERO"
      :NOTATION (C0) :REPRESENTATION (("00/15"))
      :DESCRIPTION
      "LS0 is used for code extension purposes. It causes the
    meanings of the bit combinations following it in the data stream to be
    changed. The use of LS0 is defined in Standard ECMA-35."
      :DEFAULT NIL)
     (:REFERENCE "8.3.76" :NAME "LS1" :TITLE "LOCKING-SHIFT ONE"
      :NOTATION (FS) :REPRESENTATION (("ESC" "07/14"))
      :DESCRIPTION
      "LS1 is used for code extension purposes. It causes the
    meanings of the bit combinations following it in the data stream to be
    changed. The use of LS1 is defined in Standard ECMA-35."
      :DEFAULT NIL)
     (:REFERENCE "8.3.78" :NAME "LS2" :TITLE "LOCKING-SHIFT TWO"
      :NOTATION (FS) :REPRESENTATION (("ESC" "07/13"))
      :DESCRIPTION
      "LS2R is used for code extension purposes. It causes the
    meanings of the bit combinations following it in the data stream to be
    changed. The use of LS2R is defined in Standard ECMA-35."
      :DEFAULT NIL)
     (:REFERENCE "8.3.80" :NAME "LS3" :TITLE "LOCKING-SHIFT THREE"
      :NOTATION (FS) :REPRESENTATION (("ESC" "07/12"))
      :DESCRIPTION
      "LS3R is used for code extension purposes. It causes the
    meanings of the bit combinations following it in the data stream to be
    changed. The use of LS3R is defined in Standard ECMA-35."
      :DEFAULT NIL)
     (:REFERENCE "8.3.82" :NAME "MC" :TITLE "MEDIA COPY" :NOTATION (PS)
      :REPRESENTATION (("CSI" "Ps" "06/09"))
      :DESCRIPTION
      "MC is used either to initiate a transfer of data from or to
    an auxiliary input/output device or to enable or disable the relay of
    the received data stream to an auxiliary input/output device,
    depending on the parameter value:"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.83" :NAME "MW" :TITLE "MESSAGE WAITING" :NOTATION (C1)
      :REPRESENTATION (("09/05") ("ESC" "05/05"))
      :DESCRIPTION
      "MW is used to set a message waiting indicator in the
    receiving device. An appropriate acknowledgement to the receipt of MW
    may be given by using DEVICE STATUS REPORT (DSR)."
      :DEFAULT NIL)
     (:REFERENCE "8.3.84" :NAME "NAK" :TITLE "NEGATIVE ACKNOWLEDGE"
      :NOTATION (C0) :REPRESENTATION (("01/05"))
      :DESCRIPTION
      "NAK is transmitted by a receiver as a negative response to
    the sender. The use of NAK is defined in ISO 1745."
      :DEFAULT NIL)
     (:REFERENCE "8.3.85" :NAME "NBH" :TITLE "NO BREAK HERE" :NOTATION (C1)
      :REPRESENTATION (("08/03") ("ESC" "04/03"))
      :DESCRIPTION
      "NBH is used to indicate a point where a line break shall not
    occur when text is formatted. NBH may occur between two graphic
    characters either or both of which may be SPACE."
      :DEFAULT NIL)
     (:REFERENCE "8.3.86" :NAME "NEL" :TITLE "NEXT LINE" :NOTATION (C1)
      :REPRESENTATION (("08/05") ("ESC" "04/05"))
      :DESCRIPTION
      "The effect of NEL depends on the setting of the DEVICE
    COMPONENT SELECT MODE (DCSM) and on the parameter value of SELECT
    IMPLICIT MOVEMENT DIRECTION (SIMD). If the DEVICE COMPONENT SELECT
    MODE (DCSM) is set to PRESENTATION and with a parameter value of SIMD
    equal to 0, NEL causes the active presentation position to be moved to
    the line home position of the following line in the presentation
    component. The line home position is established by the parameter
    value of SET LINE HOME (SLH). With a parameter value of SIMD equal to
    1, NEL causes the active presentation position to be moved to the line
    limit position of the following line in the presentation
    component. The line limit position is established by the parameter
    value of SET LINE LIMIT (SLL). If the DEVICE COMPONENT SELECT MODE
    (DCSM) is set to DATA and with a parameter value of SIMD equal to 0,
    NEL causes the active data position to be moved to the line home
    position of the following line in the data component. The line home
    position is established by the parameter value of SET LINE HOME
    (SLH). With a parameter value of SIMD equal to 1, NEL causes the
    active data position to be moved to the line limit position of the
    following line in the data component. The line limit position is
    established by the parameter value of SET LINE LIMIT (SLL)."
      :DEFAULT NIL)
     (:REFERENCE "8.3.87" :NAME "NP" :TITLE "NEXT PAGE" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "05/05"))
      :DESCRIPTION
      "NP causes the n-th following page in the presentation
    component to be displayed, where n equals the value of Pn. The effect
    of this control function on the active presentation position is not
    defined by this Standard."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.88" :NAME "NUL" :TITLE "NULL" :NOTATION (C0)
      :REPRESENTATION (("00/00"))
      :DESCRIPTION
      "NUL is used for media-fill or time-fill. NUL characters may
    be inserted into, or removed from, a data stream without affecting the
    information content of that stream, but such action may affect the
    information layout and/or the control of equipment."
      :DEFAULT NIL)
     (:REFERENCE "8.3.89" :NAME "OSC" :TITLE "OPERATING SYSTEM COMMAND"
      :NOTATION (C1) :REPRESENTATION (("09/13") ("ESC" "05/13"))
      :DESCRIPTION
      "OSC is used as the opening delimiter of a control string for
    operating system use. The command string following may consist of a
    sequence of bit combinations in the range 00/08 to 00/13 and 02/00 to
    07/14. The control string is closed by the terminating delimiter
    STRING TERMINATOR (ST). The interpretation of the command string
    depends on the relevant operating system."
      :DEFAULT NIL)
     (:REFERENCE "8.3.90" :NAME "PEC"
      :TITLE "PRESENTATION EXPAND OR CONTRACT" :NOTATION (PS)
      :REPRESENTATION (("CSI" "Ps" "02/00" "05/10"))
      :DESCRIPTION
      "PEC is used to establish the spacing and the extent of the
    graphic characters for subsequent text. The spacing is specified in
    the line as multiples of the spacing established by the most recent
    occurrence of SET CHARACTER SPACING (SCS) or of SELECT CHARACTER
    SPACING (SHS) or of SPACING INCREMENT (SPI) in the data stream. The
    extent of the characters is implicitly established by these control
    functions. The established spacing and the extent remain in effect
    until the next occurrence of PEC, of SCS, of SHS or of SPI in the data
    stream. The parameter values are"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.91" :NAME "PFS" :TITLE "PAGE FORMAT SELECTION"
      :NOTATION (PS) :REPRESENTATION (("CSI" "Ps" "02/00" "04/10"))
      :DESCRIPTION
      "PFS is used to establish the available area for the imaging
    of pages of text based on paper size. The pages are introduced by the
    subsequent occurrence of FORM FEED (FF) in the data stream. The
    established image area remains in effect until the next occurrence of
    PFS in the data stream. The parameter values are (see also annex E):"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.92" :NAME "PLD" :TITLE "PARTIAL LINE FORWARD"
      :NOTATION (C1) :REPRESENTATION (("08/11") ("ESC" "04/11"))
      :DESCRIPTION
      "PLD causes the active presentation position to be moved in
    the presentation component to the corresponding position of an
    imaginary line with a partial offset in the direction of the line
    progression. This offset should be sufficient either to image
    following characters as subscripts until the first following
    occurrence of PARTIAL LINE BACKWARD (PLU) in the data stream, or, if
    preceding characters were imaged as superscripts, to restore imaging
    of following characters to the active line (the line that contains the
    active presentation position). Any interactions between PLD and format
    effectors other than PLU are not defined by this Standard."
      :DEFAULT NIL)
     (:REFERENCE "8.3.93" :NAME "PLU" :TITLE "PARTIAL LINE BACKWARD"
      :NOTATION (C1) :REPRESENTATION (("08/12") ("ESC" "04/12"))
      :DESCRIPTION
      "PLU causes the active presentation position to be moved in
    the presentation component to the corresponding position of an
    imaginary line with a partial offset in the direction opposite to that
    of the line progression. This offset should be sufficient either to
    image following characters as superscripts until the first following
    occurrence of PARTIAL LINE FORWARD (PLD) in the data stream, or, if
    preceding characters were imaged as subscripts, to restore imaging of
    following characters to the active line (the line that contains the
    active presentation position). Any interactions between PLU and format
    effectors other than PLD are not defined by this Standard."
      :DEFAULT NIL)
     (:REFERENCE "8.3.94" :NAME "PM" :TITLE "PRIVACY MESSAGE" :NOTATION (C1)
      :REPRESENTATION (("09/14") ("ESC" "05/14"))
      :DESCRIPTION
      "PM is used as the opening delimiter of a control string for
    privacy message use. The command string following may consist of a
    sequence of bit combinations in the range 00/08 to 00/13 and 02/00 to
    07/14. The control string is closed by the terminating delimiter
    STRING TERMINATOR (ST). The interpretation of the command string
    depends on the relevant privacy discipline."
      :DEFAULT NIL)
     (:REFERENCE "8.3.95" :NAME "PP" :TITLE "PRECEDING PAGE" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "05/06"))
      :DESCRIPTION
      "PP causes the n-th preceding page in the presentation
    component to be displayed, where n equals the value of Pn. The effect
    of this control function on the active presentation position is not
    defined by this Standard."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.96" :NAME "PPA" :TITLE "PAGE POSITION ABSOLUTE"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "02/00" "05/00"))
      :DESCRIPTION
      "PPA causes the active data position to be moved in the data
    component to the corresponding character position on the n-th page,
    where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.97" :NAME "PPB" :TITLE "PAGE POSITION BACKWARD"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "02/00" "05/02"))
      :DESCRIPTION
      "PPB causes the active data position to be moved in the data
    component to the corresponding character position on the n-th
    preceding page, where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.98" :NAME "PPR" :TITLE "PAGE POSITION FORWARD"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "02/00" "05/01"))
      :DESCRIPTION
      "PPR causes the active data position to be moved in the data
    component to the corresponding character position on the n-th
    following page, where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.99" :NAME "PTX" :TITLE "PARALLEL TEXTS" :NOTATION (PS)
      :REPRESENTATION (("CSI" "Ps" "05/12"))
      :DESCRIPTION
      "PTX is used to delimit strings of graphic characters that are
    communicated one after another in the data stream but that are
    intended to be presented in parallel with one another, usually in
    adjacent lines. The parameter values are"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.100" :NAME "PU1" :TITLE "PRIVATE USE ONE"
      :NOTATION (C1) :REPRESENTATION (("09/01") ("ESC" "05/01"))
      :DESCRIPTION
      "PU1 is reserved for a function without standardized meaning
    for private use as required, subject to the prior agreement between
    the sender and the recipient of the data."
      :DEFAULT NIL)
     (:REFERENCE "8.3.101" :NAME "PU2" :TITLE "PRIVATE USE TWO"
      :NOTATION (C1) :REPRESENTATION (("09/02") ("ESC" "05/02"))
      :DESCRIPTION
      "PU2 is reserved for a function without standardized meaning
    for private use as required, subject to the prior agreement between
    the sender and the recipient of the data."
      :DEFAULT NIL)
     (:REFERENCE "8.3.102" :NAME "QUAD" :TITLE "QUAD" :NOTATION (PS...)
      :REPRESENTATION (("CSI" "Ps..." "02/00" "04/08"))
      :DESCRIPTION
      "QUAD is used to indicate the end of a string of graphic
    characters that are to be positioned on a single line according to the
    layout specified by the parameter values, see annex C:"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.103" :NAME "REP" :TITLE "REPEAT" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "06/02"))
      :DESCRIPTION
      "REP is used to indicate that the preceding character in the
    data stream, if it is a graphic character (represented by one or more
    bit combinations) including SPACE, is to be repeated n times, where n
    equals the value of Pn. If the character preceding REP is a control
    function or part of a control function, the effect of REP is not
    defined by this Standard."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.104" :NAME "RI" :TITLE "REVERSE LINE FEED"
      :NOTATION (C1) :REPRESENTATION (("08/13") ("ESC" "04/13"))
      :DESCRIPTION
      "If the DEVICE COMPONENT SELECT MODE (DCSM) is set to
    PRESENTATION, RI causes the active presentation position to be moved
    in the presentation component to the corresponding character position
    of the preceding line. If the DEVICE COMPONENT SELECT MODE (DCSM) is
    set to DATA, RI causes the active data position to be moved in the
    data component to the corresponding character position of the
    preceding line."
      :DEFAULT NIL)
     (:REFERENCE "8.3.105" :NAME "RIS" :TITLE "RESET TO INITIAL STATE"
      :NOTATION (FS) :REPRESENTATION (("ESC" "06/03"))
      :DESCRIPTION
      "RIS causes a device to be reset to its initial state,
    i.e. the state it has after it is made operational. This may imply, if
    applicable: clear tabulation stops, remove qualified areas, reset
    graphic rendition, put all character positions into the erased state,
    move the active presentation position to the first position of the
    first line in the presentation component, move the active data
    position to the first character position of the first line in the data
    component, set the modes into the reset state, etc."
      :DEFAULT NIL)
     (:REFERENCE "8.3.106" :NAME "RM" :TITLE "RESET MODE" :NOTATION (PS...)
      :REPRESENTATION (("CSI" "Ps..." "06/12"))
      :DESCRIPTION
      "RM causes the modes of the receiving device to be reset as
    specified by the parameter values:"
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.107" :NAME "SACS"
      :TITLE "SET ADDITIONAL CHARACTER SEPARATION" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "02/00" "05/12"))
      :DESCRIPTION
      "SACS is used to establish extra inter-character escapement
    for subsequent text. The established extra escapement remains in
    effect until the next occurrence of SACS or of SET REDUCED CHARACTER
    SEPARATION (SRCS) in the data stream or until it is reset to the
    default value by a subsequent occurrence of CARRIAGE RETURN/LINE FEED
    (CR LF) or of NEXT LINE (NEL) in the data stream, see annex C. Pn
    specifies the number of units by which the inter-character escapement
    is enlarged. The unit in which the parameter value is expressed is
    that established by the parameter value of SELECT SIZE UNIT (SSU)."
      :DEFAULT ((PN . 0)))
     (:REFERENCE "8.3.108" :NAME "SAPV"
      :TITLE "SELECT ALTERNATIVE PRESENTATION VARIANTS" :NOTATION (PS...)
      :REPRESENTATION (("CSI" "Ps..." "02/00" "05/13"))
      :DESCRIPTION
      "SAPV is used to specify one or more variants for the
    presentation of subsequent text. The parameter values are"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.109" :NAME "SCI" :TITLE "SINGLE CHARACTER INTRODUCER"
      :NOTATION (C1) :REPRESENTATION (("09/10") ("ESC" "05/10"))
      :DESCRIPTION
      "SCI and the bit combination following it are used to
    represent a control function or a graphic character. The bit
    combination following SCI must be from 00/08 to 00/13 or 02/00 to
    07/14. The use of SCI is reserved for future standardization."
      :DEFAULT NIL)
     (:REFERENCE "8.3.110" :NAME "SCO" :TITLE "SELECT CHARACTER ORIENTATION"
      :NOTATION (PS) :REPRESENTATION (("CSI" "Ps" "02/00" "06/05"))
      :DESCRIPTION
      "SCO is used to establish the amount of rotation of the
    graphic characters following in the data stream. The established value
    remains in effect until the next occurrence of SCO in the data
    stream. The parameter values are 0 0° 1 45° 2 90° 3 135° 4 180° 5 225°
    6 270° 7 315° Rotation is positive, i.e. counter-clockwise and applies
    to the normal presentation of the graphic characters along the
    character path. The centre of rotation of the affected graphic
    characters is not defined by this Standard."
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.111" :NAME "SCP" :TITLE "SELECT CHARACTER PATH"
      :NOTATION (PS1 PS2) :REPRESENTATION (("CSI" "Ps1;Ps2" "02/00" "06/11"))
      :DESCRIPTION
      "SCP is used to select the character path, relative to the
    line orientation, for the active line (the line that contains the
    active presentation position) and subsequent lines in the presentation
    component. It is also used to update the content of the active line in
    the presentation component and the content of the active line (the
    line that contains the active data position) in the data
    component. This takes effect immediately. Ps1 specifies the character
    path: 1 left-to-right (in the case of horizontal line orientation), or
    top-to-bottom (in the case of vertical line orientation) 2
    right-to-left (in the case of horizontal line orientation), or
    bottom-to-top (in the case of vertical line orientation) Ps2 specifies
    the effect on the content of the presentation component and the
    content of the data component: 0 undefined (implementation-dependent)"
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.112" :NAME "SCS" :TITLE "SET CHARACTER SPACING"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "02/00" "06/07"))
      :DESCRIPTION
      "SCS is used to establish the character spacing for subsequent
    text. The established spacing remains in effect until the next
    occurrence of SCS, or of SELECT CHARACTER SPACING (SHS) or of SPACING
    INCREMENT (SPI) in the data stream, see annex C. Pn specifies the
    character spacing. The unit in which the parameter value is expressed
    is that established by the parameter value of SELECT SIZE UNIT (SSU)."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.113" :NAME "SD" :TITLE "SCROLL DOWN" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "05/04"))
      :DESCRIPTION
      "SD causes the data in the presentation component to be moved
    by n line positions if the line orientation is horizontal, or by n
    character positions if the line orientation is vertical, such that the
    data appear to move down; where n equals the value of Pn. The active
    presentation position is not affected by this control function."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.114" :NAME "SDS" :TITLE "START DIRECTED STRING"
      :NOTATION (PS) :REPRESENTATION (("CSI" "Ps" "05/13"))
      :DESCRIPTION
      "SDS is used to establish in the data component the beginning
    and the end of a string of characters as well as the direction of the
    string. This direction may be different from that currently
    established. The indicated string follows the preceding text. The
    established character progression is not affected. The beginning of a
    directed string is indicated by SDS with a parameter value not equal
    to 0. A directed string may contain one or more nested strings. These
    nested strings may be directed strings the beginnings of which are
    indicated by SDS with a parameter value not equal to 0, or reversed
    strings the beginnings of which are indicated by START REVERSED STRING
    (SRS) with a parameter value of 1. Every beginning of such a string
    invokes the next deeper level of nesting. This Standard does not
    define the location of the active data position within any such nested
    string. The end of a directed string is indicated by SDS with a
    parameter value of 0. Every end of such a string re-establishes the
    next higher level of nesting (the one in effect prior to the string
    just ended). The direction is re-established to that in effect prior
    to the string just ended. The active data position is moved to the
    character position following the characters of the string just
    ended. The parameter values are: 0 end of a directed string;
    re-establish the previous direction 1 start of a directed string;
    establish the direction left-to-right 2 start of a directed string;
    establish the direction right-to-left"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.115" :NAME "SEE" :TITLE "SELECT EDITING EXTENT"
      :NOTATION (PS) :REPRESENTATION (("CSI" "Ps" "05/01"))
      :DESCRIPTION
      "SEE is used to establish the editing extent for subsequent
    character or line insertion or deletion. The established extent
    remains in effect until the next occurrence of SEE in the data
    stream. The editing extent depends on the parameter value: 0 the
    shifted part is limited to the active page in the presentation
    component 1 the shifted part is limited to the active line in the
    presentation component 2 the shifted part is limited to the active
    field in the presentation component 3 the shifted part is limited to
    the active qualified area 4 the shifted part consists of the relevant
    part of the entire presentation component."
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.116" :NAME "SEF" :TITLE "SHEET EJECT AND FEED"
      :NOTATION (PS1 PS2) :REPRESENTATION (("CSI" "Ps1;Ps2" "02/00" "05/09"))
      :DESCRIPTION
      "SEF causes a sheet of paper to be ejected from a printing
    device into a specified output stacker and another sheet to be loaded
    into the printing device from a specified paper bin. Parameter values
    of Ps1 are: 0 eject sheet, no new sheet loaded 1 eject sheet and load
    another from bin 1 2 eject sheet and load another from bin 2 . . . n
    eject sheet and load another from bin n Parameter values of Ps2 are: 0
    eject sheet, no stacker specified 1 eject sheet into stacker 1 2 eject
    sheet into stacker 2 . . . n eject sheet into stacker n"
      :DEFAULT ((PS1 . 0) (PS2 . 0)))
     (:REFERENCE "8.3.117" :NAME "SGR" :TITLE "SELECT GRAPHIC RENDITION"
      :NOTATION (PS...) :REPRESENTATION (("CSI" "Ps..." "06/13"))
      :DESCRIPTION
      "SGR is used to establish one or more graphic rendition
    aspects for subsequent text. The established aspects remain in effect
    until the next occurrence of SGR in the data stream, depending on the
    setting of the GRAPHIC RENDITION COMBINATION MODE (GRCM). Each graphic
    rendition aspect is specified by a parameter value: 0 default
    rendition (implementation-defined), cancels the effect of any
    preceding occurrence of SGR in the data stream regardless of the
    setting of the GRAPHIC RENDITION COMBINATION MODE (GRCM) 1 bold or
    increased intensity 2 faint, decreased intensity or second colour 3
    italicized 4 singly underlined 5 slowly blinking (less then 150 per
    minute) 6 rapidly blinking (150 per minute or more) 7 negative image 8
    concealed characters 9 crossed-out (characters still legible but
    marked as to be deleted) 10 primary (default) font 11 first
    alternative font 12 second alternative font 13 third alternative font
    14 fourth alternative font 15 fifth alternative font 16 sixth
    alternative font 17 seventh alternative font 18 eighth alternative
    font 19 ninth alternative font 20 Fraktur (Gothic) 21 doubly
    underlined 22 normal colour or normal intensity (neither bold nor
    faint) 23 not italicized, not fraktur 24 not underlined (neither
    singly nor doubly) 25 steady (not blinking) 26 (reserved for
    proportional spacing as specified in CCITT Recommendation T.61) 27
    positive image 28 revealed characters 29 not crossed out 30 black
    display 31 red display 32 green display 33 yellow display 34 blue
    display 35 magenta display 36 cyan display 37 white display 38
    (reserved for future standardization; intended for setting character
    foreground colour as specified in ISO 8613-6 [CCITT Recommendation
    T.416]) 39 default display colour (implementation-defined) 40 black
    background 41 red background 42 green background 43 yellow background
    44 blue background 45 magenta background 46 cyan background 47 white
    background 48 (reserved for future standardization; intended for
    setting character background colour as specified in ISO 8613-6 [CCITT
    Recommendation T.416]) 49 default background colour
    (implementation-defined) 50 (reserved for cancelling the effect of the
    rendering aspect established by parameter value 26) 51 framed 52
    encircled 53 overlined 54 not framed, not encircled 55 not overlined
    56 (reserved for future standardization) 57 (reserved for future
    standardization) 58 (reserved for future standardization) 59 (reserved
    for future standardization) 60 ideogram underline or right side line
    61 ideogram double underline or double line on the right side 62
    ideogram overline or left side line 63 ideogram double overline or
    double line on the left side 64 ideogram stress marking 65 cancels the
    effect of the rendition aspects established by parameter values 60 to
    64"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.118" :NAME "SHS" :TITLE "SELECT CHARACTER SPACING"
      :NOTATION (PS) :REPRESENTATION (("CSI" "Ps" "02/00" "04/11"))
      :DESCRIPTION
      "SHS is used to establish the character spacing for subsequent
    text. The established spacing remains in effect until the next
    occurrence of SHS or of SET CHARACTER SPACING (SCS) or of SPACING
    INCREMENT (SPI) in the data stream. The parameter values are 0 10
    characters per 25,4 mm 1 12 characters per 25,4 mm 2 15 characters per
    25,4 mm 3 6 characters per 25,4 mm 4 3 characters per 25,4 mm 5 9
    characters per 50,8 mm 6 4 characters per 25,4 mm"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.119" :NAME "SI" :TITLE "SHIFT-IN" :NOTATION (C0)
      :REPRESENTATION (("00/15"))
      :DESCRIPTION
      "SI is used for code extension purposes. It causes the
    meanings of the bit combinations following it in the data stream to be
    changed. The use of SI is defined in Standard ECMA-35."
      :DEFAULT NIL)
     (:REFERENCE "8.3.120" :NAME "SIMD"
      :TITLE "SELECT IMPLICIT MOVEMENT DIRECTION" :NOTATION (PS)
      :REPRESENTATION (("CSI" "Ps" "05/14"))
      :DESCRIPTION
      "SIMD is used to select the direction of implicit movement of
    the data position relative to the character progression. The direction
    selected remains in effect until the next occurrence of SIMD. The
    parameter values are: 0 the direction of implicit movement is the same
    as that of the character progression 1 the direction of implicit
    movement is opposite to that of the character progression."
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.121" :NAME "SL" :TITLE "SCROLL LEFT" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "02/00" "04/00"))
      :DESCRIPTION
      "SL causes the data in the presentation component to be moved
    by n character positions if the line orientation is horizontal, or by
    n line positions if the line orientation is vertical, such that the
    data appear to move to the left; where n equals the value of Pn. The
    active presentation position is not affected by this control
    function."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.122" :NAME "SLH" :TITLE "SET LINE HOME" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "02/00" "05/05"))
      :DESCRIPTION
      "SLH is used to establish at character position n in the
    active line (the line that contains the active presentation position)
    and lines of subsequent text in the presentation component the
    position to which the active presentation position will be moved by
    subsequent occurrences of CARRIAGE RETURN (CR), DELETE LINE (DL),
    INSERT LINE (IL) or NEXT LINE (NEL) in the data stream; where n equals
    the value of Pn. In the case of a device without data component, it is
    also the position ahead of which no implicit movement of the active
    presentation position shall occur. If the DEVICE COMPONENT SELECT MODE
    is set to DATA, SLH is used to establish at character position n in
    the active line (the line that contains the active data position) and
    lines of subsequent text in the data component the position to which
    the active data position will be moved by subsequent occurrences of
    CARRIAGE RETURN (CR), DELETE LINE (DL), INSERT LINE (IL) or NEXT LINE
    (NEL) in the data stream; where n equals the value of Pn. It is also
    the position ahead of which no implicit movement of the active data
    position shall occur. The established position is called the line home
    position and remains in effect until the next occurrence of SLH in the
    data stream."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.123" :NAME "SLL" :TITLE "SET LINE LIMIT" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "02/00" "05/06"))
      :DESCRIPTION
      "SLL is used to establish at character position n in the
    active line (the line that contains the active presentation position)
    and lines of subsequent text in the presentation component the
    position to which the active presentation position will be moved by
    subsequent occurrences of CARRIAGE RETURN (CR), or NEXT LINE (NEL) in
    the data stream if the parameter value of SELECT IMPLICIT MOVEMENT
    DIRECTION (SIMD) is equal to 1; where n equals the value of Pn. In the
    case of a device without data component, it is also the position
    beyond which no implicit movement of the active presentation position
    shall occur. If the DEVICE COMPONENT SELECT MODE is set to DATA, SLL
    is used to establish at character position n in the active line (the
    line that contains the active data position) and lines of subsequent
    text in the data component the position beyond which no implicit
    movement of the active data position shall occur. It is also the
    position in the data component to which the active data position will
    be moved by subsequent occurrences of CR or NEL in the data stream, if
    the parameter value of SELECT IMPLICIT MOVEMENT DIRECTION (SIMD) is
    equal to 1. The established position is called the line limit position
    and remains in effect until the next occurrence of SLL in the data
    stream."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.124" :NAME "SLS" :TITLE "SET LINE SPACING"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "02/00" "06/08"))
      :DESCRIPTION
      "SLS is used to establish the line spacing for subsequent
    text. The established spacing remains in effect until the next
    occurrence of SLS or of SELECT LINE SPACING (SVS) or of SPACING
    INCREMENT (SPI) in the data stream. Pn specifies the line spacing. The
    unit in which the parameter value is expressed is that established by
    the parameter value of SELECT SIZE UNIT (SSU)."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.125" :NAME "SM" :TITLE "SET MODE" :NOTATION (PS...)
      :REPRESENTATION (("CSI" "Ps..." "06/08"))
      :DESCRIPTION
      "SM causes the modes of the receiving device to be set as
    specified by the parameter values: 1 GUARDED AREA TRANSFER MODE (GATM)
    2 KEYBOARD ACTION MODE (KAM) 3 CONTROL REPRESENTATION MODE (CRM) 4
    INSERTION REPLACEMENT MODE (IRM) 5 STATUS REPORT TRANSFER MODE (SRTM)
    6 ERASURE MODE (ERM) 7 LINE EDITING MODE (VEM) 8 BI-DIRECTIONAL
    SUPPORT MODE (BDSM) 9 DEVICE COMPONENT SELECT MODE (DCSM) 10 CHARACTER
    EDITING MODE (HEM) 11 POSITIONING UNIT MODE (PUM) (see F.4.1 in annex"
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.126" :NAME "SO" :TITLE "SHIFT-OUT" :NOTATION (C0)
      :REPRESENTATION (("00/14"))
      :DESCRIPTION
      "SO is used for code extension purposes. It causes the
    meanings of the bit combinations following it in the data stream to be
    changed. The use of SO is defined in Standard ECMA-35."
      :DEFAULT NIL)
     (:REFERENCE "8.3.127" :NAME "SOH" :TITLE "START OF HEADING"
      :NOTATION (C0) :REPRESENTATION (("00/01"))
      :DESCRIPTION
      "SOH is used to indicate the beginning of a heading. The use
    of SOH is defined in ISO 1745."
      :DEFAULT NIL)
     (:REFERENCE "8.3.128" :NAME "SOS" :TITLE "START OF STRING"
      :NOTATION (C1) :REPRESENTATION (("09/08") ("ESC" "05/08"))
      :DESCRIPTION
      "SOS is used as the opening delimiter of a control string. The
    character string following may consist of any bit combination, except
    those representing SOS or STRING TERMINATOR (ST). The control string
    is closed by the terminating delimiter STRING TERMINATOR (ST). The
    interpretation of the character string depends on the application."
      :DEFAULT NIL)
     (:REFERENCE "8.3.129" :NAME "SPA" :TITLE "START OF GUARDED AREA"
      :NOTATION (C1) :REPRESENTATION (("09/06") ("ESC" "05/06"))
      :DESCRIPTION
      "SPA is used to indicate that the active presentation position is
    the first of a string of character positions in the presentation
    component, the contents of which are protected against manual
    alteration, are guarded against transmission or transfer, depending on
    the setting of the GUARDED AREA TRANSFER MODE (GATM) and may be
    protected against erasure, depending on the setting of the ERASURE
    MODE (ERM). The end of this string is indicated by END OF GUARDED AREA
    (EPA)."
      :DEFAULT NIL)
     (:REFERENCE "8.3.130" :NAME "SPD"
      :TITLE "SELECT PRESENTATION DIRECTIONS" :NOTATION (PS1 PS2)
      :REPRESENTATION (("CSI" "Ps1;Ps2" "02/00" "05/03"))
      :DESCRIPTION
      "SPD is used to select the line orientation, the line progression,
    and the character path in the presentation component. It is also used
    to update the content of the presentation component and the content of
    the data component. This takes effect immediately. Ps1 specifies the
    line orientation, the line progression and the character path: 0 line
    orientation: horizontal line progression: top-to-bottom character
    path: left-to-right 1 line orientation: vertical line progression:
    right-to-left character path: top-to-bottom 2 line orientation:
    vertical line progression: left-to-right character path: top-to-bottom
    3 line orientation: horizontal line progression: top-to-bottom
    character path: right-to-left 4 line orientation: vertical line
    progression: left-to-right character path: bottom-to-top 5 line
    orientation: horizontal line progression: bottom-to-top character
    path: right-to-left 6 line orientation: horizontal line progression:
    bottom-to-top character path: left-to-right 7 line orientation:
    vertical line progression: right-to-left character path: bottom-to-top
    Ps2 specifies the effect on the content of the presentation component
    and the content of the data component: 0 undefined
    (implementation-dependent)"
      :DEFAULT ((PS1 . 0) (PS2 . 0)))
     (:REFERENCE "8.3.131" :NAME "SPH" :TITLE "SET PAGE HOME" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "02/00" "06/09"))
      :DESCRIPTION
      "SPH is used to establish at line position n in the active page
    (the page that contains the active presentation position) and
    subsequent pages in the presentation component the position to which
    the active presentation position will be moved by subsequent
    occurrences of FORM FEED (FF) in the data stream; where n equals the
    value of Pn. In the case of a device without data component, it is
    also the position ahead of which no implicit movement of the active
    presentation position shall occur. If the DEVICE COMPONENT SELECT MODE
    is set to DATA, SPH is used to establish at line position n in the
    active page (the page that contains the active data position) and
    subsequent pages in the data component the position to which the
    active data position will be moved by subsequent occurrences of FORM
    FEED (FF) in the data stream; where n equals the value of Pn. It is
    also the position ahead of which no implicit movement of the active
    presentation position shall occur. The established position is called
    the page home position and remains in effect until the next occurrence
    of SPH in the data stream."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.132" :NAME "SPI" :TITLE "SPACING INCREMENT"
      :NOTATION (PN1 PN2) :REPRESENTATION (("CSI" "Pn1;Pn2" "02/00" "04/07"))
      :DESCRIPTION
      "SPI is used to establish the line spacing and the character
    spacing for subsequent text. The established line spacing remains in
    effect until the next occurrence of SPI or of SET LINE SPACING (SLS)
    or of SELECT LINE SPACING (SVS) in the data stream. The established
    character spacing remains in effect until the next occurrence of SET
    CHARACTER SPACING (SCS) or of SELECT CHARACTER SPACING (SHS) in the
    data stream, see annex C. Pn1 specifies the line spacing Pn2 specifies
    the character spacing The unit in which the parameter values are
    expressed is that established by the parameter value of SELECT SIZE
    UNIT (SSU)."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.133" :NAME "SPL" :TITLE "SET PAGE LIMIT" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "02/00" "06/10"))
      :DESCRIPTION
      "SPL is used to establish at line position n in the active page
    (the page that contains the active presentation position) and pages of
    subsequent text in the presentation component the position beyond
    which the active presentation position can normally not be moved;
    where n equals the value of Pn. In the case of a device without data
    component, it is also the position beyond which no implicit movement
    of the active presentation position shall occur. If the DEVICE
    COMPONENT SELECT MODE is set to DATA, SPL is used to establish at line
    position n in the active page (the page that contains the active data
    position) and pages of subsequent text in the data component the
    position beyond which no implicit movement of the active data position
    shall occur. The established position is called the page limit
    position and remains in effect until the next occurrence of SPL in the
    data stream."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.134" :NAME "SPQR"
      :TITLE "SELECT PRINT QUALITY AND RAPIDITY" :NOTATION (PS)
      :REPRESENTATION (("CSI" "Ps" "02/00" "05/08"))
      :DESCRIPTION
      "SPQR is used to select the relative print quality and the print
    speed for devices the output quality and speed of which are inversely
    related. The selected values remain in effect until the next
    occurrence of SPQR in the data stream. The parameter values are 0
    highest available print quality, low print speed 1 medium print
    quality, medium print speed 2 draft print quality, highest available
    print speed"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.135" :NAME "SR" :TITLE "SCROLL RIGHT" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "02/00" "04/01"))
      :DESCRIPTION
      "SR causes the data in the presentation component to be moved by n
    character positions if the line orientation is horizontal, or by n
    line positions if the line orientation is vertical, such that the data
    appear to move to the right; where n equals the value of Pn. The
    active presentation position is not affected by this control
    function."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.136" :NAME "SRCS"
      :TITLE "SET REDUCED CHARACTER SEPARATION" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "02/00" "06/06"))
      :DESCRIPTION
      "SRCS is used to establish reduced inter-character escapement for
    subsequent text. The established reduced escapement remains in effect
    until the next occurrence of SRCS or of SET ADDITIONAL CHARACTER
    SEPARATION (SACS) in the data stream or until it is reset to the
    default value by a subsequent occurrence of CARRIAGE RETURN/LINE FEED
    (CR/LF) or of NEXT LINE (NEL) in the data stream, see annex C. Pn
    specifies the number of units by which the inter-character escapement
    is reduced. The unit in which the parameter values are expressed is
    that established by the parameter value of SELECT SIZE UNIT (SSU)."
      :DEFAULT ((PN . 0)))
     (:REFERENCE "8.3.137" :NAME "SRS" :TITLE "START REVERSED STRING"
      :NOTATION (PS) :REPRESENTATION (("CSI" "Ps" "05/11"))
      :DESCRIPTION
      "SRS is used to establish in the data component the beginning and
    the end of a string of characters as well as the direction of the
    string. This direction is opposite to that currently established. The
    indicated string follows the preceding text. The established character
    progression is not affected. The beginning of a reversed string is
    indicated by SRS with a parameter value of 1. A reversed string may
    contain one or more nested strings. These nested strings may be
    reversed strings the beginnings of which are indicated by SRS with a
    parameter value of 1, or directed strings the beginnings of which are
    indicated by START DIRECTED STRING (SDS) with a parameter value not
    equal to 0. Every beginning of such a string invokes the next deeper
    level of nesting. This Standard does not define the location of the
    active data position within any such nested string. The end of a
    reversed string is indicated by SRS with a parameter value of 0. Every
    end of such a string re-establishes the next higher level of nesting
    (the one in effect prior to the string just ended). The direction is
    re-established to that in effect prior to the string just ended. The
    active data position is moved to the character position following the
    characters of the string just ended. The parameter values are: 0 end
    of a reversed string; re-establish the previous direction 1 beginning
    of a reversed string; reverse the direction."
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.138" :NAME "SSA" :TITLE "START OF SELECTED AREA"
      :NOTATION (C1) :REPRESENTATION (("08/06") ("ESC" "04/06"))
      :DESCRIPTION
      "SSA is used to indicate that the active presentation position is
    the first of a string of character positions in the presentation
    component, the contents of which are eligible to be transmitted in the
    form of a data stream or transferred to an auxiliary input/output
    device. The end of this string is indicated by END OF SELECTED AREA
    (ESA). The string of characters actually transmitted or transferred
    depends on the setting of the GUARDED AREA TRANSFER MODE (GATM) and on
    any guarded areas established by DEFINE AREA QUALIFICATION (DAQ), or
    by START OF GUARDED AREA (SPA) and END OF GUARDED AREA (EPA)."
      :DEFAULT NIL)
     (:REFERENCE "8.3.139" :NAME "SSU" :TITLE "SELECT SIZE UNIT"
      :NOTATION (PS) :REPRESENTATION (("CSI" "Ps" "02/00" "04/09"))
      :DESCRIPTION
      "SSU is used to establish the unit in which the numeric parameters
    of certain control functions are expressed. The established unit
    remains in effect until the next occurrence of SSU in the data
    stream. The parameter values are 0 CHARACTER - The dimensions of this
    unit are device-dependent 1 MILLIMETRE 2 COMPUTER DECIPOINT - 0,035 28
    mm (1/720 of 25,4 mm) 3 DECIDIDOT - 0,037 59 mm (10/266 mm) 4 MIL -
    0,025 4 mm (1/1 000 of 25,4 mm) 5 BASIC MEASURING UNIT (BMU) - 0,021
    17 mm (1/1 200 of 25,4 mm) 6 MICROMETRE - 0,001 mm 7 PIXEL - The
    smallest increment that can be specified in a device 8 DECIPOINT -
    0,035 14 mm (35/996 mm)"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.140" :NAME "SSW" :TITLE "SET SPACE WIDTH"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "02/00" "05/11"))
      :DESCRIPTION
      "SSW is used to establish for subsequent text the character
    escapement associated with the character SPACE. The established
    escapement remains in effect until the next occurrence of SSW in the
    data stream or until it is reset to the default value by a subsequent
    occurrence of CARRIAGE RETURN/LINE FEED (CR/LF), CARRIAGE RETURN/FORM
    FEED (CR/FF), or of NEXT LINE (NEL) in the data stream, see annex
    C. Pn specifies the escapement. The unit in which the parameter value
    is expressed is that established by the parameter value of SELECT SIZE
    UNIT (SSU). The default character escapement of SPACE is specified by
    the most recent occurrence of SET CHARACTER SPACING (SCS) or of SELECT
    CHARACTER SPACING (SHS) or of SELECT SPACING INCREMENT (SPI) in the
    data stream if the current font has constant spacing, or is specified
    by the nominal width of the character SPACE in the current font if
    that font has proportional spacing."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.141" :NAME "SS2" :TITLE "SINGLE-SHIFT TWO"
      :NOTATION (C1) :REPRESENTATION (("08/14") ("ESC" "04/14"))
      :DESCRIPTION
      "SS2 is used for code extension purposes. It causes the meanings of
    the bit combinations following it in the data stream to be
    changed. The use of SS2 is defined in Standard ECMA-35."
      :DEFAULT NIL)
     (:REFERENCE "8.3.142" :NAME "SS3" :TITLE "SINGLE-SHIFT THREE"
      :NOTATION (C1) :REPRESENTATION (("08/15") ("ESC" "04/15"))
      :DESCRIPTION
      "SS3 is used for code extension purposes. It causes the meanings of
    the bit combinations following it in the data stream to be
    changed. The use of SS3 is defined in Standard ECMA-35."
      :DEFAULT NIL)
     (:REFERENCE "8.3.143" :NAME "ST" :TITLE "STRING TERMINATOR"
      :NOTATION (C1) :REPRESENTATION (("09/12") ("ESC" "05/12"))
      :DESCRIPTION
      "ST is used as the closing delimiter of a control string opened by
    APPLICATION PROGRAM COMMAND (APC), DEVICE CONTROL STRING (DCS),
    OPERATING SYSTEM COMMAND (OSC), PRIVACY MESSAGE (PM), or START OF
    STRING (SOS)."
      :DEFAULT NIL)
     (:REFERENCE "8.3.144" :NAME "STAB" :TITLE "SELECTIVE TABULATION"
      :NOTATION (PS) :REPRESENTATION (("CSI" "Ps" "02/00" "05/14"))
      :DESCRIPTION
      "STAB causes subsequent text in the presentation component to be
    aligned according to the position and the properties of a tabulation
    stop which is selected from a list according to the value of the
    parameter Ps. The use of this control function and means of specifying
    a list of tabulation stops to be referenced by the control function
    are specified in other standards, for example ISO 8613-6."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.145" :NAME "STS" :TITLE "SET TRANSMIT STATE"
      :NOTATION (C1) :REPRESENTATION (("09/03") ("ESC" "05/03"))
      :DESCRIPTION
      "STS is used to establish the transmit state in the receiving
    device. In this state the transmission of data from the device is
    possible. The actual initiation of transmission of data is performed
    by a data communication or input/output interface control procedure
    which is outside the scope of this Standard. The transmit state is
    established either by STS appearing in the received data stream or by
    the operation of an appropriate key on a keyboard."
      :DEFAULT NIL)
     (:REFERENCE "8.3.146" :NAME "STX" :TITLE "START OF TEXT" :NOTATION (C0)
      :REPRESENTATION (("00/02"))
      :DESCRIPTION
      "STX is used to indicate the beginning of a text and the end of a
    heading. The use of STX is defined in ISO 1745."
      :DEFAULT NIL)
     (:REFERENCE "8.3.147" :NAME "SU" :TITLE "SCROLL UP" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "05/03"))
      :DESCRIPTION
      "SU causes the data in the presentation component to be moved by n
    line positions if the line orientation is horizontal, or by n
    character positions if the line orientation is vertical, such that the
    data appear to move up; where n equals the value of Pn. The active
    presentation position is not affected by this control function."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.148" :NAME "SUB" :TITLE "SUBSTITUTE" :NOTATION (C0)
      :REPRESENTATION (("01/10"))
      :DESCRIPTION
      "SUB is used in the place of a character that has been found to be
    invalid or in error. SUB is intended to be introduced by automatic
    means."
      :DEFAULT NIL)
     (:REFERENCE "8.3.149" :NAME "SVS" :TITLE "SELECT LINE SPACING"
      :NOTATION (PS) :REPRESENTATION (("CSI" "Ps" "02/00" "04/12"))
      :DESCRIPTION
      "SVS is used to establish the line spacing for subsequent text. The
    established spacing remains in effect until the next occurrence of SVS
    or of SET LINE SPACING (SLS) or of SPACING INCREMENT (SPI) in the data
    stream. The parameter values are: 0 6 lines per 25,4 mm 1 4 lines per
    25,4 mm 2 3 lines per 25,4 mm 3 12 lines per 25,4 mm 4 8 lines per
    25,4 mm 5 6 lines per 30,0 mm 6 4 lines per 30,0 mm 7 3 lines per 30,0
    mm 8 12 lines per 30,0 mm 9 2 lines per 25,4 mm"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.150" :NAME "SYN" :TITLE "SYNCHRONOUS IDLE"
      :NOTATION (C0) :REPRESENTATION (("01/06"))
      :DESCRIPTION
      "SYN is used by a synchronous transmission system in the absence of
    any other character (idle condition) to provide a signal from which
    synchronism may be achieved or retained between data terminal
    equipment. The use of SYN is defined in ISO 1745."
      :DEFAULT NIL)
     (:REFERENCE "8.3.151" :NAME "TAC" :TITLE "TABULATION ALIGNED CENTRED"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "02/00" "06/02"))
      :DESCRIPTION
      "TAC causes a character tabulation stop calling for centring to be
    set at character position n in the active line (the line that contains
    the active presentation position) and lines of subsequent text in the
    presentation component, where n equals the value of Pn. TAC causes the
    replacement of any tabulation stop previously set at that character
    position, but does not affect other tabulation stops. A text string
    centred upon a tabulation stop set by TAC will be positioned so that
    the (trailing edge of the) first graphic character and the (leading
    edge of the) last graphic character are at approximately equal
    distances from the tabulation stop."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.152" :NAME "TALE"
      :TITLE "TABULATION ALIGNED LEADING EDGE" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "02/00" "06/01"))
      :DESCRIPTION
      "TALE causes a character tabulation stop calling for leading edge
    alignment to be set at character position n in the active line (the
    line that contains the active presentation position) and lines of
    subsequent text in the presentation component, where n equals the
    value of Pn. TALE causes the replacement of any tabulation stop
    previously set at that character position, but does not affect other
    tabulation stops. A text string aligned with a tabulation stop set by
    TALE will be positioned so that the (leading edge of the) last graphic
    character of the string is placed at the tabulation stop."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.153" :NAME "TATE"
      :TITLE "TABULATION ALIGNED TRAILING EDGE" :NOTATION (PN)
      :REPRESENTATION (("CSI" "Pn" "02/00" "06/00"))
      :DESCRIPTION
      "TATE causes a character tabulation stop calling for trailing edge
    alignment to be set at character position n in the active line (the
    line that contains the active presentation position) and lines of
    subsequent text in the presentation component, where n equals the
    value of Pn. TATE causes the replacement of any tabulation stop
    previously set at that character position, but does not affect other
    tabulation stops. A text string aligned with a tabulation stop set by
    TATE will be positioned so that the (trailing edge of the) first
    graphic character of the string is placed at the tabulation stop."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.154" :NAME "TBC" :TITLE "TABULATION CLEAR"
      :NOTATION (PS) :REPRESENTATION (("CSI" "Ps" "06/07"))
      :DESCRIPTION
      "TBC causes one or more tabulation stops in the presentation
    component to be cleared, depending on the parameter value: 0 the
    character tabulation stop at the active presentation position is
    cleared 1 the line tabulation stop at the active line is cleared 2 all
    character tabulation stops in the active line are cleared 3 all
    character tabulation stops are cleared 4 all line tabulation stops are
    cleared 5 all tabulation stops are cleared In the case of parameter
    value 0 or 2 the number of lines affected depends on the setting of
    the TABULATION STOP MODE (TSM)"
      :DEFAULT ((PS . 0)))
     (:REFERENCE "8.3.155" :NAME "TCC"
      :TITLE "TABULATION CENTRED ON CHARACTER" :NOTATION (PN1 PN2)
      :REPRESENTATION (("CSI" "Pn1;Pn2" "02/00" "06/03"))
      :DESCRIPTION
      "TCC causes a character tabulation stop calling for alignment of a
    target graphic character to be set at character position n in the
    active line (the line that contains the active presentation position)
    and lines of subsequent text in the presentation component, where n
    equals the value of Pn1, and the target character about which centring
    is to be performed is specified by Pn2. TCC causes the replacement of
    any tabulation stop previously set at that character position, but
    does not affect other tabulation stops. The positioning of a text
    string aligned with a tabulation stop set by TCC will be determined by
    the first occurrence in the string of the target graphic character;
    that character will be centred upon the tabulation stop. If the target
    character does not occur within the string, then the trailing edge of
    the first character of the string will be positioned at the tabulation
    stop. The value of Pn2 indicates the code table position (binary
    value) of the target character in the currently invoked code. For a
    7-bit code, the permissible range of values is 32 to 127; for an 8-bit
    code, the permissible range of values is 32 to 127 and 160 to 255."
      :DEFAULT NIL)
     (:REFERENCE "8.3.156" :NAME "TSR" :TITLE "TABULATION STOP REMOVE"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "02/00" "06/04"))
      :DESCRIPTION
      "TSR causes any character tabulation stop at character position n
    in the active line (the line that contains the active presentation
    position) and lines of subsequent text in the presentation component
    to be cleared, but does not affect other tabulation stops. n equals
    the value of Pn."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.157" :NAME "TSS" :TITLE "THIN SPACE SPECIFICATION"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "02/00" "04/05"))
      :DESCRIPTION
      "TSS is used to establish the width of a thin space for subsequent
    text. The established width remains in effect until the next
    occurrence of TSS in the data stream, see annex C. Pn specifies the
    width of the thin space. The unit in which the parameter value is
    expressed is that established by the parameter value of SELECT SIZE
    UNIT (SSU)."
      :DEFAULT ((* . :NONE)))
     (:REFERENCE "8.3.158" :NAME "VPA" :TITLE "LINE POSITION ABSOLUTE"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "06/04"))
      :DESCRIPTION
      "VPA causes the active data position to be moved to line position n
    in the data component in a direction parallel to the line progression,
    where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.159" :NAME "VPB" :TITLE "LINE POSITION BACKWARD"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "06/11"))
      :DESCRIPTION
      "VPB causes the active data position to be moved by n line
    positions in the data component in a direction opposite to that of the
    line progression, where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.160" :NAME "VPR" :TITLE "LINE POSITION FORWARD"
      :NOTATION (PN) :REPRESENTATION (("CSI" "Pn" "06/05"))
      :DESCRIPTION
      "VPR causes the active data position to be moved by n line
    positions in the data component in a direction parallel to the line
    progression, where n equals the value of Pn."
      :DEFAULT ((PN . 1)))
     (:REFERENCE "8.3.161" :NAME "VT" :TITLE "LINE TABULATION" :NOTATION (C0)
      :REPRESENTATION (("00/11"))
      :DESCRIPTION
      "VT causes the active presentation position to be moved in the
    presentation component to the corresponding character position on the
    line at which the following line tabulation stop is set."
      :DEFAULT NIL)
     (:REFERENCE "8.3.162" :NAME "VTS" :TITLE "LINE TABULATION SET"
      :NOTATION (C1) :REPRESENTATION (("08/10") ("ESC" "04/10"))
      :DESCRIPTION
      "VTS causes a line tabulation stop to be set at the active line
    (the line that contains the active presentation position)."
      :DEFAULT NIL))))


(defun parse-byte-specification (string)
  "Parses a string in the matching: ^\\([0-9][0-9]\\)/\\([0-9][0-9]\\)$
and return (VALUES high low) or NIL."
  (when (and (= 5 (length string))
             (digit-char-p (char string 0))
             (digit-char-p (char string 1))
             (char= (character "/") (char string 2))
             (digit-char-p (char string 3))
             (digit-char-p (char string 4)))
    (values (parse-integer string :start 0 :end 2)
            (parse-integer string :start 3 :end 5))))

  
(DEFUN GENERATE-CODE-FUNCTION (CODE &KEY
                                    (COMPILE NIL)
                                    (VERBOSE NIL)
                                    (EXPORT NIL)
                                    (8-BIT NIL)
                                    (print nil)
                                    (RESULT-TYPE '(VECTOR (UNSIGNED-BYTE 8))))
  "
DO:             Defines (and may be compile) and exports a function
                named as ,(CODE-NAME CODE) that
                takes as arguments ,(CODE-NOTATION CODE) and that
                returns a string or byte vector containing the control sequence.
                In addition, if the sequence contains only one constant
                byte, defines a constant of same name as the function equal
                to this byte, or this character if RESULT-TYPE is STRING.
CODE:           The code structure to be generated.
COMPILE:        Whether the generated function must be compiled right now.
VERBOSE:        If VERBOSE is true, prints a message in the form of a comment
                (i.e., with a leading semicolon) to standard output
                indicating what function is being compiled
                and other useful information.
                If VERBOSE is false, does not print this information.
EXPORT:         Whether the generated function symbol must be exported.
8-BIT:          Whether the generated function
                must return 8-bit escape sequences or 7-bit escape sequences.
PRINT:          If NIL, then return the escape sequence
                else the function takes an optional last argument of type 
                stream or T (which is the default) and writes the escape
                sequence to this stream, or *STANDARD-OUTPUT* for T.
RESULT-TYPE:    The type that the generated function must return:
                '(vector (unsigned-byte 8))  the default.
                'string  Note that it will be subject to encoding conversion!
BUGS:           Perhaps we should generate functions that take 8-BIT and
                RESULT-TYPE as arguments (or special variables) dynamically.
"
  (LABELS ((COMPILE-ARGS
               (FMT ARGS)
             (LIST `(MAP 'LIST (FUNCTION CHAR-CODE) 
                         (FORMAT NIL ,FMT ,@ARGS))))
           (COMPILE-REP
               (R)
             (LET (HIGH LOW)
               (COND
                 ((MULTIPLE-VALUE-SETQ (HIGH LOW) (parse-byte-specification R))
                  (LIST (+ (* high 16) low)))
                 ;; TODO: We are asuming ASCII with these FORMAT!
                 ((STRING-EQUAL "Ps1;Ps2" R) (COMPILE-ARGS "~D;~D" '(PS1 PS2)))
                 ((STRING-EQUAL "Pn1;Pn2" R) (COMPILE-ARGS "~D;~D" '(PN1 PN2)))
                 ((STRING-EQUAL "Pn"      R) (COMPILE-ARGS "~D" '(PN)))
                 ((STRING-EQUAL "Ps"      R) (COMPILE-ARGS "~D" '(PS)))
                 ((STRING-EQUAL "Ps..."   R)
                  (COMPILE-ARGS "~{~D~^;~}" 
                                (if print
                                    '((let ((last (car (last ps...))))
                                        (if (or (eq 't last) (streamp last))
                                            (progn (setf stream last)
                                                   (butlast PS...))
                                            ps...)))
                                    '(PS...))))
                 ((STRING-EQUAL "ESC"   R) (LIST 27))
                 ((STRING-EQUAL "CSI"   R) (IF 8-BIT (LIST 155) (LIST 27 91)))
                 (T (LIST R))))))
    (LET* ((ARGUMENTS
            (CDR (ASSOC (CODE-NOTATION CODE)
                        '(((FS) . ())
                          ((PS...) . (&REST PS...))
                          ((PS1 PS2)  . (PS1 PS2))
                          ((PS) . (PS))
                          ((PN1 PN2) . (PN1 PN2))
                          ((PN) . (PN))
                          ((C0) . ())
                          ((C1) . ())) :TEST (FUNCTION EQUAL))))
           (REPRESENTATION
            (LET ((REPS (CODE-REPRESENTATION CODE)))
              (IF (CDR REPS)
                  (FIRST
                   (FUNCALL
                    (IF 8-BIT(FUNCTION REMOVE-IF)(FUNCTION REMOVE-IF-NOT))
                    (LAMBDA (R) (MEMBER "ESC" R :TEST (FUNCTION STRING=))) REPS))
                  (CAR REPS))))
           (RESULT (LET* ((RESULT (MAPCAN (FUNCTION COMPILE-REP)
                                          REPRESENTATION))
                          (POS (POSITION-IF
                                (LAMBDA (X) (AND (CONSP X) (EQ 'MAP (CAR X))))
                                RESULT)))
                     (IF POS
                         `(APPEND ',(SUBSEQ RESULT 0 POS)
                                  ,(ELT RESULT POS) ',(SUBSEQ RESULT (1+ POS)))
                         RESULT)))
           (NAME (LET ((MINE *PACKAGE*))
                   (WITH-STANDARD-IO-SYNTAX
                     (INTERN (STRING-UPCASE (CODE-NAME CODE)) MINE))))
           (sequence (IF ARGUMENTS
                         `(MAP ',RESULT-TYPE ,(IF (EQ 'STRING RESULT-TYPE)
                                                  (FUNCTION CODE-CHAR)
                                                  (FUNCTION IDENTITY))
                               ,(IF (EQ 'APPEND (CAR RESULT))
                                    RESULT
                                    (CONS 'LIST RESULT)))
                         (MAP RESULT-TYPE (IF (EQ 'STRING RESULT-TYPE)
                                              (FUNCTION CODE-CHAR)
                                              (FUNCTION IDENTITY))
                              RESULT))))     
      (when print
        (setf arguments (append arguments
                                (if (eq '&rest (car arguments))
                                    '(&aux      (stream *standard-output*))
                                    '(&optional (stream *standard-output*))))))
      (FUNCALL
       (IF COMPILE (FUNCTION COMPILE) (FUNCTION IDENTITY))
       (EVAL `(PROGN
                ,@(WHEN EXPORT  `((EXPORT '(,NAME))))
                ,@(WHEN VERBOSE `((FORMAT T "~&; Defining ~A~%" ',NAME)))
                ,(when (and (not arguments) (= 1 (length sequence)))
                       `(defconstant ,name ,(aref sequence 0)
                          ,(code-title code)))
                (DEFUN ,NAME
                    ,ARGUMENTS
                  ,(concatenate 'string
                                (CODE-TITLE CODE)
                                (format nil "~2%    ")
                                (code-description code))
                  ,(if print
                       (if (eq result-type 'string)
                           `(princ ,sequence stream)
                           `(write-sequence ,sequence stream))
                       sequence))))))))


(DEFUN GENERATE-all-functions (&KEY (COMPILE NIL)
                               (VERBOSE NIL)
                               (EXPORT NIL)
                               (8-BIT NIL)
                               (print nil)
                               (RESULT-TYPE '(VECTOR (UNSIGNED-BYTE 8))))
  (dolist (code *codes*)
    (generate-code-function code
                            :verbose verbose
                            :compile compile
                            :export  export
                            :8-bit   8-bit
                            :print   print
                            :result-type result-type)))






;;;;;
;;;;; Let's the user generate himself the functions.
;;;;;

;; ;; It is better to use :result-type '(unsigned-byte 8) instead of 'string
;; ;; to avoid encoding problems.
;;
;; ;; At compile-time, let's generate the functions,
;; ;; compile them and export them.
;; ;;
;; (eval-when (:compile-toplevel)
;;   (generate-all-functions :verbose nil  ; *compile-verbose*
;;                           :compile t
;;                           :export  t
;;                           :8-bit   t
;;                           :print   nil
;;                           :result-type '(vector (unsigned-byte 8))))
;; 
;; 
;; ;; When loading the source (but not when loading the compiled file),
;; ;; let's generate the functions, not compiling them, and export them.
;; ;;
;; (eval-when (:load-toplevel :execute)
;;   (generate-all-functions :verbose nil  ; *compile-verbose*
;;                           :compile nil
;;                           :export  t
;;                           :8-bit   t
;;                           :print   nil
;;                           :result-type '(vector (unsigned-byte 8))))

(defun generate-all-functions-in-ecma048 ()
  (unless (and (boundp 'ht) (fboundp 'ht))
    (let ((*package* (find-package "COM.INFORMATIMAGO.COMMON-LISP.ECMA048")))
      (generate-all-functions :verbose *compile-verbose*
                              :compile t
                              :export  t
                              :8-bit   t
                              :print   nil
                              :result-type '(vector (unsigned-byte 8))))))



(defun description-of-code-named (name)
  (loop
     with text = (code-description (find name
                                         *codes*
                                         :key (function code-name)
                                         :test (function string=)))
     for start-line = 0 then (1+ end)
     for start = (position-if (lambda (ch) (or (not (char= ch #\space))
                                               (char= ch #\newline)))
                              text :start start-line)
     for end   = (and start (position #\newline text :start start))
     collect (subseq text start end)
     while end))


(defun print-documentation (&key (stream *standard-output*)
                            (description nil)
                            (sort-by :name)) ; or :title
  "
Prints the documentation of the escape sequence functions generated.
"
  (dolist (item (mapcar
                 (lambda (f) (list f
                                   (second (function-lambda-expression f))
                                   (documentation f 'function)))  
                 (mapcar (function intern)
                         (mapcar (function code-name)
                                 (sort *codes*
                                       (function string<)
                                       :key (if (eq sort-by :name)
                                                (function code-name)
                                                (function code-title)))))))
    (format stream ";; ~6A ~14A ~A~%" 
            (first item) (or (second item) "()") (third item))
    (when description
      (format stream ";; ~%~{;; ~A~%~};; ~%;; ~%"
              (description-of-code-named (string (first item)))))))


;;----------------------------------------------------------------------
;; Generating the shell functions
;;----------------------------------------------------------------------


(DEFUN GENERATE-SHELL-FUNCTION (CODE &KEY
                                     (VERBOSE NIL)
                                     (8-BIT NIL))
  "
CODE:           The code structure to be generated.
VERBOSE:        If VERBOSE is true, prints a message in the form of a comment
                (i.e., with a leading semicolon) to standard output
                indicating what function is being compiled
                and other useful information.
                If VERBOSE is false, does not print this information.
8-BIT:          Whether the generated function
                must return 8-bit codes or 7-bit codes.
RETURN:         A string containing a shell function definition
                named as (CODE-NAME CODE) that
                takes as arguments (CODE-NOTATION CODE) and that
                returns a string containing the control sequence.
BUGS:           Perhaps we should generate functions that take 8-BIT and
                RESULT-TYPE as arguments (or special variables) dynamically.
"
  (LABELS ((COMPILE-REP
               (R)
             (LET (HIGH LOW)
               (COND
                 ((MULTIPLE-VALUE-SETQ (HIGH LOW) (parse-byte-specification R))
                  (STRING (CODE-CHAR (+ (* high 16) low))))
                 ;; TODO: We are asuming ASCII with these FORMAT!
                 ((STRING-EQUAL "Ps1;Ps2" R) "${PS1};${PS2}")
                 ((STRING-EQUAL "Pn1;Pn2" R) "${PN1};${PN2}")
                 ((STRING-EQUAL "Pn"      R) "${PN}")
                 ((STRING-EQUAL "Ps"      R) "${PS}")
                 ((STRING-EQUAL "Ps..."   R) "$(semicolon='';res='';for arg in $@;do res=\"${res}${semicolon}${arg}\";semicolon=';';done;echo -n \"${res}\")")
                 ((STRING-EQUAL "ESC"     R) (STRING(CODE-CHAR 27)))
                 ((STRING-EQUAL "CSI"     R) (IF 8-BIT
                                                 (STRING(CODE-CHAR 155))
                                                 (CONCATENATE 'STRING
                                                   (STRING(CODE-CHAR 27))
                                                   (STRING(CODE-CHAR 91)))))
                 (T (LIST R))))))
    (LET* ((ARGUMENTS
            (CDR (ASSOC (CODE-NOTATION CODE)
                        '(((FS)      . "")
                          ((PS...)   . "")
                          ((PS1 PS2) . "local PS1=\"$1\";local PS2=\"$2\"")
                          ((PS)      . "local PS=\"$1\"")
                          ((PN1 PN2) . "local PN1=\"$1\";local PN2=\"$2\"")
                          ((PN)      . "local PN=\"$1\"")
                          ((C0)      . "")
                          ((C1)      . "")) :TEST (FUNCTION EQUAL))))
           (REPRESENTATION
            (LET ((REPS (CODE-REPRESENTATION CODE)))
              (IF (CDR REPS)
                  (FIRST
                   (FUNCALL
                    (IF 8-BIT(FUNCTION REMOVE-IF)(FUNCTION REMOVE-IF-NOT))
                    (LAMBDA (R) (MEMBER "ESC" R :TEST (FUNCTION STRING=))) REPS))
                  (CAR REPS))))
           (RESULT (MAPCAR (FUNCTION COMPILE-REP) REPRESENTATION))
           (NAME (STRING-UPCASE (CODE-NAME CODE))))
      (WHEN VERBOSE (FORMAT T "~&; Defining ~A~%" NAME))
      (FORMAT NIL
        "function ~A () {~%    # ~A~%    ~A~%    echo -n ~{~S~}~%}~%"
        NAME (CODE-TITLE CODE)
        ARGUMENTS RESULT))))


(defun print-shell-functions  (&key (stream *standard-output*))
  "
Prints sh/bash functions to print the escape sequences.
"
  (dolist (code *codes*)
    (princ (generate-shell-function code) stream)))



;; ------------------------------------------------------------------------
;; ACK    ()             ACKNOWLEDGE
;; APC    ()             APPLICATION PROGRAM COMMAND
;; BEL    ()             BELL
;; BPH    ()             BREAK PERMITTED HERE
;; BS     ()             BACKSPACE
;; CAN    ()             CANCEL
;; CBT    (PN)           CURSOR BACKWARD TABULATION
;; CCH    ()             CANCEL CHARACTER
;; CHA    (PN)           CURSOR CHARACTER ABSOLUTE
;; CHT    (PN)           CURSOR FORWARD TABULATION
;; CMD    ()             CODING METHOD DELIMITER
;; CNL    (PN)           CURSOR NEXT LINE
;; CPL    (PN)           CURSOR PRECEDING LINE
;; CPR    (PN1 PN2)      ACTIVE POSITION REPORT
;; CR     ()             CARRIAGE RETURN
;; CSI    ()             CONTROL SEQUENCE INTRODUCER
;; CTC    (&REST PS...)  CURSOR TABULATION CONTROL
;; CUB    (PN)           CURSOR LEFT
;; CUD    (PN)           CURSOR DOWN
;; CUF    (PN)           CURSOR RIGHT
;; CUP    (PN1 PN2)      CURSOR POSITION
;; CUU    (PN)           CURSOR UP
;; CVT    (PN)           CURSOR LINE TABULATION
;; DA     (PS)           DEVICE ATTRIBUTES
;; DAQ    (&REST PS...)  DEFINE AREA QUALIFICATION
;; DC1    ()             DEVICE CONTROL ONE
;; DC2    ()             DEVICE CONTROL TWO
;; DC3    ()             DEVICE CONTROL THREE
;; DC4    ()             DEVICE CONTROL FOUR
;; DCH    (PN)           DELETE CHARACTER
;; DCS    ()             DEVICE CONTROL STRING
;; DL     (PN)           DELETE LINE
;; DLE    ()             DATA LINK ESCAPE
;; DMI    ()             DISABLE MANUAL INPUT
;; DSR    (PS)           DEVICE STATUS REPORT
;; DTA    (PN1 PN2)      DIMENSION TEXT AREA
;; EA     (PS)           ERASE IN AREA
;; ECH    (PN)           ERASE CHARACTER
;; ED     (PS)           ERASE IN PAGE
;; EF     (PS)           ERASE IN FIELD
;; EL     (PS)           ERASE IN LINE
;; EM     ()             END OF MEDIUM
;; EMI    ()             ENABLE MANUAL INPUT
;; ENQ    ()             ENQUIRY
;; EOT    ()             END OF TRANSMISSION
;; EPA    ()             END OF GUARDED AREA
;; ESA    ()             END OF SELECTED AREA
;; ESC    ()             ESCAPE
;; ETB    ()             END OF TRANSMISSION BLOCK
;; ETX    ()             END OF TEXT
;; FF     ()             FORM FEED
;; FNK    (PN)           FUNCTION KEY
;; FNT    (PS1 PS2)      FONT SELECTION
;; GCC    (PS)           GRAPHIC CHARACTER COMBINATION
;; GSM    (PN1 PN2)      GRAPHIC SIZE MODIFICATION
;; GSS    (PN)           GRAPHIC SIZE SELECTION
;; HPA    (PN)           CHARACTER POSITION ABSOLUTE
;; HPB    (PN)           CHARACTER POSITION BACKWARD
;; HPR    (PN)           CHARACTER POSITION FORWARD
;; HT     ()             CHARACTER TABULATION
;; HTJ    ()             CHARACTER TABULATION WITH JUSTIFICATION
;; HTS    ()             CHARACTER TABULATION SET
;; HVP    (PN1 PN2)      CHARACTER AND LINE POSITION
;; ICH    (PN)           INSERT CHARACTER
;; IDCS   (PS)           IDENTIFY DEVICE CONTROL STRING
;; IGS    (PS)           IDENTIFY GRAPHIC SUBREPERTOIRE
;; IL     (PN)           INSERT LINE
;; INT    ()             INTERRUPT
;; IS1    ()             INFORMATION SEPARATOR ONE (US - UNIT SEPARATOR)
;; IS2    ()             INFORMATION SEPARATOR TWO (RS - RECORD SEPARATOR)
;; IS3    ()             INFORMATION SEPARATOR THREE (GS - GROUP SEPARATOR)
;; IS4    ()             INFORMATION SEPARATOR FOUR (FS - FILE SEPARATOR)
;; JFY    (&REST PS...)  JUSTIFY
;; LF     ()             LINE FEED
;; LS0    ()             LOCKING-SHIFT ZERO
;; LS1    ()             LOCKING-SHIFT ONE
;; LS2    ()             LOCKING-SHIFT TWO
;; LS3    ()             LOCKING-SHIFT THREE
;; MC     (PS)           MEDIA COPY
;; MW     ()             MESSAGE WAITING
;; NAK    ()             NEGATIVE ACKNOWLEDGE
;; NBH    ()             NO BREAK HERE
;; NEL    ()             NEXT LINE
;; NP     (PN)           NEXT PAGE
;; NUL    ()             NULL
;; OSC    ()             OPERATING SYSTEM COMMAND
;; PEC    (PS)           PRESENTATION EXPAND OR CONTRACT
;; PFS    (PS)           PAGE FORMAT SELECTION
;; PLD    ()             PARTIAL LINE FORWARD
;; PLU    ()             PARTIAL LINE BACKWARD
;; PM     ()             PRIVACY MESSAGE
;; PP     (PN)           PRECEDING PAGE
;; PPA    (PN)           PAGE POSITION ABSOLUTE
;; PPB    (PN)           PAGE POSITION BACKWARD
;; PPR    (PN)           PAGE POSITION FORWARD
;; PTX    (PS)           PARALLEL TEXTS
;; PU1    ()             PRIVATE USE ONE
;; PU2    ()             PRIVATE USE TWO
;; QUAD   (&REST PS...)  QUAD
;; REP    (PN)           REPEAT
;; RI     ()             REVERSE LINE FEED
;; RIS    ()             RESET TO INITIAL STATE
;; RM     (&REST PS...)  RESET MODE
;; SACS   (PN)           SET ADDITIONAL CHARACTER SEPARATION
;; SAPV   (&REST PS...)  SELECT ALTERNATIVE PRESENTATION VARIANTS
;; SCI    ()             SINGLE CHARACTER INTRODUCER
;; SCO    (PS)           SELECT CHARACTER ORIENTATION
;; SCP    (PS1 PS2)      SELECT CHARACTER PATH
;; SCS    (PN)           SET CHARACTER SPACING
;; SD     (PN)           SCROLL DOWN
;; SDS    (PS)           START DIRECTED STRING
;; SEE    (PS)           SELECT EDITING EXTENT
;; SEF    (PS1 PS2)      SHEET EJECT AND FEED
;; SGR    (&REST PS...)  SELECT GRAPHIC RENDITION
;; SHS    (PS)           SELECT CHARACTER SPACING
;; SI     ()             SHIFT-IN
;; SIMD   (PS)           SELECT IMPLICIT MOVEMENT DIRECTION
;; SL     (PN)           SCROLL LEFT
;; SLH    (PN)           SET LINE HOME
;; SLL    (PN)           SET LINE LIMIT
;; SLS    (PN)           SET LINE SPACING
;; SM     (&REST PS...)  SET MODE
;; SO     ()             SHIFT-OUT
;; SOH    ()             START OF HEADING
;; SOS    ()             START OF STRING
;; SPA    ()             START OF GUARDED AREA
;; SPD    (PS1 PS2)      SELECT PRESENTATION DIRECTIONS
;; SPH    (PN)           SET PAGE HOME
;; SPI    (PN1 PN2)      SPACING INCREMENT
;; SPL    (PN)           SET PAGE LIMIT
;; SPQR   (PS)           SELECT PRINT QUALITY AND RAPIDITY
;; SR     (PN)           SCROLL RIGHT
;; SRCS   (PN)           SET REDUCED CHARACTER SEPARATION
;; SRS    (PS)           START REVERSED STRING
;; SS2    ()             SINGLE-SHIFT TWO
;; SS3    ()             SINGLE-SHIFT THREE
;; SSA    ()             START OF SELECTED AREA
;; SSU    (PS)           SELECT SIZE UNIT
;; SSW    (PN)           SET SPACE WIDTH
;; ST     ()             STRING TERMINATOR
;; STAB   (PS)           SELECTIVE TABULATION
;; STS    ()             SET TRANSMIT STATE
;; STX    ()             START OF TEXT
;; SU     (PN)           SCROLL UP
;; SUB    ()             SUBSTITUTE
;; SVS    (PS)           SELECT LINE SPACING
;; SYN    ()             SYNCHRONOUS IDLE
;; TAC    (PN)           TABULATION ALIGNED CENTRED
;; TALE   (PN)           TABULATION ALIGNED LEADING EDGE
;; TATE   (PN)           TABULATION ALIGNED TRAILING EDGE
;; TBC    (PS)           TABULATION CLEAR
;; TCC    (PN1 PN2)      TABULATION CENTRED ON CHARACTER
;; TSR    (PN)           TABULATION STOP REMOVE
;; TSS    (PN)           THIN SPACE SPECIFICATION
;; VPA    (PN)           LINE POSITION ABSOLUTE
;; VPB    (PN)           LINE POSITION BACKWARD
;; VPR    (PN)           LINE POSITION FORWARD
;; VT     ()             LINE TABULATION
;; VTS    ()             LINE TABULATION SET
;; ------------------------------------------------------------------------



;; ------------------------------------------------------------------------
;; (PS1 PS2)
;; 
;;     SPD   : (CSI Ps1;Ps2 02/00 05/03)
;;     SEF   : (CSI Ps1;Ps2 02/00 05/09)
;;     SCP   : (CSI Ps1;Ps2 02/00 06/11)
;;     FNT   : (CSI Ps1;Ps2 02/00 04/04)
;; 
;; (PS)
;; 
;;     TBC   : (CSI Ps 06/07)
;;     SVS   : (CSI Ps 02/00 04/12)
;;     STAB  : (CSI Ps 02/00 05/14)
;;     SSU   : (CSI Ps 02/00 04/09)
;;     SRS   : (CSI Ps 05/11)
;;     SPQR  : (CSI Ps 02/00 05/08)
;;     SIMD  : (CSI Ps 05/14)
;;     SHS   : (CSI Ps 02/00 04/11)
;;     SEE   : (CSI Ps 05/01)
;;     SDS   : (CSI Ps 05/13)
;;     SCO   : (CSI Ps 02/00 06/05)
;;     PTX   : (CSI Ps 05/12)
;;     PFS   : (CSI Ps 02/00 04/10)
;;     PEC   : (CSI Ps 02/00 05/10)
;;     MC    : (CSI Ps 06/09)
;;     IGS   : (CSI Ps 02/00 04/13)
;;     IDCS  : (CSI Ps 02/00 04/15)
;;     GCC   : (CSI Ps 02/00 05/15)
;;     EL    : (CSI Ps 04/11)
;;     EF    : (CSI Ps 04/14)
;;     ED    : (CSI Ps 04/10)
;;     EA    : (CSI Ps 04/15)
;;     DSR   : (CSI Ps 06/14)
;;     DA    : (CSI Ps 06/03)
;; 
;; (PS...)
;; 
;;     SM    : (CSI Ps... 06/08)
;;     SGR   : (CSI Ps... 06/13)
;;     SAPV  : (CSI Ps... 02/00 05/13)
;;     RM    : (CSI Ps... 06/12)
;;     QUAD  : (CSI Ps... 02/00 04/08)
;;     JFY   : (CSI Ps... 02/00 04/06)
;;     DAQ   : (CSI Ps... 06/15)
;;     CTC   : (CSI Ps... 05/07)
;; 
;; (PN1 PN2)
;; 
;;     TCC   : (CSI Pn1;Pn2 02/00 06/03)
;;     SPI   : (CSI Pn1;Pn2 02/00 04/07)
;;     HVP   : (CSI Pn1;Pn2 06/06)
;;     GSM   : (CSI Pn1;Pn2 02/00 04/02)
;;     DTA   : (CSI Pn1;Pn2 02/00 05/04)
;;     CUP   : (CSI Pn1;Pn2 04/08)
;;     CPR   : (CSI Pn1;Pn2 05/02)
;; 
;; (FS)
;; 
;;     RIS   : (ESC 06/03)
;;     LS3   : (ESC 06/15)
;;     LS2   : (ESC 06/14)
;;     INT   : (ESC 06/01)
;;     EMI   : (ESC 06/02)
;;     DMI   : (ESC 06/00)
;;     CMD   : (ESC 06/04)
;; 
;; (PN)
;; 
;;     VPR   : (CSI Pn 06/05)
;;     VPB   : (CSI Pn 06/11)
;;     VPA   : (CSI Pn 06/04)
;;     TSS   : (CSI Pn 02/00 04/05)
;;     TSR   : (CSI Pn 02/00 06/04)
;;     TATE  : (CSI Pn 02/00 06/00)
;;     TALE  : (CSI Pn 02/00 06/01)
;;     TAC   : (CSI Pn 02/00 06/02)
;;     SU    : (CSI Pn 05/03)
;;     SSW   : (CSI Pn 02/00 05/11)
;;     SRCS  : (CSI Pn 02/00 06/06)
;;     SR    : (CSI Pn 02/00 04/01)
;;     SPL   : (CSI Pn 02/00 06/10)
;;     SPH   : (CSI Pn 02/00 06/09)
;;     SLS   : (CSI Pn 02/00 06/08)
;;     SLL   : (CSI Pn 02/00 05/06)
;;     SLH   : (CSI Pn 02/00 05/05)
;;     SL    : (CSI Pn 02/00 04/00)
;;     SD    : (CSI Pn 05/04)
;;     SCS   : (CSI Pn 02/00 06/07)
;;     SACS  : (CSI Pn 02/00 05/12)
;;     REP   : (CSI Pn 06/02)
;;     PPR   : (CSI Pn 02/00 05/01)
;;     PPB   : (CSI Pn 02/00 05/02)
;;     PPA   : (CSI Pn 02/00 05/00)
;;     PP    : (CSI Pn 05/06)
;;     NP    : (CSI Pn 05/05)
;;     IL    : (CSI Pn 04/12)
;;     ICH   : (CSI Pn 04/00)
;;     HPR   : (CSI Pn 06/01)
;;     HPB   : (CSI Pn 06/10)
;;     HPA   : (CSI Pn 06/00)
;;     GSS   : (CSI Pn 02/00 04/03)
;;     FNK   : (CSI Pn 02/00 05/07)
;;     ECH   : (CSI Pn 05/08)
;;     DL    : (CSI Pn 04/13)
;;     DCH   : (CSI Pn 05/00)
;;     CVT   : (CSI Pn 05/09)
;;     CUU   : (CSI Pn 04/01)
;;     CUF   : (CSI Pn 04/03)
;;     CUD   : (CSI Pn 04/02)
;;     CUB   : (CSI Pn 04/04)
;;     CPL   : (CSI Pn 04/06)
;;     CNL   : (CSI Pn 04/05)
;;     CHT   : (CSI Pn 04/09)
;;     CHA   : (CSI Pn 04/07)
;;     CBT   : (CSI Pn 05/10)
;; 
;; (C1)
;; 
;;     VTS   : (08/10) (ESC 04/10)
;;     STS   : (09/03) (ESC 05/03)
;;     ST    : (09/12) (ESC 05/12)
;;     SS3   : (08/15) (ESC 04/15)
;;     SS2   : (08/14) (ESC 04/14)
;;     SSA   : (08/06) (ESC 04/06)
;;     SPA   : (09/06) (ESC 05/06)
;;     SOS   : (09/08) (ESC 05/08)
;;     SCI   : (09/10) (ESC 05/10)
;;     RI    : (08/13) (ESC 04/13)
;;     PU2   : (09/02) (ESC 05/02)
;;     PU1   : (09/01) (ESC 05/01)
;;     PM    : (09/14) (ESC 05/14)
;;     PLU   : (08/12) (ESC 04/12)
;;     PLD   : (08/11) (ESC 04/11)
;;     OSC   : (09/13) (ESC 05/13)
;;     NEL   : (08/05) (ESC 04/05)
;;     NBH   : (08/03) (ESC 04/03)
;;     MW    : (09/05) (ESC 05/05)
;;     HTS   : (08/08) (ESC 04/08)
;;     HTJ   : (08/09) (ESC 04/09)
;;     ESA   : (08/07) (ESC 04/07)
;;     EPA   : (09/07) (ESC 05/07)
;;     DCS   : (09/00) (ESC 05/00)
;;     CSI   : (09/11) (ESC 05/11)
;;     CCH   : (09/04) (ESC 05/04)
;;     BPH   : (08/02) (ESC 04/02)
;;     APC   : (09/15) (ESC 05/15)
;; 
;; (C0)
;; 
;;     VT    : (00/11)
;;     SYN   : (01/06)
;;     SUB   : (01/10)
;;     STX   : (00/02)
;;     SOH   : (00/01)
;;     SO    : (00/14)
;;     SI    : (00/15)
;;     NUL   : (00/00)
;;     NAK   : (01/05)
;;     LS1   : (00/14)
;;     LS0   : (00/15)
;;     LF    : (00/10)
;;     IS4   : (01/12)
;;     IS3   : (01/13)
;;     IS2   : (01/14)
;;     IS1   : (01/15)
;;     HT    : (00/09)
;;     FF    : (00/12)
;;     ETX   : (00/03)
;;     ETB   : (01/07)
;;     ESC   : (01/11)
;;     EOT   : (00/04)
;;     ENQ   : (00/05)
;;     EM    : (01/09)
;;     DLE   : (01/00)
;;     DC4   : (01/04)
;;     DC3   : (01/03)
;;     DC2   : (01/02)
;;     DC1   : (01/01)
;;     CR    : (00/13)
;;     CAN   : (01/08)
;;     BS    : (00/08)
;;     BEL   : (00/07)
;;     ACK   : (00/06)
;; ------------------------------------------------------------------------




#||
(in-package "COM.INFORMATIMAGO.COMMON-LISP.ECMA048")


(dolist (c *codes*)
  (terpri)
  (dolist (r '(string (vector (unsigned-byte 8))))
    (dolist (h '(nil t))
      (print (generate-code-function c :8-bit h :result-type r)))))

(dolist (c classes)
  (format t "~%;; ~A~%;; ~:{~%;;     ~5A : ~{~A~^ ~}~}~%;; "
          (cdr (assoc :notation (first c)))
          (mapcar (lambda (e) (list (cdr (assoc :name e))
                                    (cdr (assoc :representation e)))) c)))

||#



;;;; ecma084.lisp                     --                     --          ;;;;
