;;;; -*- coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mysql.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    MySQL FFI.
;;;;    This package exports mysql functions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-04-21 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2006
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
;;;;**************************************************************************

(DEFINE-PACKAGE "COM.INFORMATIMAGO.CLISP.MYSQL"
  (:NICKNAMES "MYSQL")
  (:DOCUMENTATION
   "This package exports mysql functions.

    Copyright Pascal J. Bourguignon 2006 - 2006
    This package is provided under the GNU General Public License.
    See the source file for details.")
  (:FROM "COMMON-LISP" :IMPORT :ALL)
  ;;   (:USE "COM.INFORMATIMAGO.COMMON-LISP.STREAM")
  ;;   (:FROM "COM.INFORMATIMAGO.COMMON-LISP.STRING"  :IMPORT :ALL)
  (:FROM "COM.INFORMATIMAGO.COMMON-LISP.UTILITY" :IMPORT :ALL)
  (:EXPORT ))


(defconstant +library+ 
  #+unix "/usr/lib/libmysqlclient.so"
  #-unix (error "Where is the mysqlclient library?"))

(ffi:def-c-type my-bool   ffi:char)
(ffi:def-c-type gptr      (ffi:c-pointer ffi:char))
(ffi:def-c-type my-socket ffi:int)

(defconstant +name-len+                    64 "Field/table name length")
(defconstant +HOSTNAME-LENGTH+             60)
(defconstant +USERNAME-LENGTH+             16)
(defconstant +SERVER-VERSION-LENGTH        60)
(defconstant +LOCAL-HOST+             "localhost")
(defconstant +LOCAL-HOST-NAMEDPIPE+   ".")


(defenum enum-server-command
  +COM-SLEEP+ +COM-QUIT+ +COM-INIT-DB+ +COM-QUERY+ +COM-FIELD-LIST+
  +COM-CREATE-DB+ +COM-DROP-DB+ +COM-REFRESH+ +COM-SHUTDOWN+
  +COM-STATISTICS+ +COM-PROCESS-INFO+ +COM-CONNECT+
  +COM-PROCESS-KILL+ +COM-DEBUG+ +COM-PING+ +COM-TIME+
  +COM-DELAYED-INSERT+ +COM-CHANGE-USER+ +COM-BINLOG-DUMP+
  +COM-TABLE-DUMP+ +COM-CONNECT-OUT+)


(defconstant +NOT-NULL-FLAG+                1 "Field can't be NULL")
(defconstant +PRI-KEY-FLAG+                 2 "Field is part of a primary key")
(defconstant +UNIQUE-KEY-FLAG+              4 "Field is part of a unique key")
(defconstant +MULTIPLE-KEY-FLAG+            8 "Field is part of a key")
(defconstant +BLOB-FLAG+                   16 "Field is a blob")
(defconstant +UNSIGNED-FLAG+               32 "Field is unsigned")
(defconstant +ZEROFILL-FLAG+               64 "Field is zerofill")
(defconstant +BINARY-FLAG+                128 "")
;; The following are only sent to new clients 
(defconstant +ENUM-FLAG+                  256 "field is an enum")
(defconstant +AUTO-INCREMENT-FLAG+        512 "field is a autoincrement field")
(defconstant +TIMESTAMP-FLAG+            1024 "Field is a timestamp")
(defconstant +SET-FLAG+                  2048 "field is a set")
(defconstant +NUM-FLAG+                 32768 "Field is num (for clients)")
(defconstant +PART-KEY-FLAG+            16384 "Intern; Part of some key")
(defconstant +GROUP-FLAG+               32768 "Intern: Group field")
(defconstant +UNIQUE-FLAG+              65536 "Intern: Used by sql-yacc")

(defconstant +REFRESH-GRANT+                1 "Refresh grant tables")
(defconstant +REFRESH-LOG+                  2 "Start on new log file")
(defconstant +REFRESH-TABLES+               4 "close all tables")
(defconstant +REFRESH-HOSTS+                8 "Flush host cache")
(defconstant +REFRESH-STATUS+              16 "Flush status variables")
(defconstant +REFRESH-THREADS+             32 "Flush thread cache")
(defconstant +REFRESH-SLAVE+               64 
  "Reset master info and restart slave thread")
(defconstant +REFRESH-MASTER+             128 
  "Remove all bin logs in the index and truncate the index")

;; The following can't be set with mysql-refresh() 
(defconstant +REFRESH-READ-LOCK+        16384 "Lock tables for read")
(defconstant +REFRESH-FAST+             32768 "Intern flag")

(defconstant +CLIENT-LONG-PASSWORD+         1 "new more secure passwords")
(defconstant +CLIENT-FOUND-ROWS+            2 "Found instead of affected rows")
(defconstant +CLIENT-LONG-FLAG+             4 "Get all column flags")
(defconstant +CLIENT-CONNECT-WITH-DB+       8 "One can specify db on connect")
(defconstant +CLIENT-NO-SCHEMA+            16 "Don't allow database.table.column")
(defconstant +CLIENT-COMPRESS+             32 "Can use compression protocol")
(defconstant +CLIENT-ODBC+                 64 "Odbc client")
(defconstant +CLIENT-LOCAL-FILES+         128 "Can use LOAD DATA LOCAL")
(defconstant +CLIENT-IGNORE-SPACE+        256 "Ignore spaces before '('")
(defconstant +CLIENT-CHANGE-USER+         512 "Support the mysql-change-user()")
(defconstant +CLIENT-INTERACTIVE+        1024 "This is an interactive client")
(defconstant +CLIENT-SSL+                2048 "Switch to SSL after handshake")
(defconstant +CLIENT-IGNORE-SIGPIPE+     4096 "IGNORE sigpipes")
(defconstant +CLIENT-TRANSACTIONS+       8192 "Client knows about transactions")

(defconstant +SERVER-STATUS-IN-TRANS+       1 "Transaction has started")
(defconstant +SERVER-STATUS-AUTOCOMMIT+     2 "Server in auto-commit mode")

(defconstant +MYSQL-ERRMSG-SIZE+          200)
(defconstant +NET-READ-TIMEOUT+            30 "Timeout on read")
(defconstant +NET-WRITE-TIMEOUT+           60 "Timeout on write")
(defconstant +NET-WAIT-TIMEOUT+   (* 8 60 60) "Wait for new query")


(defconstant +MAX-CHAR-WIDTH+             255 "Max length for a CHAR colum")
(defconstant +MAX-BLOB-WIDTH+            8192 "Default width for blob")

(ffi:def-c-type net
  (vio                ffi:c-pointer)
  (fd                 my-socket)        ; For Perl DBI/dbd 
  (fcntl              ffi:int)
  (buff               ffi:c-pointer)
  (buff-end           ffi:c-pointer)
  (write-pos          ffi:c-pointer)
  (read-pos           ffi:c-pointer)
  (last-error        (ffi:c-array-max ffi:char +MYSQL-ERRMSG-SIZE+))
  (last-errno         ffi:uint)
  (max-packet         ffi:uint)
  (timeout            ffi:uint)
  (pkt-nr             ffi:uint)
  (error              ffi:uchar)
  (return-errno       my-bool)
  (compress           my-bool)
  (no-send-ok         my-bool)        ; needed if we are doing several
  ;; queries in one command ( as in LOAD TABLE ... FROM MASTER ),
  ;; and do not want to confuse the client with OK at the wrong time.
  (remain-in-buf      ffi:ulong)
  (length             ffi:ulong)
  (buf-length         ffi:ulong)
  (where-b            ffi:ulong)
  (return-status      (ffi:c-ptr-null ffi:uint))
  (reading-or-writing ffi:uchar)
  (save-char          ffi:char))

(defconstant +packet-error+ #xffffffff)

(defenum enum-field-types 
  +FIELD-TYPE-DECIMAL+
  +FIELD-TYPE-TINY+
  +FIELD-TYPE-SHORT+
  +FIELD-TYPE-LONG+
  +FIELD-TYPE-FLOAT+
  +FIELD-TYPE-DOUBLE+
  +FIELD-TYPE-NULL+
  +FIELD-TYPE-TIMESTAMP+
  +FIELD-TYPE-LONGLONG+
  +FIELD-TYPE-INT24+
  +FIELD-TYPE-DATE+
  +FIELD-TYPE-TIME+
  +FIELD-TYPE-DATETIME+
  +FIELD-TYPE-YEAR+
  +FIELD-TYPE-NEWDATE+
  (+FIELD-TYPE-ENUM+        247)
  (+FIELD-TYPE-SET+         248)
  (+FIELD-TYPE-TINY-BLOB+   249)
  (+FIELD-TYPE-MEDIUM-BLOB+ 250)
  (+FIELD-TYPE-LONG-BLOB+   251)
  (+FIELD-TYPE-BLOB+        252)
  (+FIELD-TYPE-VAR-STRING+  253)
  (+FIELD-TYPE-STRING+      254))

(defconstant +FIELD-TYPE-CHAR+        +FIELD-TYPE-TINY+ "For compability")
(defconstant +FIELD-TYPE-INTERVAL+    +FIELD-TYPE-ENUM+ "For compability")

(ffi:def-c-var max-allowed-packet 
    (:name "max_allowed_packet")
  (:type ffi:ulong)
  (:library #.+library+))
(ffi:def-c-var net-buffer-length
    (:name "net_buffer_length")
  (:type ffi:ulong)
  (:library #.+library+))

(defun net-new-transaction (net) (setf (ffi:slot net 'pkt-nr) 0))

(ffi:def-call-out my-net-init
    (:name "my_net_init")
  (:arguments (net (ffi:c-pointer net)) (vio ffi:c-pointer))
  (:result-type ffi:int)
  (:library #.+library+))

(ffi:def-call-out net-end
    (:name "net_end")
  (:arguments (net (ffi:c-pointer net)))
  (:result-type nil)
  (:library #.+library+))

(ffi:def-call-out net-clear
    (:name "net_clear")
  (:arguments (net (ffi:c-pointer net)))
  (:result-type nil)
  (:library #.+library+))

(ffi:def-call-out net-flush
    (:name "net_flush")
  (:arguments (net (ffi:c-pointer net)))
  (:result-type nil)
  (:library #.+library+))

(ffi:def-call-out my-net-write
    (:name "my_net_write")
  (:arguments (net (ffi:c-pointer net))
              (packet (ffi:array ffi:uchar) :in)
              (len ffi:ulong))
  (:result-type ffi:int)
  (:library #.+library+))

(ffi:def-call-out net-write
    (:name "net_write")
  (:arguments (net (ffi:c-pointer net))
              (command ffi:uchar)
              (packet (ffi:array ffi:uchar) :in)
              (len ffi:ulong))
  (:result-type ffi:int)
  (:library #.+library+))

(ffi:def-call-out net-real-write
    (:name "net_real_write")
  (:arguments (net (ffi:c-pointer net))
              (packet (ffi:array ffi:uchar) :in)
              (len ffi:ulong))
  (:result-type ffi:int)
  (:library #.+library+))

(ffi:def-call-out my-net-read
    (:name "my_net_read")
  (:arguments (net (ffi:c-pointer net)))
  (:result-type ffi:uint)
  (:library #.+library+))


(ffi:def-c-type rand-struct
  (seed1         ffi:ulong)
  (seed2         ffi:ulong)
  (max-value     ffi:ulong)
  (max-value-dbl ffi:double))

;; The following is for user defined functions 

(defenum item-result
  +STRING-RESULT+
  +REAL-RESULT+
  +INT-RESULT+)

(ffi:def-c-type udf-args
  (arg-count   ffi:uint)                     ; Number of arguments 
  (arg-type   (ffi:c-array-ptr item-result)) ; Pointer to item-results
  (args       (ffi:c-array-ptr ffi:c-string)) ; Pointer to argument
  (lengths    (ffi:c-array-ptr ffi:ulong)) ; Length of string arguments
  (maybe-null (ffi:c-array-ptr ffi:char))) ; Set to 1 for all maybe-null args

;; This holds information about the result

(ffi:def-c-type udf-init
  (maybe-null  my-bool)               ; 1 iff function can return NULL
  (decimals    ffi:uint)                ; for real functions
  (max-length  ffi:uint)                ; For string functions
  (ptr         ffi:c-pointer)        ; free pointer for function data 
  (const-item  my-bool))   ;  0 if result is independent of arguments 

;; Constants when using compression 
(defconstant +NET-HEADER-SIZE+              4 "standard header size")
(defconstant +COMP-HEADER-SIZE+             3 "compression header extra size")

;; Prototypes to password functions

(ffi:def-call-out random-init
    (:name "randominit")
  (:arguments (rs (ffi:c-ptr rand-struct)) (seed1 ffi:ulong) (seed2 ffi:ulong))
  (:result-type nil)
  (:library #.+library+))

(ffi:def-call-out rnd
    (:name "rnd")
  (:arguments (rs (ffi:c-ptr rand-struct)))
  (:result-type ffi:double-float)
  (:library #.+library+))

(ffi:def-call-out make-scrambled-password
    (:name "make_scrambled_password")
  (:arguments (to ffi:c-string :out) (password ffi:c-string :in))
  (:result-type nil)
  (:library #.+library+))

(ffi:def-call-out get-salt-from-password
    (:name "get_salt_from_password")
  (:arguments (res ffi:ulong :out) (password ffi:c-string :in))
  (:result-type nil)
  (:library #.+library+))


void make-password-from-salt(char *to, unsigned long *hash-res) ;
char *scramble(char *to,const char *message,const char *password,
                    my-bool old-ver)    ;
my-bool check-scramble(const char *, const char *message,
                             unsigned long *salt,my-bool old-ver) ;
char *get-tty-password(char *opt-message)                         ;
void hash-password(unsigned long *result, const char *password)   ;

/* Some other useful functions */

void my-init(void)                      ;
void load-defaults(const char *conf-file, const char **groups,
                         int *argc, char ***argv) ;
my-bool my-thread-init(void)                      ;
void my-thread-end(void)                          ;


(defconstant +NULL-LENGTH+ #xffffffff "For net-store-length")

;; Version numbers for protocol & mysqld 

(defconstant +PROTOCOL_VERSION+            10)
(defconstant +MYSQL_SERVER_VERSION+ "3.23.55")
(defconstant +MYSQL_SERVER_SUFFIX+         "")
(defconstant +FRM_VER+                      6)
(defconstant +MYSQL_VERSION_ID+         32355)
(defconstant +MYSQL_PORT+                3306)
(defconstant +MYSQL_UNIX_ADDR+       "/var/lib/mysql/mysql.sock")
(defconstant +MYSQL_CONFIG_NAME+         "my")

;; mysqld compile time options 
(defconstant +MYSQL_CHARSET+          "latin1")


(ffi:def-c-var mysql-port      ffi:uint)
(ffi:def-c-var mysql-unix-port ffi:c-string)

#define IS-PRI-KEY(n)	((n) & PRI-KEY-FLAG)
#define IS-NOT-NULL(n)	((n) & NOT-NULL-FLAG)
#define IS-BLOB(n)	((n) & BLOB-FLAG)
#define IS-NUM(t)	((t) <= FIELD-TYPE-INT24 || (t) == FIELD-TYPE-YEAR)
#define IS-NUM-FIELD(f)	 ((f)->flags & NUM-FLAG)
#define INTERNAL-NUM-FIELD(f) (((f)->type <= FIELD-TYPE-INT24 && ((f)->type != FIELD-TYPE-TIMESTAMP || (f)->length == 14 || (f)->length == 8)) || (f)->type == FIELD-TYPE-YEAR)

typedef struct st-mysql-field {
char *name                            ;			/* Name of column */
char *table      ;			/* Table of column if column was a field */
char *def    ;			/* Default value (set by mysql-list-fields) */
enum enum-field-types type ;	/* Type of field. Se mysql-com.h for types */
unsigned int length                     ;		/* Width of column */
unsigned int max-length              ;	/* Max width of selected set */
unsigned int flags                      ;		/* Div flags */
unsigned int decimals              ;	/* Number of decimals in field */
} MYSQL-FIELD                           ;

typedef char **MYSQL-ROW  ;		/* return data as array of strings */
typedef unsigned int MYSQL-FIELD-OFFSET; /* offset to current field */

#if defined(NO-CLIENT-LONG-LONG)
typedef unsigned long my-ulonglong      ;
#elif defined (--WIN--)
typedef unsigned --int64 my-ulonglong   ;
#else
typedef unsigned long long my-ulonglong ;
#endif

#define MYSQL-COUNT-ERROR (~(my-ulonglong) 0)

typedef struct st-mysql-rows {
struct st-mysql-rows *next              ;		/* list of rows */
MYSQL-ROW data                          ;
} MYSQL-ROWS                            ;

typedef MYSQL-ROWS *MYSQL-ROW-OFFSET  ;	/* offset to current row */

typedef struct st-mysql-data {
my-ulonglong rows                       ;
unsigned int fields                     ;
MYSQL-ROWS *data                        ;
MEM-ROOT alloc                          ;
} MYSQL-DATA                            ;

struct st-mysql-options {
unsigned int connect-timeout,client-flag;
my-bool compress,named-pipe             ;
unsigned int port                       ;
char *host,*init-command,*user,*password,*unix-socket,*db     ;
char *my-cnf-file,*my-cnf-group, *charset-dir, *charset-name  ;
my-bool use-ssl            ;				/* if to use SSL or not */
char *ssl-key                        ;				/* PEM key file */
char *ssl-cert                      ;				/* PEM cert file */
char *ssl-ca                      ;					/* PEM CA file */
char *ssl-capath            ;				/* PEM directory of CA-s? */
}                                       ;

enum mysql-option { MYSQL-OPT-CONNECT-TIMEOUT, MYSQL-OPT-COMPRESS,
MYSQL-OPT-NAMED-PIPE, MYSQL-INIT-COMMAND,
MYSQL-READ-DEFAULT-FILE, MYSQL-READ-DEFAULT-GROUP,
MYSQL-SET-CHARSET-DIR, MYSQL-SET-CHARSET-NAME,
MYSQL-OPT-LOCAL-INFILE}                 ;

enum mysql-status { MYSQL-STATUS-READY,MYSQL-STATUS-GET-RESULT,
MYSQL-STATUS-USE-RESULT}                ;

typedef struct st-mysql {
NET		net                ;			/* Communication parameters */
gptr		connector-fd              ;		/* ConnectorFd for SSL */
char		*host,*user,*passwd,*unix-socket,*server-version,*host-info,
*info,*db                                            ;
unsigned int	port,client-flag,server-capabilities ;
unsigned int	protocol-version                     ;
unsigned int	field-count                          ;
unsigned int 	server-status                        ;
unsigned long thread-id      ;		/* Id for connection in server */
my-ulonglong affected-rows              ;
my-ulonglong insert-id  ;		/* id if insert on table with NEXTNR */
my-ulonglong extra-info                 ;		/* Used by mysqlshow */
unsigned long packet-length             ;
enum mysql-status status                ;
MYSQL-FIELD	*fields                     ;
MEM-ROOT	field-alloc                 ;
my-bool	free-me                    ;		/* If free in mysql-close */
my-bool	reconnect          ;		/* set to 1 if automatic reconnect */
struct st-mysql-options options         ;
char	        scramble-buff[9]        ;
struct charset-info-st *charset         ;
unsigned int  server-language           ;
} MYSQL                                 ;


typedef struct st-mysql-res {
my-ulonglong row-count                     ;
unsigned int	field-count, current-field ;
MYSQL-FIELD	*fields                        ;
MYSQL-DATA	*data                          ;
MYSQL-ROWS	*data-cursor                   ;
MEM-ROOT	field-alloc                    ;
MYSQL-ROW	row                    ;			/* If unbuffered read */
MYSQL-ROW	current-row              ;		/* buffer to current row */
unsigned long *lengths      ;		/* column lengths of current row */
MYSQL		*handle                ;		/* for unbuffered reads */
my-bool	eof                  ;			/* Used my mysql-fetch-row */
} MYSQL-RES                             ;

/* Functions to get information from the MYSQL and MYSQL-RES structures */
/* Should definitely be used if one uses shared libraries */

my-ulonglong STDCALL mysql-num-rows(MYSQL-RES *res)   ;
unsigned int STDCALL mysql-num-fields(MYSQL-RES *res) ;
my-bool STDCALL mysql-eof(MYSQL-RES *res)             ;
MYSQL-FIELD *STDCALL mysql-fetch-field-direct(MYSQL-RES *res,
                                                        unsigned int fieldnr) ;
MYSQL-FIELD * STDCALL mysql-fetch-fields(MYSQL-RES *res) ;
MYSQL-ROWS * STDCALL mysql-row-tell(MYSQL-RES *res)      ;
unsigned int STDCALL mysql-field-tell(MYSQL-RES *res)    ;

unsigned int STDCALL mysql-field-count(MYSQL *mysql)   ;
my-ulonglong STDCALL mysql-affected-rows(MYSQL *mysql) ;
my-ulonglong STDCALL mysql-insert-id(MYSQL *mysql)     ;
unsigned int STDCALL mysql-errno(MYSQL *mysql)         ;
char * STDCALL mysql-error(MYSQL *mysql)               ;
char * STDCALL mysql-info(MYSQL *mysql)                ;
unsigned long STDCALL mysql-thread-id(MYSQL *mysql)    ;
const char * STDCALL mysql-character-set-name(MYSQL *mysql) ;

MYSQL *		STDCALL mysql-init(MYSQL *mysql) ;
#ifdef HAVE-OPENSSL
int		STDCALL mysql-ssl-set(MYSQL *mysql, const char *key,
                                    const char *cert, const char *ca,
                                    const char *capath) ;
char *		STDCALL mysql-ssl-cipher(MYSQL *mysql)      ;
int		STDCALL mysql-ssl-clear(MYSQL *mysql)           ;
#endif /* HAVE-OPENSSL */
MYSQL *		STDCALL mysql-connect(MYSQL *mysql, const char *host,
                                        const char *user, const char *passwd) ;
my-bool		STDCALL mysql-change-user(MYSQL *mysql, const char *user, 
                                            const char *passwd, const char *db) ;
#if MYSQL-VERSION-ID >= 32200
MYSQL *		STDCALL mysql-real-connect(MYSQL *mysql, const char *host,
                                             const char *user,
                                             const char *passwd,
                                             const char *db,
                                             unsigned int port,
                                             const char *unix-socket,
                                             unsigned int clientflag) ;
#else
MYSQL *		STDCALL mysql-real-connect(MYSQL *mysql, const char *host,
                                             const char *user,
                                             const char *passwd,
                                             unsigned int port,
                                             const char *unix-socket,
                                             unsigned int clientflag) ;
#endif
void		STDCALL mysql-close(MYSQL *sock)                  ;
int		STDCALL mysql-select-db(MYSQL *mysql, const char *db) ;
int		STDCALL mysql-query(MYSQL *mysql, const char *q)      ;
int		STDCALL mysql-send-query(MYSQL *mysql, const char *q,
                                       unsigned int length) ;
int		STDCALL mysql-read-query-result(MYSQL *mysql)       ;
int		STDCALL mysql-real-query(MYSQL *mysql, const char *q,
                                       unsigned int length)   ;
int		STDCALL mysql-create-db(MYSQL *mysql, const char *DB) ;
int		STDCALL mysql-drop-db(MYSQL *mysql, const char *DB)   ;
int		STDCALL mysql-shutdown(MYSQL *mysql)                  ;
int		STDCALL mysql-dump-debug-info(MYSQL *mysql)           ;
int		STDCALL mysql-refresh(MYSQL *mysql,
                                    unsigned int refresh-options) ;
int		STDCALL mysql-kill(MYSQL *mysql,unsigned long pid)        ;
int		STDCALL mysql-ping(MYSQL *mysql)                          ;
char *		STDCALL mysql-stat(MYSQL *mysql)                      ;
char *		STDCALL mysql-get-server-info(MYSQL *mysql)           ;
char *		STDCALL mysql-get-client-info(void)                   ;
char *		STDCALL mysql-get-host-info(MYSQL *mysql)             ;
unsigned int	STDCALL mysql-get-proto-info(MYSQL *mysql)        ;
MYSQL-RES *	STDCALL mysql-list-dbs(MYSQL *mysql,const char *wild) ;
MYSQL-RES *	STDCALL mysql-list-tables(MYSQL *mysql,const char *wild) ;
MYSQL-RES *	STDCALL mysql-list-fields(MYSQL *mysql, const char *table,
                                            const char *wild) ;
MYSQL-RES *	STDCALL mysql-list-processes(MYSQL *mysql)        ;
MYSQL-RES *	STDCALL mysql-store-result(MYSQL *mysql)          ;
MYSQL-RES *	STDCALL mysql-use-result(MYSQL *mysql)            ;
int		STDCALL mysql-options(MYSQL *mysql,enum mysql-option option,
                                    const char *arg)     ;
void		STDCALL mysql-free-result(MYSQL-RES *result) ;
void		STDCALL mysql-data-seek(MYSQL-RES *result,
                                              my-ulonglong offset) ;
MYSQL-ROW-OFFSET STDCALL mysql-row-seek(MYSQL-RES *result, MYSQL-ROW-OFFSET) ;
MYSQL-FIELD-OFFSET STDCALL mysql-field-seek(MYSQL-RES *result,
                                                      MYSQL-FIELD-OFFSET offset) ;
MYSQL-ROW	STDCALL mysql-fetch-row(MYSQL-RES *result)         ;
unsigned long * STDCALL mysql-fetch-lengths(MYSQL-RES *result) ;
MYSQL-FIELD *	STDCALL mysql-fetch-field(MYSQL-RES *result)   ;
unsigned long	STDCALL mysql-escape-string(char *to,const char *from,
                                                 unsigned long from-length) ;
unsigned long STDCALL mysql-real-escape-string(MYSQL *mysql,
                                                     char *to,const char *from,
                                                     unsigned long length) ;
void		STDCALL mysql-debug(const char *debug) ;
char *		STDCALL mysql-odbc-escape-string(MYSQL *mysql,
                                                   char *to,
                                                   unsigned long to-length,
                                                   const char *from,
                                                   unsigned long from-length,
                                                   void *param,
                                                   char *
                                                   (*extend-buffer)
                                                   (void *, char *to,
                                                         unsigned long *length)) ;
void 		STDCALL myodbc-remove-escape(MYSQL *mysql,char *name) ;
unsigned int	STDCALL mysql-thread-safe(void)                   ;

  
#define mysql-reload(mysql) mysql-refresh((mysql),REFRESH-GRANT)

