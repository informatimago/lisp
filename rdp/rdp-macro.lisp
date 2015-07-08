;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rdp-macro.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    The defgrammar macro.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-24 <PJB> Extracted from rdp.lisp.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2015
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;**************************************************************************

(in-package "COM.INFORMATIMAGO.RDP")


(defmacro defgrammar (name &key terminals (scanner t) (skip-spaces t)
                      start rules 
                      (target-language :lisp) (trace nil))
  "
DO:     This macro generates a simple scanner and recursive decent parser 
        for the language described by this grammar.
        For each <non-terminal> in the grammar, a function named
        <name>/PARSE-<non-terminal> is generated, in addition to 
        functions SCAN-<name> and PARSE-<name>.
        The grammar structure is also generated for run-time
        in the global special variable <name>.


TERMINALS:

        A list describing the terminal, of form either:
             (terminal-name match-regexp) 
             (terminal-name match-regexp / exclude-regexp) 

        In the first form, if the match-regexp matches, left-anchored
        to the current position, then the corresponding terminal is
        recognized.

        In the second form, if the match-regexp matches, left-anchored
        to the current position, and the exclude-regexp DOES NOT
        match, left-anchored to the first position following the
        match, then the corresponding terminal is recognized.

        Note that terminals don't necessarily have a name since they
        may be written directly in the grammar rules as strings.


SCANNER:

        Can be either:
        - T            A scanner is generated.
        - NIL          No scanner is generated.
        - a class-name subclass of COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER
                       which is used to get tokens.

SKIP-SPACES:

        When false, the spaces are not eaten between token.  It's up
        to the grammar or the scanner to deal with them.  (Only when a
        scanner is generated with :scanner t).

START:

        The start symbol (non-terminal).

RULES:

        A list of grammar rules.


TARGET-LANGUAGE:

        Specifies the language into which the code is generated, as a
        keyword.  The actions must still be written as lisp
        expressions, but to generate the language specified.  There
        must be a set of methods specialized on this target-language
        keyword.

TRACE:

        When true, the parser functions are traced..


SYNTAX:

    (defgrammar <name>
        :terminals (( <terminal>       \"regexp\" [ / \"regexp\" ]) ...)
        :start        <non-terminal>
        :rules     ((--> <non-terminal> <items> ) ...))

    <items>            ::= | <item> <items>
    <item>             ::= <seq> | <rep> | <alt> | <opt>
                           | <non-terminal> | <literal-terminal> | <terminal>
    <seq>              ::= (SEQ <items> <action>) ; <items> may be empty.
    <rep>              ::= (REP <item> <items> <action>)
    <opt>              ::= (OPT <item> <items> <action>)
    <alt>              ::= (ALT <item> <items>)
    <action>           ::= | :ACTION <forms>
    <forms>            ::= | <form> <forms>
    <form>             ::= form        -- any lisp form.
    <non-terminal>     ::= symbol      -- any lisp symbol (keywords reserved).
    <terminal>         ::= symbol      -- any lisp symbol (keywords reserved).
    <literal-terminal> ::= string      -- any lisp string.

SEMANTICS:

        The terminals are either named terminals listed in the :TERMINALS
        clause, or literal terminals written directly in the productions as 
        lisp strings.  They are matched as-is.

        An extended regular expression regex(7) may be given that
        will be matched by the scanner to infer the given terminal.
        The literal terminals are matched first, the longest first,
        and with ([A-Za-z0-9]|$) appended to terminals ending in a
        letter or digit (so that they don't match when part of a
        longer identifier).
        Then the regular expressions are matched in the order given.

        A second regexp can be given for a terminal, which must not be
        matched, after the first regexp, for the terminal to be
        recognized.  (:terminals ((alpha \"[A-Za-z]+\" / \"[0-9]\")))
        will recognize \"abcd, efgh\" as the ALPHA \"abcd\", but
        won't recognize \"abcd42, efgh\" as an ALPHA.

        :START specifies the start non-terminal symbol.

        The non-terminal symbols are infered implicitely from the grammar rules.

        If there are more than one subforms, or an action,
        the REP and OPT forms take an implicit SEQ:
          (REP a b c :ACTION f)   -->  (REP (SEQ a b c :ACTION f))
          (OPT a b c :ACTION f)   -->  (OPT (SEQ a b c :ACTION f))
          (REP a :ACTION f)       -->  (REP (SEQ a :ACTION f))
          (OPT a :ACTION f)       -->  (OPT (SEQ a :ACTION f))
          (REP a)                 -->  (REP a)
          (OPT a)                 -->  (OPT a)

        Embedded ALT are flattened:
          (ALT a b (ALT c d) e f) --> (ALT a b c d e f)

        Actions are executed in a lexical environment where the
        symbols $1, $2, etc are bound to the results of the
        subforms. $0 is bound to the  list of the results of the
        subforms.  In addition, for each non-terminal the non-terminal
        symbol is bound to the result of the corresponding subform.
        When the non-terminal is present several times in the
        production, a dot and an index number is appended.

            (--> example
                 (seq thing item \",\" item (opt \",\" item
                                                :action (list item))
                      :action (list thing item.1 item.2 $5)))

            would build a list with the results of parsing thing, the
            two items, and the optional part.

        The action for REP is to return a possibly empty list of the
        results of its single subform repeated.  (REP is 0 or more).

        The action for OPT is to return either NIL, or the result of
        its single subform unchanged.

        The action for an ALT is to return the result of the selected 
        alternative unchanged.

        The default action for an internal SEQ is to return the list of the
        results of its subforms.

        The default action for an implicit SEQ for a given <non-terminal> lhs
        is to return the list of the results of its subforms prefixed by the
        <non-terminal> symbol.

TODO:   We could also flatten sequences without action, or even sequences with
        actions with renumbering.
"
  (dolist (terminal terminals)
    (assert (or (and (= 2 (length terminal))
                     (symbolp (first terminal))
                     (stringp (second terminal)))
                (and (= 4 (length terminal))
                     (symbolp (first terminal))
                     (stringp (second terminal))
                     (symbolp (third terminal))
                     (string= "/" (third terminal))
                     (stringp (fourth terminal))))
            (terminal)
            "Invalid terminal clause ~S.~%Should be (terminal \"regexp\") or (terminal \"regexp\" / \"regexp\")."
            terminal))
  (generate-grammar name
                    :terminals terminals
                    :scanner scanner
                    :skip-spaces skip-spaces
                    :start start
                    :rules rules
                    :target-language target-language
                    :trace trace))

;;;; THE END ;;;;
