Informatimago Public Common Lisp Libraries
==========================================

common-lisp/

> The sublibrary systems provided by this library should contain only pure conformant Common Lisp packages.
>
> They should compile and run in all Common Lisp compliant implementations, and should have make use of no external package (eg. compatibility library) and no other (implementation dependant) package than COMMON-LISP. They should not use \#+/\#- to activate or disable implementation specific code. [There remains some packages using \#+/\#- with implementations specific variants, we're working on removing these forms].

clext/

> Common Lisp Extensions.
>
> This directory contains Common-Lisp packages that are mostly portable, but that use some extensions, packages out of the Common-Lisp specifications, like GRAY or other portability libraries.

clmisc/

> Miscellaneous Common Lisp packages.

clisp/

> clisp specific packages.

susv3/

> POSIX API (clisp specific for now).

tools/

> Various tools to help developing and compiling these packages. (Mostly obsolete since the introduction of ASDF and Quicklisp).

rdp/

> Simple Recursive-Descent Parser.

objcl/

> Objective-CL reader macros.

small-cl-pgms/

> Various small lisp programs and proof-of-concept demos. Those are not gathered with ASDF system definitions.

Repository
==========

These libraries can be obtained from the git repository at <https://gitlab.com/com-informatimago/com-informatimago/> :

    git clone https://gitlab.com/com-informatimago/com-informatimago.git informatimago

They're also available thru [quicklisp](http://quicklisp.org/): :

    (ql:quickload :com.informatimago.common-lisp)
    (ql:quickload :com.informatimago.clext)
    (ql:quickload :com.informatimago.clmisc)
    #+clisp (ql:quickload :com.informatimago.clisp)
    #+clisp (ql:quickload :com.informatimago.susv3)
    (ql:quickload :com.informatimago.rdp)
    #+(and ccl darwin) (ql:quickload :com.informatimago.objcl)

    (ql:quickload :com.informatimago)

Documentation
=============

You may [browse the documentation](doc/), or you may also [browse the sources at gitlab](https://gitlab.com/com-informatimago/com-informatimago/tree/master).

Bug Reports & Patches
=====================

Bug reports may be posted on gitlab issues: [](https://gitlab.com/com-informatimago/com-informatimago/issues), and merge requests for patches: [](https://gitlab.com/com-informatimago/com-informatimago/merge_requests). Alternatively, they may be sent to [](mailto:pjb@informatimago.com).

Authors & License
=================

All this code is authored by Pascal J. Bourguignon, apart from:

-   the system com.informatimago.lisp-reader.package which is authored by Zach Beane and modified by Pascal J. Bourguignon.
-   the system com.informatimago.lispdoc which is authored by Sven Van Caekenberghe and modified by Pascal J. Bourguignon.

All this code is released under the [GNU AFFERO GENERAL PUBLIC LICENSE](http://www.gnu.org/licenses/agpl-3.0.html) apart from:

-   the system com.informatimago.lisp-reader.package which is released under the [BSD 2-clause license](https://tldrlegal.com/license/bsd-2-clause-license-%28freebsd%29#fulltext). The original parts of the com.informatimago.lisp-reader.package system are: Copyright (c) 2012 Zachary Beane \<<xach@xach.com>\>, All Rights Reserved
-   the system com.informatimago.lispdoc which is released under the [LLGPL license](https://tldrlegal.com/license/lisp-lesser-general-public-license#fulltext).

> The original web site referenced from [cliki](http://www.cliki.net/LispDoc) seems to have disappared (dead links), so a tarball containing a copy of the original sources is included in the git repository.

References
==========

-   AGPL: [](http://www.gnu.org/licenses/agpl-3.0.html)
-   Common Lisp: [](http://en.wikipedia.org/wiki/Common_lisp)
-   Common Lisp Wiki: [](http://cliki.net)
-   Pascal J. Bourguignon: [](mailto:pjb@informatimago.com) [](http://www.informatimago.com/)
-   Quicklisp: [](http://quicklisp.org/)
-   Zach Beane: [](http://xach.com/)

TODO
====

-   create a library for low level utilities (eg. proper-list-p) that are used by several libraries to break circular dependencies.

