# Informatimago Public Common Lisp Libraries

A collection of Common Lisp libraries by Pascal J. Bourguignon: portable
utilities and data structures, parsers and a recursive-descent parser
generator, a portable Common Lisp reader, file-format and protocol code,
POSIX bindings, and many small programs. Most of it is portable ANSI
Common Lisp. The systems build with ASDF and are distributed through
[Ultralisp](https://ultralisp.org/) (see *Repository* below).

The library is organized in the following directories.

common-lisp/

> The sublibrary systems provided by this library should contain only
> pure conformant Common Lisp packages.
>
> They should compile and run in all Common Lisp compliant
> implementations, and should have make use of no external package (eg.
> compatibility library) and no other (implementation dependant) package
> than COMMON-LISP. They should not use \#+/#- to activate or disable
> implementation specific code. \[There remains some packages using
> \#+/#- with implementations specific variants, we're working on
> removing these forms\].

clext/

> Common Lisp Extensions.
>
> This directory contains Common-Lisp packages that are mostly portable,
> but that use some extensions, packages out of the Common-Lisp
> specifications, like GRAY or other portability libraries.

clmisc/

> Miscellaneous Common Lisp packages.

clisp/

> Packages specific to the CLISP implementation.

susv3/

> POSIX (SUSv3) API bindings, currently for CLISP only.

tools/

> Build helpers from before ASDF, kept for reference.

rdp/

> Simple Recursive-Descent Parser.

objcl/

> Objective-CL reader macros.

small-cl-pgms/

> Various small lisp programs and proof-of-concept demos. Those are not
> gathered with ASDF system definitions.

# Repository

These libraries can be obtained from the
<span class="title-ref">git</span> repository at
<https://gitlab.com/com-informatimago/com-informatimago/> :

    git clone https://gitlab.com/com-informatimago/com-informatimago.git informatimago

They're also available thru [ultralisp](https://ultralisp.org/): After
following the ultralisp install instructions, execute: :

    (ql:quickload :com.informatimago.common-lisp)
    (ql:quickload :com.informatimago.clext)
    (ql:quickload :com.informatimago.clmisc)
    #+clisp (ql:quickload :com.informatimago.clisp)
    #+clisp (ql:quickload :com.informatimago.susv3)
    (ql:quickload :com.informatimago.rdp)
    #+(and ccl darwin) (ql:quickload :com.informatimago.objcl)

    (ql:quickload :com.informatimago)

# Documentation

You may [browse the
documentation](http://www.informatimago.com/develop/lisp/doc/), or you
may also [browse the sources at
gitlab](https://gitlab.com/com-informatimago/com-informatimago/tree/master).

# Bug Reports & Patches

Bug reports may be [posted on gitlab
issues](https://gitlab.com/com-informatimago/com-informatimago/issues),
and [merge requests for patches on
gitlab](https://gitlab.com/com-informatimago/com-informatimago/merge_requests).
Alternatively, they may be sent to [the
author](mailto:pjb@informatimago.com).

# Authors & License

All this code is authored by Pascal J. Bourguignon, apart from:

- the system
  <span class="title-ref">com.informatimago.lisp-reader.package</span>
  which is authored by Zach Beane and modified by Pascal J. Bourguignon.
- the system <span class="title-ref">com.informatimago.lispdoc</span>
  which is authored by Sven Van Caekenberghe and modified by Pascal J.
  Bourguignon.

All this code is released under the [GNU AFFERO GENERAL PUBLIC
LICENSE](http://www.gnu.org/licenses/agpl-3.0.html) apart from:

- the system
  <span class="title-ref">com.informatimago.lisp-reader.package</span>
  which is released under the [BSD 2-clause
  license](https://tldrlegal.com/license/bsd-2-clause-license-%28freebsd%29#fulltext).
  The original parts of the
  <span class="title-ref">com.informatimago.lisp-reader.package</span>
  system are: Copyright (c) 2012 Zachary Beane \<xach@xach.com\>, All
  Rights Reserved
- the <span class="title-ref">system com.informatimago.lispdoc</span>
  which is released under the [LLGPL
  license](https://tldrlegal.com/license/lisp-lesser-general-public-license#fulltext).

> The original web site referenced from
> [cliki](http://www.cliki.net/LispDoc) seems to have disappared (dead
> links), so a tarball containing a copy of the original sources is
> included in the git repository.

# References

- AGPL: <http://www.gnu.org/licenses/agpl-3.0.html>
- Common Lisp: <http://en.wikipedia.org/wiki/Common_lisp>
- Common Lisp Wiki: <http://cliki.net>
- Pascal J. Bourguignon:
  [mailto:pjb@informatimago.com](mailto:pjb@informatimago.com)
  <http://www.informatimago.com/>
- Ultralisp: <https://ultralisp.org/>
- Zach Beane: <http://xach.com/>
- Advantages of monolithic version control
  <https://danluu.com/monorepo/>

# TODO

- create a library for low level utilities (eg. proper-list-p) that are
  used by several libraries to break circular dependencies.
