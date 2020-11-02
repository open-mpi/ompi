dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved.
dnl
dnl Copyright (c) 2020      Intel, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl
dnl Just in case someone looks for it here someday, here is a
dnl conveninent reference for what Markdown pandoc supports:
dnl
dnl https://rmarkdown.rstudio.com/authoring_pandoc_markdown.html
dnl

AC_DEFUN([PMIX_SETUP_MAN_PAGES],[
    AC_ARG_ENABLE(man-pages,
                  [AC_HELP_STRING([--disable-man-pages],
                                  [Do not generate/install man pages (default: enabled)])])

    PANDOC=
    PMIX_ENABLE_MAN_PAGES=0
    AC_MSG_CHECKING([if want man pages])
    AS_IF([test -z "$enable_man_pages" || test "$enable_man_pages" = "yes"],
          [AC_MSG_RESULT([yes])
	   PMIX_ENABLE_MAN_PAGES=1
	   _PMIX_SETUP_PANDOC],
	  [AC_MSG_RESULT([no])])

    AC_SUBST(PANDOC)
    AM_CONDITIONAL([PMIX_ENABLE_MAN_PAGES], [test $PMIX_ENABLE_MAN_PAGES -eq 1])
    AC_DEFINE_UNQUOTED([PMIX_ENABLE_MAN_PAGES], [$PMIX_ENABLE_MAN_PAGES],
                       [Whether or not we will build manpages])

    AS_IF([test $PMIX_ENABLE_MAN_PAGES -eq 1],
          [PMIX_SUMMARY_ADD([[Options]],[[Manpages built]], [pmix_manpages], [yes])],
          [PMIX_SUMMARY_ADD([[Options]],[[Manpages built]], [pmix_manpages], [yes])])

])

dnl Back-end pandoc setup
AC_DEFUN([_PMIX_SETUP_PANDOC],[
    PMIX_VAR_SCOPE_PUSH([min_major_version min_minor_version pandoc_version pandoc_major pandoc_minor])

    # If we need to generate man pages, we need pandoc >v1.12.
    AC_PATH_PROG([PANDOC], [pandoc])

    # If we found Pandoc, check its version.  We need >=v1.12.
    # To be clear: I know that v1.12 works, and I know that v1.9 does not
    # work.  I did not test the versions in between to know exactly what
    # the lowest version is that works.  Someone is free to update this
    # check someday to be more accurate if they wish.
    min_major_version=1
    min_minor_version=12
    AS_IF([test -n "$PANDOC"],
          [pandoc_version=`pandoc --version | head -n 1 | awk '{ print $[2] }'`
           pandoc_major=`echo $pandoc_version | cut -d\. -f1`
           pandoc_minor=`echo $pandoc_version | cut -d\. -f2`
           AC_MSG_CHECKING([pandoc version])
           AC_MSG_RESULT([major: $pandoc_major, minor: $pandoc_minor])

           AC_MSG_CHECKING([if pandoc version is >=v$min_major_version.$min_minor_version])
           AS_IF([test $pandoc_major -lt $min_major_version], [PANDOC=])
           AS_IF([test $pandoc_major -eq $min_major_version && test $pandoc_minor -lt $min_minor_version],
                 [PANDOC=])
           AS_IF([test -n "$PANDOC"],
                 [AC_MSG_RESULT([yes])],
                 [AC_MSG_RESULT([no])])
          ])

    AS_IF([test -z "$PANDOC" || test -n "`echo $PANDOC | $GREP missing`"],
          [AS_IF([test "$PMIX_DEVEL" = "1" && test -z "$enable_man_pages"],
                 [AC_MSG_CHECKING([man pages will be built])
                  AC_MSG_RESULT([no - adequate pandoc installation not found])
                  PANDOC=
                  PMIX_ENABLE_MAN_PAGES=0],
                 [AS_IF([test ! -f "$srcdir/tools/wrapper/pmix_wrapper.1"],
                         [AC_MSG_WARN([*** Could not find a suitable pandoc on your system.])
                          AC_MSG_WARN([*** You need pandoc >=$min_major_version.$min_minor_version to build OpenPMIx man pages.])
                          AC_MSG_WARN([*** See pandoc.org.])
                          AC_MSG_WARN([*** NOTE: If you are building from a tarball downloaded from the OpenPMIx GitHub repository, you do not need Pandoc])
                          AC_MSG_ERROR([Cannot continue])
                         ])])
           ])

    PMIX_VAR_SCOPE_POP
])
