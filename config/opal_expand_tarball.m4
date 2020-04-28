dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2020      Amazon.com, Inc. or its affiliates.  All Rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl OPAL_EXPAND_TARBALL(tarball name, expansion path, test filename)
dnl
dnl <tarball name>        path to the tarball to expand, relative to
dnl                       the source directory.
dnl <expansion path>      path relative to the current directory, in
dnl                       which the tarball should be expanded.
dnl <test filename>       a unique filename in the tarball to test for
dnl                       existance.
dnl
dnl If <build dir>/<expansion path>/<test filename> is not readable,
dnl then expand the tarball <src dir>/<tarball name> in the directory
dnl basename(<expansion path>).

AC_DEFUN([OPAL_EXPAND_TARBALL_PREREQ], [
    AC_PATH_PROG(GZIP_BIN, [gzip], [])
    AC_PATH_PROG(BZIP2_BIN, [bzip2], [])
])

AC_DEFUN([OPAL_EXPAND_TARBALL], [
    AC_REQUIRE([OPAL_EXPAND_TARBALL_PREREQ])

    OPAL_VAR_SCOPE_PUSH([expansion_dir expansion_tarball])

    expansion_dir=`dirname "$2"`
    AS_MKDIR_P([$expansion_dir])

    expansion_tarball="$ac_abs_confdir/$1"
    AS_IF([test ! -r $2/$3], [
        AC_MSG_NOTICE([Expanding $srcdir/$1 in $expansion_dir])
        case "$1" in
            *.tar.gz|*.tgz)
                AS_IF([test -z "$GZIP_BIN"], [AC_MSG_ERROR([gzip not found])])
                (cd "$expansion_dir" ; umask 0022 ; "$GZIP_BIN" -d < "$expansion_tarball" | tar xf - || AC_MSG_ERROR([failed to extract $1]))
                ;;
            *.tar.bz2|*.tbz2)
                AS_IF([test -z "$BZIP2_BIN"], [AC_MSG_ERROR([bzip2 not found])])
                (cd "$expansion_dir" ; umask 0022 ; "$BZIP_BIN" -d < "$expansion_tarball" | tar xf - || AC_MSG_ERROR([failed to extract $1]))
                ;;
            *)  # maybe tar is smart?
                (cd "$expansion_dir" ; umask 0022 ; tar xf "$expansion_tarball" || AC_MSG_ERROR([failed to extract $1]))
                ;;
        esac])

    OPAL_VAR_SCOPE_POP
])
