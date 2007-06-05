#!/bin/sh
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

#
# This file is almost identical in functionality to
# ompi_get_version.m4.  It is unfortunate that we have to duplicate
# code, but it is really the only what that I can think to do it.  :-(
# Hence, if you change the logic here for determining version numbers,
# YOU MUST ALSO CHANGE IT IN ompi_get_version.m4!!
#

srcfile="$1"
option="$2"

case "$option" in
    # svnversion can take a while to run.  If we don't need it, don't run it.
    --major|--minor|--release|--greek|--base|--help)
        OMPI_NEED_SVN=0
        ;;
    *)
        OMPI_NEED_SVN=1
esac


if test "$srcfile" = ""; then
    option="--help"
else
    OMPI_MAJOR_VERSION="`cat $srcfile | egrep '^major=' | cut -d= -f2`"
    OMPI_MINOR_VERSION="`cat $srcfile | egrep '^minor=' | cut -d= -f2`"
    OMPI_RELEASE_VERSION="`cat $srcfile | egrep '^release=' | cut -d= -f2`"
    OMPI_GREEK_VERSION="`cat $srcfile | egrep '^greek=' | cut -d= -f2`"
    OMPI_WANT_SVN="`cat $srcfile | egrep '^want_svn=' | cut -d= -f2`"
    OMPI_SVN_R="`cat $srcfile | egrep '^svn_r=' | cut -d= -f2`"
    if test "$OMPI_RELEASE_VERSION" != "0" -a "$OMPI_RELEASE_VERSION" != ""; then
	OMPI_VERSION="$OMPI_MAJOR_VERSION.$OMPI_MINOR_VERSION.$OMPI_RELEASE_VERSION"
    else
	OMPI_VERSION="$OMPI_MAJOR_VERSION.$OMPI_MINOR_VERSION"
    fi

    OMPI_VERSION="${OMPI_VERSION}${OMPI_GREEK_VERSION}"

    OMPI_BASE_VERSION="$OMPI_VERSION"

    if test "$OMPI_WANT_SVN" = "1" -a "$OMPI_NEED_SVN" = "1" ; then
        if test "$OMPI_SVN_R" = "-1"; then
            if test -d .svn; then
                ver="r`svnversion .`"
            else
                ver="svn`date '+%m%d%Y'`"
            fi
            OMPI_SVN_R="$ver"
        fi
	OMPI_VERSION="${OMPI_VERSION}$OMPI_SVN_R"
    fi

    if test "$option" = ""; then
	option="--full"
    fi
fi

case "$option" in
    --full|-v|--version)
	echo $OMPI_VERSION
	;;
    --major)
	echo $OMPI_MAJOR_VERSION
	;;
    --minor)
	echo $OMPI_MINOR_VERSION
	;;
    --release)
	echo $OMPI_RELEASE_VERSION
	;;
    --greek)
	echo $OMPI_GREEK_VERSION
	;;
    --svn)
	echo $OMPI_SVN_R
	;;
    --base)
        echo $OMPI_BASE_VERSION
        ;;
    --all)
        echo ${OMPI_VERSION} ${OMPI_MAJOR_VERSION} ${OMPI_MINOR_VERSION} ${OMPI_RELEASE_VERSION} ${OMPI_GREEK_VERSION} ${OMPI_SVN_R}
        ;;
    -h|--help)
	cat <<EOF
$0 <srcfile> [<option>]

<srcfile> - Text version file
<option>  - One of:
    --full    - Full version number
    --major   - Major version number
    --minor   - Minor version number
    --release - Release version number
    --greek   - Greek (alpha, beta, etc) version number
    --svn     - Subversion repository number
    --all     - Show all version numbers, separated by :
    --base    - Show base version number (no svn number)
    --help    - This message
EOF
        ;;
    *)
        echo "Unrecognized option $option.  Run $0 --help for options"
        ;;
esac

# All done

exit 0
