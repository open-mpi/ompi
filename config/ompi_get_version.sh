#!/bin/sh
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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
# ompi_get_version.sh.  It is unfortunate that we have to duplicate code,
# but it is really the only what that I can think to do it.  :-( Hence,
# if you change the logic here for determining version numbers, YOU MUST
# ALSO CHANGE IT IN ompi_get_version.sh!!
#

srcfile="$1"
option="$2"

if test "$srcfile" = ""; then
    option="--help"
else
    OMPI_MAJOR_VERSION="`cat $srcfile | grep major | cut -d= -f2`"
    OMPI_MINOR_VERSION="`cat $srcfile | grep minor | cut -d= -f2`"
    OMPI_RELEASE_VERSION="`cat $srcfile | grep release | cut -d= -f2`"
    OMPI_ALPHA_VERSION="`cat $srcfile | grep alpha | cut -d= -f2`"
    OMPI_BETA_VERSION="`cat $srcfile | grep beta | cut -d= -f2`"
    OMPI_SVN_VERSION="`cat $srcfile | grep svn | cut -d= -f2`"
    if test "$OMPI_RELEASE_VERSION" != "0" -a "$OMPI_RELEASE_VERSION" != ""; then
	OMPI_VERSION="$OMPI_MAJOR_VERSION.$OMPI_MINOR_VERSION.$OMPI_RELEASE_VERSION"
    else
	OMPI_VERSION="$OMPI_MAJOR_VERSION.$OMPI_MINOR_VERSION"
    fi

    if test "`expr $OMPI_ALPHA_VERSION \> 0`" = "1"; then
	OMPI_VERSION="${OMPI_VERSION}a$OMPI_ALPHA_VERSION"
    elif test "`expr $OMPI_BETA_VERSION \> 0`" = "1"; then
	OMPI_VERSION="${OMPI_VERSION}b$OMPI_BETA_VERSION"
    fi

    if test "$OMPI_SVN_VERSION" != "0"; then
        if test -d .svn; then
            ver="r`svnversion .`"
        else
            ver="svn`date '+%m%d%Y'`"
        fi
        OMPI_SVN_VERSION="$ver"
	OMPI_VERSION="${OMPI_VERSION}$ver"
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
    --alpha)
	echo $OMPI_ALPHA_VERSION
	;;
    --beta)
	echo $OMPI_BETA_VERSION
	;;
    --svn)
	echo $OMPI_SVN_VERSION
	;;
    --all)
        echo ${OMPI_VERSION} ${OMPI_MAJOR_VERSION} ${OMPI_MINOR_VERSION} ${OMPI_RELEASE_VERSION} ${OMPI_ALPHA_VERSION} ${OMPI_BETA_VERSION} ${OMPI_SVN_VERSION}
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
    --alpha   - Alpha version number
    --beta    - Beta version nmumber
    --svn     - Subversion repository number
    --all     - Show all version numbers, separated by :
    --help    - This message
EOF
esac

# All done

exit 0
