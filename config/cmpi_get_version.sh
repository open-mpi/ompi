#!/bin/sh
#
# Copyright (c) 2003 The Trustees of Indiana University.  
#                    All rights reserved.
# 
# This file is part of the CMMPI software package.  For license
# information, see the LICENSE file in the top level directory of the
# CMPI source distribution.
#
# $Id: cmpi_get_version.sh,v 1.1 2003/11/22 16:36:20 jsquyres Exp $
#
# Since we do this in multiple places, it's worth putting in a
# separate shell script.  Very primitive script to get the version
# number of CMPI into a coherent variable.  Can query for any of the
# individual parts of the version number, too.
#

srcfile="$1"
option="$2"

if test "$srcfile" = ""; then
    option="--help"
else
    CMPI_MAJOR_VERSION="`cat $srcfile | grep major | cut -d= -f2`"
    CMPI_MINOR_VERSION="`cat $srcfile | grep minor | cut -d= -f2`"
    CMPI_RELEASE_VERSION="`cat $srcfile | grep release | cut -d= -f2`"
    CMPI_ALPHA_VERSION="`cat $srcfile | grep alpha | cut -d= -f2`"
    CMPI_BETA_VERSION="`cat $srcfile | grep beta | cut -d= -f2`"
    CMPI_CVS_VERSION="`cat $srcfile | grep cvs | cut -d= -f2`"
    if test "$CMPI_RELEASE_VERSION" != "0" -a "$CMPI_RELEASE_VERSION" != ""; then
	CMPI_VERSION="$CMPI_MAJOR_VERSION.$CMPI_MINOR_VERSION.$CMPI_RELEASE_VERSION"
    else
	CMPI_VERSION="$CMPI_MAJOR_VERSION.$CMPI_MINOR_VERSION"
    fi

    if test "`expr $CMPI_ALPHA_VERSION \> 0`" = "1"; then
	CMPI_VERSION="${CMPI_VERSION}a$CMPI_ALPHA_VERSION"
    elif test "`expr $CMPI_BETA_VERSION \> 0`" = "1"; then
	CMPI_VERSION="${CMPI_VERSION}b$CMPI_BETA_VERSION"
    fi

    if test "$CMPI_CVS_VERSION" = "1"; then
	CMPI_VERSION="${CMPI_VERSION}cvs"
    elif test "`expr $CMPI_CVS_VERSION \> 0`" = "1"; then
	CMPI_VERSION="${CMPI_VERSION}cvs$CMPI_CVS_VERSION"
    fi

    if test "$option" = ""; then
	option="--full"
	fi
fi

case "$option" in
    --full|-v|--version)
	echo $CMPI_VERSION
	;;
    --major)
	echo $CMPI_MAJOR_VERSION
	;;
    --minor)
	echo $CMPI_MINOR_VERSION
	;;
    --release)
	echo $CMPI_RELEASE_VERSION
	;;
    --alpha)
	echo $CMPI_ALPHA_VERSION
	;;
    --beta)
	echo $CMPI_BETA_VERSION
	;;
    --cvs)
	echo $CMPI_CVS_VERSION
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
    --cvs     - CVS date stamp
    --help    - This message
EOF
esac

# All done

exit 0
