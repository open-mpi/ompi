#!/bin/sh
#
# $HEADER$
#
# Since we do this in multiple places, it's worth putting in a
# separate shell script.  Very primitive script to get the version
# number of LAM into a coherent variable.  Can query for any of the
# individual parts of the version number, too.
#

srcfile="$1"
option="$2"

if test "$srcfile" = ""; then
    option="--help"
else
    LAM_MAJOR_VERSION="`cat $srcfile | grep major | cut -d= -f2`"
    LAM_MINOR_VERSION="`cat $srcfile | grep minor | cut -d= -f2`"
    LAM_RELEASE_VERSION="`cat $srcfile | grep release | cut -d= -f2`"
    LAM_ALPHA_VERSION="`cat $srcfile | grep alpha | cut -d= -f2`"
    LAM_BETA_VERSION="`cat $srcfile | grep beta | cut -d= -f2`"
    LAM_SVN_VERSION="`cat $srcfile | grep svn | cut -d= -f2`"
    if test "$LAM_RELEASE_VERSION" != "0" -a "$LAM_RELEASE_VERSION" != ""; then
	LAM_VERSION="$LAM_MAJOR_VERSION.$LAM_MINOR_VERSION.$LAM_RELEASE_VERSION"
    else
	LAM_VERSION="$LAM_MAJOR_VERSION.$LAM_MINOR_VERSION"
    fi

    if test "`expr $LAM_ALPHA_VERSION \> 0`" = "1"; then
	LAM_VERSION="${LAM_VERSION}a$LAM_ALPHA_VERSION"
    elif test "`expr $LAM_BETA_VERSION \> 0`" = "1"; then
	LAM_VERSION="${LAM_VERSION}b$LAM_BETA_VERSION"
    fi

    if test "$LAM_SVN_VERSION" = "1"; then
        LAM_VERSION="${LAM_VERSION}svn"
    if test "`expr $LAM_SVN_VERSION \> 0`" = "1"; then
        if test -d .svn; then
            ver="r`svn info . | grep Revision | cut -d\  -f 2`"
        else
            ver="svn`date '+%m%d%Y'`"
        fi
	LAM_VERSION="${LAM_VERSION}$ver"
    fi

    if test "$option" = ""; then
	option="--full"
	fi
fi

case "$option" in
    --full|-v|--version)
	echo $LAM_VERSION
	;;
    --major)
	echo $LAM_MAJOR_VERSION
	;;
    --minor)
	echo $LAM_MINOR_VERSION
	;;
    --release)
	echo $LAM_RELEASE_VERSION
	;;
    --alpha)
	echo $LAM_ALPHA_VERSION
	;;
    --beta)
	echo $LAM_BETA_VERSION
	;;
    --svn)
	echo $LAM_SVN_VERSION
	;;
    --all)
        echo ${LAM_VERSION}:${LAM_MAJOR_VERSION}:${LAM_MINOR_VERSION}:${LAM_RELEASE_VERSION}:${LAM_ALPHA_VERSION}:${LAM_BETA_VERSION}:${LAM_SVN_VERSION}
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
