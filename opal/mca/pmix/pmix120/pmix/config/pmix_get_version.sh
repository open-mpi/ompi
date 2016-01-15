#!/bin/sh
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Intel, Inc. All rights reserved
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#



# PMIX_GET_VERSION(version_file, variable_prefix)
# -----------------------------------------------
# parse version_file for version information, setting
# the following shell variables:
#
#  prefix_VERSION
#  prefix_BASE_VERSION
#  prefix_MAJOR_VERSION
#  prefix_MINOR_VERSION
#  prefix_RELEASE_VERSION
#  prefix_GREEK_VERSION
#  prefix_REPO_REV
#  prefix_TARBALL_VERSION
#  prefix_RELEASE_DATE



srcfile="$1"
option="$2"

if test -z "$srcfile"; then
    option="--help"
else

        if test -f "$srcfile"; then
        srcdir=`dirname $srcfile`
        pmix_vers=`sed -n "
    t clear
    : clear
    s/^major/PMIX_MAJOR_VERSION/
    s/^minor/PMIX_MINOR_VERSION/
    s/^release/PMIX_RELEASE_VERSION/
    s/^greek/PMIX_GREEK_VERSION/
    s/^repo_rev/PMIX_REPO_REV/
    s/^tarball_version/PMIX_TARBALL_VERSION/
    s/^date/PMIX_RELEASE_DATE/
    t print
    b
    : print
    p" < "$srcfile"`
    eval "$pmix_vers"

        PMIX_VERSION="$PMIX_MAJOR_VERSION.$PMIX_MINOR_VERSION.$PMIX_RELEASE_VERSION"
        PMIX_VERSION="${PMIX_VERSION}${PMIX_GREEK_VERSION}"

        if test "$PMIX_TARBALL_VERSION" = ""; then
            PMIX_TARBALL_VERSION=$PMIX_VERSION
        fi

        # If repo_rev was not set in the VERSION file, then get it now
        if test "$PMIX_REPO_REV" = ""; then
            # See if we can find the "git" command.
            git_happy=0
            git --version > /dev/null 2>&1
            if test $? -eq 0; then
                git_happy=1
            fi

            # If we're in a git repo and we found the git command, use
            # git describe to get the repo rev
            if test -d "$srcdir/.git" && test $git_happy -eq 1; then
                if test "$srcdir" != "`pwd`"; then
                    git_save_dir=`pwd`
                    cd $srcdir
                    PMIX_REPO_REV=`git describe --tags --always`
                    cd $git_save_dir
                    unset git_save_dir
                else
                    PMIX_REPO_REV=`git describe --tags --always`
                fi
            else
                PMIX_REPO_REV="date`date '+%Y-%m-%d'`"
            fi
        fi


    fi


    if test "$option" = ""; then
    option="--full"
    fi
fi

case "$option" in
    --full|-v|--version)
    echo $PMIX_VERSION
    ;;
    --major)
    echo $PMIX_MAJOR_VERSION
    ;;
    --minor)
    echo $PMIX_MINOR_VERSION
    ;;
    --release)
    echo $PMIX_RELEASE_VERSION
    ;;
    --greek)
    echo $PMIX_GREEK_VERSION
    ;;
    --repo-rev)
    echo $PMIX_REPO_REV
    ;;
    --tarball)
        echo $PMIX_TARBALL_VERSION
        ;;
    --release-date)
        echo $PMIX_RELEASE_DATE
        ;;
    --all)
        echo ${PMIX_VERSION} : ${PMIX_MAJOR_VERSION} : ${PMIX_MINOR_VERSION} : ${PMIX_RELEASE_VERSION} : ${PMIX_GREEK_VERSION} : ${PMIX_REPO_REV} : ${PMIX_TARBALL_VERSION}
        ;;
    -h|--help)
    cat <<EOF
$0 <srcfile> <option>

<srcfile> - Text version file
<option>  - One of:
    --full         - Full version number
    --major        - Major version number
    --minor        - Minor version number
    --release      - Release version number
    --greek        - Greek (alpha, beta, etc) version number
    --repo-rev     - Repository version
    --tarball      - Show tarball filename version string
    --all          - Show all version numbers, separated by :
    --release-date - Show the release date
    --help         - This message
EOF
        ;;
    *)
        echo "Unrecognized option $option.  Run $0 --help for options"
        ;;
esac

# All done

exit 0
