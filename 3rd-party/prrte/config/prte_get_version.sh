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
# Copyright (c) 2008-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2024      Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#



# PRTE_GET_VERSION(version_file, variable_prefix)
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
    s/^major/PRTE_MAJOR_VERSION/
    s/^minor/PRTE_MINOR_VERSION/
    s/^release/PRTE_RELEASE_VERSION/
    s/^greek/PRTE_GREEK_VERSION/
    s/^repo_rev/PRTE_REPO_REV/
    s/^tarball_version/PRTE_TARBALL_VERSION/
    s/^date/PRTE_RELEASE_DATE/
    t print
    b
    : print
    p" < "$srcfile"`
    eval "$pmix_vers"

        PRTE_VERSION="$PRTE_MAJOR_VERSION.$PRTE_MINOR_VERSION.$PRTE_RELEASE_VERSION"
        PRTE_VERSION="${PRTE_VERSION}${PRTE_GREEK_VERSION}"

        if test "$PRTE_TARBALL_VERSION" = ""; then
            PRTE_TARBALL_VERSION=$PRTE_VERSION
        fi

        # If repo_rev was not set in the VERSION file, then get it now
        if test "$PRTE_REPO_REV" = ""; then
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
                    cd "$srcdir"
                    PRTE_REPO_REV=`git describe --tags --always`
                    cd "$git_save_dir"
                    unset git_save_dir
                else
                    PRTE_REPO_REV=`git describe --tags --always`
                fi
            else
                PRTE_REPO_REV=date`$srcdir/config/getdate.sh '+%Y-%m-%d'`
            fi
        fi


    fi


    if test "$option" = ""; then
    option="--full"
    fi
fi

case "$option" in
    --full|-v|--version)
    echo $PRTE_VERSION
    ;;
    --major)
    echo $PRTE_MAJOR_VERSION
    ;;
    --minor)
    echo $PRTE_MINOR_VERSION
    ;;
    --release)
    echo $PRTE_RELEASE_VERSION
    ;;
    --greek)
    echo $PRTE_GREEK_VERSION
    ;;
    --repo-rev)
    echo $PRTE_REPO_REV
    ;;
    --tarball)
        echo $PRTE_TARBALL_VERSION
        ;;
    --release-date)
        echo $PRTE_RELEASE_DATE
        ;;
    --all)
        echo ${PRTE_VERSION} : ${PRTE_MAJOR_VERSION} : ${PRTE_MINOR_VERSION} : ${PRTE_RELEASE_VERSION} : ${PRTE_GREEK_VERSION} : ${PRTE_REPO_REV} : ${PRTE_TARBALL_VERSION}
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
