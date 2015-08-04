#!/bin/sh
#
# Copyright © 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright © 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright © 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright © 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright © 2008-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright © 2014 Inria.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

srcfile="$1"
option="$2"

if test -z "$srcfile"; then
    option="--help"
else
    : ${srcdir=.}

    if test -f "$srcfile"; then
        ompi_vers=`sed -n "
	t clear
	: clear
	s/^major/PMIX_MAJOR_VERSION/
	s/^minor/PMIX_MINOR_VERSION/
	s/^release/PMIX_RELEASE_VERSION/
	s/^greek/PMIX_GREEK_VERSION/
	s/\\\${major}/\\\${PMIX_MAJOR_VERSION}/
	s/\\\${minor}/\\\${PMIX_MINOR_VERSION}/
	s/\\\${release}/\\\${PMIX_RELEASE_VERSION}/
	s/\\\${greek}/\\\${PMIX_GREEK_VERSION}/
	s/^date/PMIX_RELEASE_DATE/
	s/^snapshot_version/PMIX_SNAPSHOT_VERSION/
	s/^snapshot/PMIX_SNAPSHOT/
	t print
	b
	: print
	p" < "$srcfile"`
	eval "$ompi_vers"

        PMIX_VERSION="$PMIX_MAJOR_VERSION.$PMIX_MINOR_VERSION.$PMIX_RELEASE_VERSION${PMIX_GREEK_VERSION}"

        # If PMIX_SNAPSHOT=1, then use PMIX_SNAPSHOT_VERSION
        if test "$PMIX_SNAPSHOT" = "1"; then
            # First, verify that PMIX_SNAPSHOT_VERSION isn't empty.
            if test -z "$PMIX_SNAPSHOT_VERSION"; then
                echo "*** ERROR: $1 contains snapshot=1, but an empty value for snapshot_version" 1>&2
                exit 1
            fi
            PMIX_VERSION=$PMIX_SNAPSHOT_VERSION
        fi
    fi

    if test "$option" = ""; then
	option="--version"
    fi
fi

case "$option" in
    --version)
	echo $PMIX_VERSION
	;;
    --release-date)
        echo $PMIX_RELEASE_DATE
        ;;
    --snapshot)
        echo $PMIX_SNAPSHOT
        ;;
    -h|--help)
	cat <<EOF
$0 <srcfile> <option>

<srcfile> - Text version file
<option>  - One of:
    --version      - Show version number
    --release-date - Show the release date
    --snapshot     - Show whether this is a snapshot release or not
    --help         - This message
EOF
        ;;
    *)
        echo "Unrecognized option $option.  Run $0 --help for options"
        ;;
esac

# All done

exit 0
