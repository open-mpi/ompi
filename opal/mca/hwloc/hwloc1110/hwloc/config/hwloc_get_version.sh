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
	s/^major/HWLOC_MAJOR_VERSION/
	s/^minor/HWLOC_MINOR_VERSION/
	s/^release/HWLOC_RELEASE_VERSION/
	s/^greek/HWLOC_GREEK_VERSION/
	s/\\\${major}/\\\${HWLOC_MAJOR_VERSION}/
	s/\\\${minor}/\\\${HWLOC_MINOR_VERSION}/
	s/\\\${release}/\\\${HWLOC_RELEASE_VERSION}/
	s/\\\${greek}/\\\${HWLOC_GREEK_VERSION}/
	s/^date/HWLOC_RELEASE_DATE/
	s/^snapshot_version/HWLOC_SNAPSHOT_VERSION/
	s/^snapshot/HWLOC_SNAPSHOT/
	t print
	b
	: print
	p" < "$srcfile"`
	eval "$ompi_vers"

        HWLOC_VERSION="$HWLOC_MAJOR_VERSION.$HWLOC_MINOR_VERSION.$HWLOC_RELEASE_VERSION${HWLOC_GREEK_VERSION}"

        # If HWLOC_SNAPSHOT=1, then use HWLOC_SNAPSHOT_VERSION
        if test "$HWLOC_SNAPSHOT" = "1"; then
            # First, verify that HWLOC_SNAPSHOT_VERSION isn't empty.
            if test -z "$HWLOC_SNAPSHOT_VERSION"; then
                echo "*** ERROR: $1 contains snapshot=1, but an empty value for snapshot_version" 1>&2
                exit 1
            fi
            HWLOC_VERSION=$HWLOC_SNAPSHOT_VERSION
        fi
    fi

    if test "$option" = ""; then
	option="--version"
    fi
fi

case "$option" in
    --version)
	echo $HWLOC_VERSION
	;;
    --release-date)
        echo $HWLOC_RELEASE_DATE
        ;;
    --snapshot)
        echo $HWLOC_SNAPSHOT
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
