#!/bin/sh
#
# Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
#                         Use is subject to license terms.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dtrace -s $1 -c $2 -o $2.$OMPI_MCA_ns_nds_vpid.trace
