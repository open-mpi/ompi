#!/bin/sh
#
# Copyright (c) 2006-2008 Sun Microsystems, Inc.  All rights reserved.
#                         Use is subject to license terms.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#

dtrace -s $1 -c $2 -o $2.$OMPI_COMM_WORLD_RANK.trace
