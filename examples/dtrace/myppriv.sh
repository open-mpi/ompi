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

# run ppriv under a shell so you can get the privileges of the 
# process that mprun creates
ppriv $$
