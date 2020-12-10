#! /bin/sh
##
## Copyright (C) by Argonne National Laboratory
##     See COPYRIGHT in top-level directory
##

${AUTORECONF:-autoreconf} ${autoreconf_args:-"-vif"} -I confdb

echo "=== running autoreconf in 'mpl' ==="
(cd mpl && ${AUTORECONF:-autoreconf} ${autoreconf_args:-"-vif"}) || exit 1
