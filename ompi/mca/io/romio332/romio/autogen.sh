#!/bin/sh

${AUTORECONF:-autoreconf} ${autoreconf_args:-"-vif"} -I confdb

echo "=== running autoreconf in 'mpl' ==="
(cd mpl && ${AUTORECONF:-autoreconf} ${autoreconf_args:-"-vif"}) || exit 1
