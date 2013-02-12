#!/bin/bash
# Copyright (c) 2012      Mellanox Technologies, Inc.
#                         All rights reserved.

rpmname="$1"
if test "$rpmname" = ""; then
    echo "Usage: buildtarball.sh <rpm>"
    exit 1
fi
if test ! -f $rpmname; then
    echo "Can't find $rpmname"
    exit 1
fi
echo "--> Found rpm: $rpmname"

version="`rpm -qp $rpmname | sed s/openshmem-//`"
mkdir -p tarballs
rpm2cpio $rpmname | cpio -id
cd opt/mellanox
tar jcvf ../../tarballs/openshmem-$version.tar.bz openshmem
cd ../..
rm -rf opt/
