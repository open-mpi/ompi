#!/bin/sh
# Copyright (c) 2012      Mellanox Technologies, Inc.
#                         All rights reserved.


mydir=$(cd `dirname $0`;pwd)
rpmdir=$mydir/rpm-dist/`hostname`
rpmdist=$rpmdir/build

knem_tgz="$mydir/knem-0.9.7.tar.gz"

mkdir -p $rpmdir $rpmdist
rpmbuild --define="_rpmdir $rpmdir" --define="_srcrpmdir $rpmdir" --define="_sourcedir $rpmdist" --define="_specdir $rpmdist" --define="_builddir $rpmdist" -tb $knem_tgz
