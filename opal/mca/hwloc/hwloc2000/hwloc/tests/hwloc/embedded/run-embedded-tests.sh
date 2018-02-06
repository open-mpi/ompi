#!/bin/bash
#
# Copyright © 2010 Cisco Systems, Inc.  All rights reserved.
# See COPYING in top-level directory.
#
# Simple script to help test embedding:
#
#     ./run-embedded-tests.sh <tarball_name>
#

set -o pipefail

tarball=$1
if test "$tarball" = ""; then
    echo "Usage: $0 <tarball_name>"
    exit 1
elif test ! -r $tarball; then
    echo cannot read tarball: $tarball
    exit 1
fi

#---------------------------------------------------------------------

i=1
last_print=
print() {
    last_print="=== $i: $*"
    echo $last_print
    i=`expr $i + 1`
}

#---------------------------------------------------------------------

try() {
    cmd=$*
    eval $cmd
    status=$?
    if test "$status" != "0"; then
        echo "Command failed (status $status): $cmd"
        echo "Last print was: $last_print"
        exit 1
    fi
}

#---------------------------------------------------------------------

# $1 = announcement banner
# $2 = path to configure script
do_build() {
    print Running $1 configure...
    try $2/configure 2>&1 | tee config.out
    if test "$?" != 0; then exit $?; fi

    print Running make
    try make 2>&1 | tee make.out
    if test "$?" != 0; then exit $?; fi

    print Running make check
    try make check 2>&1 | tee check.out
    if test "$?" != 0; then exit $?; fi

    print Running make clean
    try make clean 2>&1 | tee clean.out
    if test "$?" != 0; then exit $?; fi

    print Running make distclean
    try make distclean 2>&1 | tee distclean.out
    if test "$?" != 0; then exit $?; fi
}

#---------------------------------------------------------------------

# Get tarball name
print Got tarball: $tarball

# Get the version
ver=`echo $tarball | sed -e 's/^.*hwloc-//' -e 's/\.tar\..*$//'`
print Got version: $ver

# Extract
print Extracting tarball...
rm -rf hwloc-$ver
if test "`echo $tarball | grep .tar.bz2`" != ""; then
    try tar jxf $tarball
else
    try tar zxf $tarball
fi

print Removing old tree...
rm -rf hwloc-tree
mv hwloc-$ver hwloc-tree

# Autogen
print Running autogen...
try ./autogen.sh

# Do it normally (non-VPATH)
do_build non-VPATH .

# Do a VPATH in the same tree that we just setup
mkdir build
cd build
do_build VPATH ..

cd ..
rm -rf build

# Now whack the tree and do a clean VPATH
print Re-extracting tarball...
rm -rf hwloc-$ver
if test "`echo $tarball | grep .tar.bz2`" != ""; then
    try tar jxf $tarball
else
    try tar zxf $tarball
fi

print Removing old tree...
rm -rf hwloc-tree
mv hwloc-$ver hwloc-tree

# Autogen
print Running autogen...
try ./autogen.sh

# Run it again on a clean VPATH
mkdir build
cd build
do_build VPATH-clean ..

cd ..
rm -rf build

print All tests passed!
