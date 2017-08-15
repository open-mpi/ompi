#!/bin/sh -f
#
# Copyright © 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright © 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright © 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright © 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright © 2010-2014   Inria.  All rights reserved.
# Copyright © 2009-2014 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

builddir="`pwd`"

srcdir=$1
cd "$srcdir"
srcdir=`pwd`
cd "$builddir"

distdir="$builddir/$2"
HWLOC_VERSION=$3

if test "$distdir" = ""; then
    echo "Must supply relative distdir as argv[2] -- aborting"
    exit 1
elif test "$HWLOC_VERSION" = ""; then
    echo "Must supply version as argv[1] -- aborting"
    exit 1
fi

#========================================================================

start=`date`
cat <<EOF

Creating hwloc distribution
In directory: `pwd`
Srcdir: $srcdir
Builddir: $builddir
Version: $HWLOC_VERSION
Started: $start

EOF

umask 022

if test ! -d "$distdir"; then
    echo "*** ERROR: dist dir does not exist"
    echo "*** ERROR:   $distdir"
    exit 1
fi

if test ! -d $srcdir/doc/doxygen-doc; then
    echo "*** The srcdir does not already have a doxygen-doc tree built."
    echo "*** hwloc's config/distscript.csh requires the docs to be built"
    echo "*** in the srcdir before executing 'make dist'."
    exit 1
fi

# Trivial helper function
doit() {
    echo $*
    eval $*
}

echo "*** Copying doxygen-doc tree to dist..."
echo "*** Directory: srcdir: $srcdir, distdir: $distdir, pwd: `pwd`"
doit mkdir -p $distdir/doc/doxygen-doc
doit chmod -R a=rwx $distdir/doc/doxygen-doc
doit rm -rf $distdir/doc/doxygen-doc

# We want to copy the entire directory tree to the distdir.  In some
# cases, doxygen-doc may be a sym link, so we want the copy to follow
# the sym links.  It's a bit of a portability nightmare, so try a few
# different ways...
# This seems to work on OS X and Linux (but not Solaris)
doit "tar c -C $srcdir -h -f - doc/doxygen-doc | tar x -C $distdir -f -"
if test ! -d $distdir/doc/doxygen-doc; then
    # This seems to work on Linux and Solaris
    doit cp -rpf $srcdir/doc/doxygen-doc/ $distdir/doc
fi
if test ! -d $distdir/doc/doxygen-doc; then
    # This seems to work on OS X (probably redundant, but we know it works)
    doit cp -rpf $srcdir/doc/doxygen-doc $distdir/doc
fi
# If we still failed, just error out
if test ! -d $distdir/doc/doxygen-doc; then
    echo "ERROR: Cannot seem to copy a directory to the distdir :-("
    exit 1
fi

echo "*** Copying new README"
ls -lf $distdir/README
doit cp -pf $srcdir/README $distdir

#########################################################
# VERY IMPORTANT: Now go into the new distribution tree #
#########################################################
cd "$distdir"
echo "*** Now in distdir: $distdir"

#
# Remove all the latex source files from the distribution tree (the
# PDFs are still there; we're just removing the latex source because
# some of the filenames get really, really long...).
#

echo "*** Removing latex source from dist tree"
doit rm -rf doc/doxygen-doc/latex

#
# All done
#

cat <<EOF
*** hwloc version $HWLOC_VERSION distribution created

Started: $start
Ended:   `date`

EOF
