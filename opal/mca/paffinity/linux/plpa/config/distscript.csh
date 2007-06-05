#! /bin/csh -f
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

set srcdir="$1"
set builddir="`pwd`"
set distdir="$builddir/$2"
set PLPA_VERSION="$3"
set PLPA_SVN_VERSION="$4"

if ("$distdir" == "") then
    echo "Must supply relative distdir as argv[2] -- aborting"
    exit 1
elif ("$PLPA_VERSION" == "") then
    echo "Must supply version as argv[1] -- aborting"
    exit 1
endif

# we can catch some hard (but possible) to do mistakes by looking at
# our tree's revision number, but only if we are in the source tree.
# Otherwise, use what configure told us, at the cost of allowing one
# or two corner cases in (but otherwise VPATH builds won't work)
set svn_r=$PLPA_SVN_VERSION
if (-d .svn) then
    set svn_r="r`svnversion .`"
endif

set start=`date`
cat <<EOF
 
Creating PLPA distribution
In directory: `pwd`
Version: $PLPA_VERSION
Started: $start
 
EOF

umask 022

if (! -d "$distdir") then
    echo "*** ERROR: dist dir does not exist"
    echo "*** ERROR:   $distdir"
    exit 1
endif

#
# See if we need to update the version file with the current SVN
# revision number.  Do this *before* entering the distribution tree to
# solve a whole host of problems with VPATH (since srcdir may be
# relative or absolute)
#
set cur_svn_r="`grep '^svn_r' ${distdir}/VERSION | cut -d= -f2`"
if ("$cur_svn_r" == "-1") then
    sed -e 's/^svn_r=.*/svn_r='$svn_r'/' "${distdir}/VERSION" > "${distdir}/version.new"
    cp "${distdir}/version.new" "${distdir}/VERSION"
    rm -f "${distdir}/version.new"
    # need to reset the timestamp to not annoy AM dependencies
    touch -r "${srcdir}/VERSION" "${distdir}/VERSION"
    echo "*** Updated VERSION file with SVN r number"
else
    echo "*** Did NOT updated VERSION file with SVN r number"
endif

#########################################################
# VERY IMPORTANT: Now go into the new distribution tree #
#########################################################
cd "$distdir"
echo "*** Now in distdir: $distdir"

#
# Get the latest config.guess and config.sub from ftp.gnu.org
#

echo "*** Downloading latest config.sub/config.guess from ftp.gnu.org..."
cd config
set configdir="`pwd`"
mkdir tmp.$$
cd tmp.$$
# "Temporary" instructions at ftp.gnu.org/gnu/config advise using the 
# following URLs:
wget -t 1 -T 10 'http://savannah.gnu.org/cgi-bin/viewcvs/*checkout*/config/config/config.guess'
wget -t 1 -T 10 'http://savannah.gnu.org/cgi-bin/viewcvs/*checkout*/config/config/config.sub'
chmod +x config.guess config.sub

# Recently, ftp.gnu.org has had zero-legnth config.guess / config.sub
# files, which causes the automated nightly SVN snapshot tarball to
# fail to be made correctly.  This is a primitive attempt to fix that.
# If we got zero-length files from wget, use a config.guess /
# config.sub from a known location that is more recent than what ships
# in the current generation of auto* tools.

if (! -f config.guess || ! -s config.guess) then
    echo " - WARNING: Got BAD config.guess from ftp.gnu.org"
    echo " - WARNING: using included version"
else
    cp config.guess ..
endif
if (! -f config.sub || ! -s config.sub) then
    echo " - WARNING: Got BAD config.sub from ftp.gnu.org"
    echo " - WARNING: using known version"
else
    cp config.sub ..
endif
cd ..
rm -rf tmp.$$
cd ..

#
# All done
#

cat <<EOF
*** PLPA version $PLPA_VERSION distribution created
 
Started: $start
Ended:   `date`
 
EOF

