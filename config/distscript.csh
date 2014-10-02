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
# Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

set srcdir="$1"
set builddir="`pwd`"
set distdir="$builddir/$2"
set OMPI_VERSION="$3"
set OMPI_REPO_REV="$4"

if ("$distdir" == "") then
    echo "Must supply relative distdir as argv[2] -- aborting"
    exit 1
elif ("$OMPI_VERSION" == "") then
    echo "Must supply version as argv[1] -- aborting"
    exit 1
endif

# we can catch some hard (but possible) to do mistakes by looking at
# our repo's revision, but only if we are in the source tree.
# Otherwise, use what configure told us, at the cost of allowing one
# or two corner cases in (but otherwise VPATH builds won't work)
set repo_rev=$OMPI_REPO_REV
if (-d .git) then
    set repo_rev="`config/opal_get_version.sh VERSION --repo-rev`"
endif

set start=`date`
cat <<EOF

Creating Open MPI distribution
In directory: `pwd`
Version: $OMPI_VERSION
Started: $start

EOF

umask 022

# Some shell startup files alias cp, mv, and rm to have "-i"
# (interactive).  Unalias here, just so that we don't use that option.
unalias cp
unalias rm
unalias mv

if (! -d "$distdir") then
    echo "*** ERROR: dist dir does not exist"
    echo "*** ERROR:   $distdir"
    exit 1
endif

#
# Update VERSION:repo_rev with the best value we have.
#
sed -e 's/^repo_rev=.*/repo_rev='$repo_rev'/' "${distdir}/VERSION" > "${distdir}/version.new"
cp -f "${distdir}/version.new" "${distdir}/VERSION"
rm -f "${distdir}/version.new"
# need to reset the timestamp to not annoy AM dependencies
touch -r "${srcdir}/VERSION" "${distdir}/VERSION"
echo "*** Updated VERSION file with repo rev number"

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
# Official HTTP git mirrors for config.guess / config.sub
wget -t 1 -T 10 -O config.guess 'http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.guess;hb=master'
wget -t 1 -T 10 -O config.sub 'http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.sub;hb=master'
chmod +x config.guess config.sub

# Recently, ftp.gnu.org has had zero-legnth config.guess / config.sub
# files, which causes the automated nightly SVN snapshot tarball to
# fail to be made correctly.  This is a primitive attempt to fix that.
# If we got zero-length files from wget, use a config.guess /
# config.sub from a known location that is more recent than what ships
# in the current generation of auto* tools.  Also check to ensure that
# the resulting scripts are runnable (Jan 2009: there are un-runnable
# scripts available right now because of some git vulnerability).

# Before you complain about this too loudly, remember that we're using
# unreleased software...

set happy=0
if (! -f config.guess || ! -s config.guess) then
    echo " - WARNING: Got bad config.guess from ftp.gnu.org (non-existent or empty)"
else
    ./config.guess >& /dev/null
    if ($status != 0) then
        echo " - WARNING: Got bad config.guess from ftp.gnu.org (not executable)"
    else
        if (! -f config.sub || ! -s config.sub) then
            echo " - WARNING: Got bad config.sub from ftp.gnu.org (non-existent or empty)"
        else
            ./config.sub `./config.guess` >& /dev/null
            if ($status != 0) then
                echo " - WARNING: Got bad config.sub from ftp.gnu.org (not executable)"
            else
                echo " - Got good config.guess and config.sub from ftp.gnu.org"
                cp -f config.sub config.guess ..
                set happy=1
            endif
        endif
    endif
endif

if ("$happy" == "0") then
    echo " - WARNING: using included versions for both config.sub and config.guess"
endif
cd ..
rm -rf tmp.$$
cd ..


#
# Find all the config.guess/config.sub files, and replace them with
# the ones that we've downloaded
#

echo "*** Now in: `pwd`"
echo "*** Replacing config.sub/config.guess with latest from ftp.gnu.org..."
foreach file (config.guess config.sub)
    foreach dir (opal orte ompi)
        if (-d $dir) then
            find $dir -name $file \
                -exec chmod +w {} \; \
                -exec cp -f $configdir/$file {} \; \
                -print
        endif
    end
end


#
# Put the release version number in the README and INSTALL files
#

set files="README INSTALL"
echo "*** Updating version number in $files..."
foreach file ($files)
    echo " - Setting $file"
    if (-f $file) then
	sed -e "s/OMPI_VERSION/$OMPI_VERSION/g" $file > bar
	mv -f bar $file
    endif
end

#
# All done
#

cat <<EOF
*** Open MPI version $OMPI_VERSION distribution created

Started: $start
Ended:   `date`

EOF

