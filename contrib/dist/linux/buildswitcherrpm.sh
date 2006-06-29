#!/bin/sh -f
#
# Copyright (c) 2006 The Trustees of Indiana University and Indiana
#                    University Research and Technology
#                    Corporation.  All rights reserved.
# Copyright (c) 2006 Cisco Systems, Inc.  All rights reserved.
#

#
# General config vars
#

target="noarch"
specfile="openmpi-switcher-modulefile.spec"
sourcefile="openmpi-switcher-modulefile.tcl"
rpmbuild_options=

#########################################################################
# You should not need to change anything below this line
#########################################################################

#
# Look for the tarball
#

tarball="$1"
if test -z "$tarball" -o ! -f "$tarball"; then
    echo "Could not find the source tarball"
    exit 1
fi

#
# Ensure that we're running in the contrib/dist/linux directory
#

if test ! -f buildswitcherrpm.sh; then
    echo "Please run this script in the contrib/dist/linux directory"
    exit 1
fi

#
# Look for the source file
#

if test ! -f $sourcefile; then
    echo "Could not find $sourcefile"
    exit 1
fi

#
# Get the version number...
#

first="`basename $tarball | cut -d- -f2`"
if test "`echo $first | fgrep .tar.gz`" != ""; then
    version="`echo $first | sed -e 's/\.tar\.gz//'`"
elif test "`echo $first | fgrep .tar.bz2`" != ""; then
    version="`echo $first | sed -e 's/\.tar\.bz2//'`"
else
    echo Unable to determine version number from filename
    echo This script only supports .tar.gz and .tar.bz2 filenames
    exit 1
fi
unset first
echo "--> Found Open MPI version: $version"

#
# do we have the spec files?
#

if test ! -f $specfile; then
    echo "can't find $specfile"
    exit 1
fi
echo "--> Found specfile: $specfile"

#
# Find where the top RPM-building directory is
#

rpmtopdir="`grep %_topdir $HOME/.rpmmacros | awk '{ print $2 }'`"
if test "$rpmtopdir" != ""; then
    if test ! -d "$rpmtopdir"; then
	mkdir -p "$rpmtopdir"
	mkdir -p "$rpmtopdir/BUILD"
	mkdir -p "$rpmtopdir/RPMS"
	mkdir -p "$rpmtopdir/RPMS/i386"
	mkdir -p "$rpmtopdir/RPMS/i586"
	mkdir -p "$rpmtopdir/RPMS/i686"
	mkdir -p "$rpmtopdir/RPMS/noarch"
	mkdir -p "$rpmtopdir/RPMS/athlon"
	mkdir -p "$rpmtopdir/SOURCES"
	mkdir -p "$rpmtopdir/SPECS"
	mkdir -p "$rpmtopdir/SRPMS"
    fi
    need_root=0
elif test -d /usr/src/RPM; then
    need_root=1
    rpmtopdir="/usr/src/RPM"
else
    need_root=1
    rpmtopdir="/usr/src/redhat"
fi
echo "--> Found RPM top dir: $rpmtopdir"

#
# If we need root and are not root, try to sudo
#

if test "$need_root" = "1" -a "`whoami`" != "root"; then
    echo "--> Trying to sudo: \"$0 $*\""
    echo "------------------------------------------------------------"
    sudo -u root sh -c "$0 $tarball"
    echo "------------------------------------------------------------"
    echo "--> sudo finished"
    exit 0
fi


#
# make sure we have write access to the directories we need
#

if test ! -w $rpmtopdir/SOURCES ; then
    echo "Problem creating rpms: You do not have a $rpmtopdir directory"
    echo "tree or you do not have write access to the $rpmtopdir directory"
    echo "tree.  Please remedy and try again."
    exit 1
fi
echo "--> Have write access to $rpmtopdir/SOURCES"


#
# copy the files to the relevant directories
#

cp $sourcefile $rpmtopdir/SOURCES


#
# what command should we use?
# RH 8.0 changed from using "rpm -ba" to "rpmbuild -ba".  ARRGGH!!!
#

which rpmbuild
if test "$?" = "0"; then
    rpm_cmd="rpmbuild"
else
    rpm_cmd="rpm"
fi


#
# build the OSCAR RPM
#

specdest="$rpmtopdir/SPECS/$specfile"
sed -e 's/OPENMPIVERSION/'$version'/g' $specfile > "$specdest"

echo "--> Building the OSCAR Open MPI modulefile RPM"

cmd="$rpm_cmd -ba --target=$target $rpmbuild_options $specdest"
echo "--> $cmd"
eval $cmd

# This is a total hack and is only here because RPM changed option
# formats between RH 7.1 and 7.2.  <sigh>

if test $? != 0; then
  echo "*** FAILURE BUILDING OSCAR RPM!"
  echo "Aborting"
  exit 1
fi
echo "--> Done building the OSCAR Open MPI modulefile RPM"

#
# Done
#

cat <<EOF

------------------------------------------------------------------------------
====                FINISHED BUILDING Open MPI switcher RPMs              ====
------------------------------------------------------------------------------
A copy of the tarball is located in: $rpmtopdir/SOURCES/
The completed rpms are located in:   $rpmtopdir/RPMS/noarch/
The sources rpms are located in:     $rpmtopdir/SRPMS/
The spec files are located in:       $rpmtopdir/SPECS/
------------------------------------------------------------------------------

EOF
