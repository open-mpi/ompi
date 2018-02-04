#!/bin/sh -f
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Intel, Inc. All rights reserved.
#

#
# General config vars
# The following vars can be set from outside and will affect script behave:
# prefix,rpmbuild_options,configure_options,build_srpm,build_single,build_multiple,rpmtopdir
#


specfile="pmix.spec"
prefix=${prefix:-"/opt/pmix"}
rpmbuild_options=${rpmbuild_options:-"--define 'mflags -j4' --define '_source_filedigest_algorithm md5'  --define '_binary_filedigest_algorithm md5'"}
configure_options=${configure_options:-""}

# Helpful when debugging
#rpmbuild_options="--define 'mflags -j4' --define 'install_in_opt 1' --define 'cflags -g' --define 'install_modulefile 1' --define 'modules_rpm_name dhcp'"
#configure_options="--disable-mpi-f77 --without-io-romio --disable-mpi-f90"

# Some distro's will attempt to force using bizarre, custom compiler
# names (e.g., i386-redhat-linux-gnu-gcc).  So hardwire them to use
# "normal" names.
#export CC=gcc
#export CXX=g++
#export F77=f77
#export FC=

# Note that this script can build one or all of the following RPMs:
# SRPM, all-in-one, multiple.

# If you want to build the SRPM, put "yes" here
build_srpm=${build_srpm:-"yes"}
# If you want to build the "all in one RPM", put "yes" here
build_single=${build_single:-"no"}
# If you want to build the "multiple" RPMs, put "yes" here
build_multiple=${build_multiple:-"no"}

#########################################################################
# You should not need to change anything below this line
#########################################################################

#
# get the tarball name
#

tarball="$1"
if test "$tarball" = ""; then
    echo "Usage: buildrpm.sh <tarball>"
    exit 1
fi
if test ! -f $tarball; then
    echo "Can't find $tarball"
    exit 1
fi
echo "--> Found tarball: $tarball"

#
# get the extension from the tarball (gz or bz2)
#

extension=`echo $tarball | egrep '\.bz2'`
if test -n "$extension"; then
    extension=bz2
else
    extension=gz
fi

#
# Get the version number
#

first="`basename $tarball | cut -d- -f2`"
version="`echo $first | sed -e 's/\.tar\.'$extension'//'`"
unset first
echo "--> Found PMIx version: $version"

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

rpmtopdir=${rpmtopdir:-"`grep %_topdir $HOME/.rpmmacros | awk '{ print $2 }'`"}
if test "$rpmtopdir" != ""; then
	rpmbuild_options="$rpmbuild_options --define '_topdir $rpmtopdir'"
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
elif test -d /usr/src/packages; then
    need_root=1
    rpmtopdir="/usr/src/packages"
else
    need_root=1
    rpmtopdir="/usr/src/redhat"
fi
echo "--> Found RPM top dir: $rpmtopdir"

#
# If we're not root, try to sudo
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
# move the tarball file to the rpm directory
#

cp $tarball $rpmtopdir/SOURCES

#
# Print out the compilers
#

cat <<EOF
--> Hard-wired for compilers:
    CC = $CC
    CXX = $CXX
    F77 = $F77
    FC = $FC
EOF

#
# what command should we use?
# RH 8.0 changed from using "rpm -ba" to "rpmbuild -ba".  ARRGGH!!!
#

which rpmbuild 2>&1 >/dev/null
if test "$?" = "0"; then
    rpm_cmd="rpmbuild"
else
    rpm_cmd="rpm"
fi


#
# from the specfile
#

specdest="$rpmtopdir/SPECS/pmix-$version.spec"
sed -e 's/\$VERSION/'$version'/g' \
    -e 's/\$EXTENSION/'$extension'/g' \
    $specfile > "$specdest"
echo "--> Created destination specfile: $specdest"
release=`egrep -i release: $specdest | cut -d\  -f2`

#
# Setup compiler string
#

if test "$CC" != ""; then
    configure_options="$configure_options CC=$CC"
fi
if test "$CXX" != ""; then
    configure_options="$configure_options CXX=$CXX"
fi
if test "$F77" != ""; then
    configure_options="$configure_options F77=$F77"
fi
if test "$FC" != ""; then
    configure_options="$configure_options FC=$FC"
fi

#
# Make the SRPM
#

if test "$build_srpm" = "yes"; then
    echo "--> Building the PMIx SRPM"
    rpmbuild_options="$rpmbuild_options --define 'dist %{nil}'"
    cmd="$rpm_cmd $rpmbuild_options -bs $specdest"
    echo "--> $cmd"
    eval $cmd

    if test $? != 0; then
        echo "*** FAILURE BUILDING SRPM!"
        echo "Aborting"
        exit 1
    fi
    echo "--> Done building the SRPM"
fi

#
# Make the single RPM
#

if test "$build_single" = "yes"; then
    echo "--> Building the single PMIx RPM"
    cmd="$rpm_cmd -bb $rpmbuild_options --define 'build_all_in_one_rpm 1'"
    if test "$configure_options" != ""; then
        cmd="$cmd --define 'configure_options $configure_options'"
    fi
    cmd="$cmd $specdest"
    echo "--> $cmd"
    eval $cmd

    if test $? != 0; then
        echo "*** FAILURE BUILDING SINGLE RPM!"
        echo "Aborting"
        exit 1
    fi
    echo "--> Done building the single RPM"
fi

#
# Make the multi RPM
#

if test "$build_multiple" = "yes"; then
    echo "--> Building the multiple PMIx RPM"
    cmd="$rpm_cmd -bb $rpmbuild_options --define 'build_all_in_one_rpm 0'"
    if test "$configure_options" != ""; then
        cmd="$cmd --define 'configure_options $configure_options'"
    fi
    cmd="$cmd $specdest"
    echo "--> $cmd"
    eval $cmd

    if test $? != 0; then
        echo "*** FAILURE BUILDING MULTIPLE RPM!"
        echo "Aborting"
        exit 1
    fi
    echo "--> Done building the multiple RPM"
fi

#
# Done
#

cat <<EOF

------------------------------------------------------------------------------
====                FINISHED BUILDING PMIx RPM                        ====
------------------------------------------------------------------------------
A copy of the tarball is located in: $rpmtopdir/SOURCES/
The completed rpms are located in:   $rpmtopdir/RPMS/i<something>86/
The sources rpms are located in:     $rpmtopdir/SRPMS/
The spec files are located in:       $rpmtopdir/SPECS/
------------------------------------------------------------------------------

EOF
