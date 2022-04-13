#!/bin/bash -f
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2006-2016 Cisco Systems, Inc.  All rights reserved.
#

#
# External script parameters
# The folowing parameters could be used to affect script behaviour.
# Please, do NOT set the same settings with parameters and config vars.
#
# -b
#    If you specify this option, only the all-in-one binary RPM will
#    be built. By default, only the source RPM (SRPM) is built. Other
#    parameters that affect the all-in-one binary RPM will be ignored
#    unless this option is specified.
#
# -n name
#    This option will change the name of the produced RPM to the "name".
#    It is useful to use with "-o" and "-m" options if you want to have
#    multiple Open MPI versions installed simultaneously in the same
#    enviroment. Requires use of option "-b".
#
# -o
#    With this option the install path of the binary RPM will be changed
#    to /opt/_NAME_/_VERSION_. Requires use of option "-b".
#
# -m
#    This option causes the RPM to also install modulefiles
#    to the location specified in the specfile. Requires use of option "-b".
#
# -i
#    Also build a debuginfo RPM. By default, the debuginfo RPM is not built.
#    Requires use of option "-b".
#
# -f lf_location
#    Include support for Libfabric. "lf_location" is Libfabric install
#    path. Requires use of option "-b".
#
# -t tm_location
#    Include support for Torque/PBS Pro. "tm_location" is path of the
#    Torque/PBS Pro header files. Requires use of option "-b".
#
# -d
#    Build with debugging support. By default,
#    the RPM is built without debugging support.
#
# -c parameter
#    Add custom configure parameter.
#
# -r parameter
#    Add custom RPM build parameter.
#
# -s
#    If specified, the script will try to unpack the openmpi.spec
#    file from the tarball specified on the command line. By default,
#    the script will look for the specfile in the current directory.
#
# -R directory
#    Specifies the top level RPM build direcotry.
#
# -h
#    Prints script usage information.
#

#
# General config vars
# The following vars can be set from outside and will affect script behave:
# prefix,rpmbuild_options,configure_options,build_srpm,build_single,build_multiple,rpmtopdir
#
specfile="openmpi.spec"
prefix=${prefix:-"/opt/openmpi"}
rpmbuild_options=${rpmbuild_options:-"--define 'mflags -j4' --define '_source_filedigest_algorithm md5'  --define '_binary_filedigest_algorithm md5'"}
configure_options=${configure_options:-""}
unpack_spec=0

# Helpful when debugging
#rpmbuild_options="--define 'mflags -j4' --define 'install_in_opt 1' --define 'cflags -g' --define 'install_modulefile 1' --define 'modules_rpm_name dhcp'"
#configure_options="--disable-mpi-f77 --without-io-romio --disable-mpi-f90"

# Some distro's will attempt to force using bizarre, custom compiler
# names (e.g., i386-redhat-linux-gnu-gcc).  So hardwire them to use
# "normal" names.
#export CC=gcc
#export CXX=g++
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
# save original parameters
#
orig_param="$@"

#
# usage information
#
usage="Usage: $0 [-b][-o][-m][-d][-u][-s][-h] [-n name][-f lf_location][-t tm_location][-R directory] tarball

  -b
             build all-in-one binary RPM only (required for all other flags to work)
             {default: build only SRPM}

  -n name
             name of the resulting RPM package set to name. Requires -b flag.
             {default: openmpi}

  -o         install in /opt/_NAME_/_VERSION_. Requires -b flag.
             {default: install in /usr}

  -m         install modulefiles during RPM installation. Requires -b flag.
             {default: modulefiles will NOT be installed}

  -i         build debuginfo RPM. Requires -b flag.
             {default: do NOT build debuginfo RPM}

  -f lf_location
             include Libfabric support from <lf_location>. Requires -b flag.
             {default: try to build with Libfabric support}

  -t tm_location
             include Torque/PBS Pro support from tm_location. Requires -b flag.
             {default: try to build with Torque/PBS Pro}

  -d         build with Debugging support
             {default: without debugging support}

  -s         try to unpack openmpi.spec file from tarball
             {default: search for openmpi.spec in current directory}

  -c parameter
             add custom configure parameter

  -r parameter
             add custom RPM build parameter

  -R directory
             Specifies the top level RPM build direcotry.

  -h         print this message and exit

  tarball    path to Open MPI source tarball
  "

#
# parse args
#
libfabric_path=""
rpmtopdir=

while getopts bn:omif:t:dc:r:sR:h flag; do
    case "$flag" in
      b) build_srpm="no"
         build_single="yes"
         ;;
      n) rpmbuild_options="$rpmbuild_options --define '_name $OPTARG'"
         ;;
      o) rpmbuild_options="$rpmbuild_options --define 'install_in_opt 1'"
         configure_options="$configure_options --enable-mpirun-prefix-by-default"
         ;;
      m) rpmbuild_options="$rpmbuild_options --define 'install_modulefile 1'"
         ;;
      i) rpmbuild_options="$rpmbuild_options --define 'build_debuginfo_rpm 1'"
         ;;
      f) libfabric_path="$OPTARG"
         ;;
      t) configure_options="$configure_options --with-tm=$OPTARG"
         ;;
      d) configure_options="$configure_options --enable-debug"
         ;;
      c) configure_options="$configure_options $OPTARG"
         ;;
      r) rpmbuild_options="$rpmbuild_options $OPTARG"
         ;;
      R) rpmtopdir="$OPTARG"
         ;;
      s) unpack_spec="1"
         ;;
      h) echo "$usage" 1>&2
         exit 0
         ;;
    esac
done
shift $(( OPTIND - 1 ));

#
# get the tarball name
#

tarball="$1"
if test "$tarball" = ""; then
    echo "$usage"
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
echo "--> Found Open MPI version: $version"

#
# Try to unpack spec file from tarball
#

if test $unpack_spec -eq 1; then
    tar -xf $tarball --wildcards --no-anchored 'openmpi.spec' --strip=4
fi

#
# do we have the spec files?
#

if test ! -r $specfile; then
    echo "can't find $specfile"
    exit 1
fi
echo "--> Found specfile: $specfile"

#
# try to find Libfabric lib subir
#
if test -n $libfabric_path; then
    # does lib64 exist?
    if test -d $libfabric_path/lib64; then
        # yes, so I will use lib64 as include dir
        configure_options="$configure_options --with-libfabric=$libfabric_path \"LDFLAGS=-Wl,--build-id -Wl,-rpath -Wl,$libfabric_path/lib64 -Wl,--enable-new-dtags\""
        echo "--> Found Libfabric lib dir: $libfabric_path/lib64"
    # does lib exist?
    elif test -d $libfabric_path/lib; then
        # yes, so I will use lib as include dir
        configure_options="$configure_options --with-libfabric=$libfabric_path \"LDFLAGS=-Wl,--build-id -Wl,-rpath -Wl,$libfabric_path/lib -Wl,--enable-new-dtags\""
        echo "--> Found Libfabric lib dir: $libfabric_path/lib"
    else
        # I give up, there is no lib64 or lib subdir
        echo "ERROR: Can't find Libfabric lib64/lib dir in $libfabric_path"
        exit 1
    fi
fi

#
# Find where the top RPM-building directory is
#

# if the user did not specify an $rpmtopdir, check for an .rpmmacros file.
if test "$rpmtopdir" == ""; then
    file=~/.rpmmacros
    if test -r $file; then
        rpmtopdir=${rpmtopdir:-"`grep %_topdir $file | awk '{ print $2 }'`"}
    fi
fi

# If needed, initialize the $rpmtopdir directory. If no $rpmtopdir was
# specified, try various system-level defaults.
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
    echo "--> Trying to sudo: \"$0 $orig_param\""
    echo "------------------------------------------------------------"
    sudo -u root sh -c "$0 $orig_param"
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

specdest="$rpmtopdir/SPECS/openmpi-$version.spec"
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
if test "$FC" != ""; then
    configure_options="$configure_options FC=$FC"
fi

#
# Make the SRPM
#

if test "$build_srpm" = "yes"; then
    echo "--> Building the Open MPI SRPM"
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
    echo "--> Building the single Open MPI RPM"
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
    echo "--> Building the multiple Open MPI RPM"
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
====                FINISHED BUILDING Open MPI RPM                        ====
------------------------------------------------------------------------------
A copy of the tarball is located in: $rpmtopdir/SOURCES/
The completed rpms are located in:   $rpmtopdir/RPMS/
The sources rpms are located in:     $rpmtopdir/SRPMS/
The spec files are located in:       $rpmtopdir/SPECS/
------------------------------------------------------------------------------

EOF
