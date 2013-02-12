#!/bin/bash
# Copyright (c) 2012      Mellanox Technologies, Inc.
#                         All rights reserved.


#this three parameters define the name of rpm package: <package_name>-<version>-<release>
package_name=openshmem
#version - defined from VERSION file 
#release - define from VERSION file

if [ -d /opt/knem-0.9.7mlnx1 ];then
	KNEM_FOLDER="/opt/knem-0.9.7mlnx1"
else
	KNEM_FOLDER="/opt/knem-0.9.7"
fi


fca='no'
debug='no'
extra_cflags=""
slurm_dir=""
while getopts :k:hf:dmw:scl: OPTION
do 
    case $OPTION in
    k)KNEM_FOLDER=$OPTARG 
        ;;
	h)echo "`basename $0` -[h] [-k <knem folder>] [-f <spec file>] [-d (debug)] [-w <number> (-DOSHMEM_WAIT_COMPLETION_DEBUG=number)] [-s (-DOSHMEM_SM_PUT_SYNC_MODE)]"
      exit 0;
        ;;
    l)slurm_dir=$OPTARG
      if [ -d "$slurm_dir/include" ]; then
		echo using pmi/slurm
		oshmem_configure_params="$oshmem_configure_params --with-pmi=$slurm_dir --with-slurm=$slurm_dir"
	  else
		  echo PMI selected but not found
		  exit -1
	  fi
        ;;
    f)rpmspec=$OPTARG
        ;;
        \?)echo "`basename $0` -[h] [-k <knem folder>] [-f <spec file>] [-d (debug)] [-w <number> (-DOSHMEM_WAIT_COMPLETION_DEBUG=number)] [-s (-DOSHMEM_SM_PUT_SYNC_MODE)]"
        ;;
    d)debug='yes'
        ;;
    c)fca='yes'
        ;;
    m)mxm='yes'
        ;;
    w) if [ $OPTARG -lt  0 ]; then
        echo '-w key should be > 0';
        exit 1;
        fi
      extra_cflags="$extra_cflags -DOSHMEM_WAIT_COMPLETION_DEBUG=$OPTARG"
        ;;
    s)extra_cflags="$extra_cflags -DOSHMEM_SM_PUT_SYNC_MODE"
        ;;
    esac
done

if [ $debug == "yes" ]; then
    DEBUG_CONF="--enable-debug"
    oshmem_name_prefix="debug-"
    extra_cflags="$extra_cflags -g -O0"
elif [ $debug == "no" ]; then
    DEBUG_CONF="--disable-debug"
    extra_cflags="$extra_cflags -g -O2"
    oshmem_name_prefix="";
fi
if [ "x" == "x$rpmspec" ]; then
    echo "please provide a spec file: -f <filename>";
    exit 1;
fi

if [ ! -f "$rpmspec" ]; then
   echo "$rpmspec does not exist";
   exit 1;
fi

if [ ! -d "$KNEM_FOLDER" ]; then 
    echo "$KNEM_FOLDER does not exist";
    exit 1;
fi
#parameters that are passed to the ./configure script
oshmem_configure_params="$oshmem_configure_params --with-oshmem --enable-mpirun-prefix-by-default $DEBUG_CONF --with-knem=$KNEM_FOLDER"


if [ $fca == "yes" ]; then
	echo using fca
	oshmem_configure_params="$oshmem_configure_params --with-fca=/opt/mellanox/fca"
fi

if [ $mxm == "yes" ]; then
	echo using mxm
	oshmem_configure_params="$oshmem_configure_params --with-mxm=/opt/mellanox/mxm"
fi

#build the binary rpm only
build_binary_rpm=${build_binary_rpm:='no'}
#build both: binary and source rpms
build_source_rpm=${build_source_rpm:='no'}

which rpmbuild &> /dev/null; 
if [ $? -ne 0 ]; then \
    echo "*** This make target requires an rpm-based linux distribution."; \
    (exit 1); exit 1; \
fi

mkdir -p rpm-dist sources build

work_dir=$(cd `dirname $0` && pwd)
version="`grep major= $work_dir/../VERSION | sed -e s/major=//`"."`grep minor= $work_dir/../VERSION | sed -e s/minor=//`"
release="`grep release= $work_dir/../VERSION | sed -e s/release=//`"
build=$(hg id -n | sed -e s/\+//g)
release=$build
echo oshmem version defined: $version-$release build: $build


echo Making source tarball...
(cd $work_dir/.. && ./autogen.sh && ./configure $oshmem_configure_params)
make -C $work_dir/.. distcheck && make -C $work_dir/.. get_tarball to=$work_dir/sources


rpmmacros="--define='_rpmdir $work_dir/rpm-dist' --define='_srcrpmdir $work_dir/rpm-dist' --define='_sourcedir $work_dir/sources' --define='_specdir $work_dir' --define='_builddir $work_dir/build'" 
rpmopts="--nodeps --buildroot=$work_dir/_rpm"

# Generate spec file for rpm
echo Generating oshmem.spec file
if [ ! -z "$oshmem_configure_params" ]
then
    oshmem_configure_params=${oshmem_configure_params//\//\\\/}
fi
if [ ! -z "$extra_cflags" ]
then 
    extra_cflags=${extra_cflags//\//\\\/}
fi

sed -e s/@OSHMEM_NAME@/openshmem/ -e s/@OSHMEM_CONFIGURE_PARAMS@/"$oshmem_configure_params"/ \
-e s/@OSHMEM_VERSION@/"$version"/ -e s/@OSHMEM_RELEASE@/"$release"/ -e s/@OSHMEM_CFLAGS@/"$extra_cflags"/ \
-e s/@OSHMEM_BUILD@/"$build"/ -e s/@OSHMEM_NAME_PREFIX@/"$oshmem_names_prefix"/ \
$rpmspec > oshmem.spec
echo ${version}-${build} > latest.txt

if [ $build_source_rpm == 'yes' ]
then
    echo running source rpmbuild...
    echo -ba -v $rpmmacros  $rpmopts oshmem.spec | xargs rpmbuild
    if [ $? -ne 0 ]; then \
        (exit 1); exit 1; \
    fi
	exit 0;
fi


if [ $build_binary_rpm == 'yes' ]
then
    echo running binary rpmbuild...
#    echo "$rpmmacros $rpmopts $rpmspec"
    echo -bb -v $rpmmacros $rpmopts oshmem.spec | xargs rpmbuild
    if [ $? -ne 0 ]; then \
        (exit 1); exit 1; \
    fi
fi



