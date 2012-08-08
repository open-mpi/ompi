#!/bin/sh

#####
#
# Configuration options
#
#####

# e-mail address to send results to
results_addr=timattox@open-mpi.org

# svn repository uri
code_uri=http://svn.open-mpi.org/svn/ompi

# where to put built tarballs
#outputroot=/l/osl/www/www.open-mpi.org/nightly
outputroot=/home/mpiteam/openmpi/testing

# where to find the build script
script_uri=${code_uri}/trunk/contrib/nightly/create_tarball.sh

# The tarballs to make
if [ $# -eq 0 ] ; then
  dirs="branches/v1.3"
else
  dirs=$@
fi

# Build root - scratch space
build_root=/home/mpiteam/openmpi/testing-tarball-build-root

export PATH=$HOME/local/bin:$PATH
export LD_LIBRARY_PATH=$HOME/local/lib:$LD_LIBRARY_PATH

#####
#
# Actually do stuff
#
#####

# load the modules configuration
#JJH File No longer exists: . /etc/profile.d/00-modules.sh
. /etc/profile.d/modules.sh
module use ~/modules
module load sowing

# get our nightly build script
mkdir -p $build_root
cd $build_root
script=`basename $script_uri`
wget --quiet --no-check-certificate --tries=10 $script_uri -O $script
if test ! $? -eq 0 ; then
    echo "wget of Open MPI nightly tarball create script failed."
    if test -f $script ; then
        echo "Using older version of $script for this run."
    else
        echo "No build script available.  Aborting."
        exit 1
    fi
fi
chmod +x $script

# Loop making them
for dir in $dirs; do
        ver=`basename $dir`

        module load "autotools/ompi-$ver"
       
	./$script \
		$build_root/$ver \
		$results_addr \
		$code_uri/$dir \
		$outputroot/$ver >/dev/null 2>&1
        module unload "autotools"
done
