#!/bin/sh

#####
#
# Configuration options
#
#####

# e-mail address to send results to
results_addr=hwloc-devel@open-mpi.org

# svn repository uri
code_uri=http://svn.open-mpi.org/svn/hwloc

# where to put built tarballs
outputroot=/l/osl/www/www.open-mpi.org/software/hwloc/nightly

# where to find the build script
script_uri=${code_uri}/trunk/contrib/nightly/create_tarball.sh
script_uri=contrib/nightly/create_tarball.sh

# The tarballs to make
if [ $# -eq 0 ] ; then
  dirs="/trunk /branches/v1.5 /branches/v1.4 /branches/v1.3 /branches/v1.2 /branches/v1.1 /branches/v1.0"
else
  dirs=$@
fi

# Build root - scratch space
build_root=/home/mpiteam/hwloc/nightly-tarball-build-root

export PATH=$HOME/local/bin:$PATH
export LD_LIBRARY_PATH=$HOME/local/lib:$LD_LIBRARY_PATH

#####
#
# Actually do stuff
#
#####

# load the modules configuration
. /etc/profile.d/modules.sh
module use ~/modules

# get our nightly build script
mkdir -p $build_root
cd $build_root

# Loop making them
for dir in $dirs; do
    # Remove leading /
    safe_dirname=`echo $dir | sed -e 's/^\///g'`
    # Convert remaining /'s to -'s
    safe_dirname=`echo $safe_dirname | sed -e 's/\//-/g'`
    # Now form a URL-specific script name
    script=$safe_dirname-`basename $script_uri`

    wget --quiet --no-check-certificate --tries=10 $code_uri/$dir/$script_uri -O $script
    if test ! $? -eq 0 ; then
        echo "wget of hwloc nightly tarball create script failed."
        if test -f $script ; then
            echo "Using older version of $script for this run."
        else
            echo "No build script available.  Aborting."
            exit 1
        fi
    fi
    chmod +x $script

    ver=`basename $dir`

    module load "autotools/hwloc-$ver"
    module load "tex-live/hwloc-$ver"

	./$script \
		$build_root/$ver \
		$results_addr \
		$code_uri/$dir \
		$outputroot/$ver >/dev/null 2>&1

    module unload autotools tex-live
done
