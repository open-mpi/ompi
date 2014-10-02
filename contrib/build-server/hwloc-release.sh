#!/bin/sh -x

# The tarballs to make
#dirs="/branches/v1.2 /branches/v1.1 /branches/v1.0 /trunk"

# The tarballs to make
if [ $# -eq 0 ] ; then
  echo "Must specify which SVN branch to create (e.g., branches/v0.9)"
  exit 1
fi
dirs=$@

# Build root - scratch space
build_root=/home/mpiteam/hwloc/release/

# Script to execute
script=contrib/dist/make_dist_tarball

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

# move to the directory
# Loop making them
for dir in $dirs; do
        ver=`basename $dir`

        cd $build_root/$ver

        module load "autotools/hwloc-$ver"
        module load "tex-live/hwloc-$ver"

        ./$script $@ >dist.out 2>&1

        module unload autotools tex-live
done
