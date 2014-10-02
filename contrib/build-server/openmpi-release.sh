#!/bin/sh -x

# The tarballs to make
if [ $# -eq 0 ] ; then
    dirs="branches/v1.8"
else
    dirs=$@
fi

# Build root - scratch space
build_root=/home/mpiteam/openmpi/release/

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
module load sowing

# move to the directory
# Loop making them
for dir in $dirs; do
        ver=`basename $dir`

        cd $build_root/$ver

        module load "autotools/ompi-$ver"

        ./$script $@ >dist.out 2>&1
	if test "$?" != "0"; then
		cat <<EOF
=============================================================================
== Dist failure
== Last few lines of output (full results in dist.out file):
=============================================================================
EOF
		tail -n 20 dist.out
		exit 1
	fi

        module unload "autotools"
done
