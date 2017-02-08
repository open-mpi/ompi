#!/bin/sh -x

# The tarballs to make
if [ $# -eq 0 ] ; then
    branches="v1.0"
else
    branches=$1
    shift
fi

# Build root - scratch space
build_root=/home/mpiteam/pmix/release

# Script to execute
script=contrib/make_dist_tarball

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
for branch in $branches; do
    cd $build_root/$branch

    module load "autotools/pmix-$branch"
    module load libevent/pmix-$branch

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

    module unload libevent
    module unload autotools
done
