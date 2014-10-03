#!/bin/sh

#####
#
# Configuration options
#
#####

# e-mail address to send results to
results_addr=testing@open-mpi.org

# svn repository uri
master_code_uri=https://github.com/open-mpi/ompi.git
master_raw_uri=https://raw.github.com/open-mpi/ompi
release_code_uri=https://github.com/open-mpi/ompi-release.git
release_raw_uri=https://raw.github.com/open-mpi/ompi-release

# where to put built tarballs
outputroot=/l/osl/www/www.open-mpi.org/nightly

# where to find the build script
script_uri=contrib/nightly/create_tarball.sh

# helper scripts dir
script_dir=/u/mpiteam/scripts

# The tarballs to make
if [ $# -eq 0 ] ; then
    # We're no longer ever checking the 1.0 - 1.6 branches anymore
    branches="master v1.8"
else
    branches=$@
fi

# Build root - scratch space
build_root=/home/mpiteam/openmpi/nightly-tarball-build-root

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

# Loop making the tarballs
module unload autotools
for branch in $branches; do
    if test "$branch" = "master"; then
        code_uri=$master_code_uri
        raw_uri=$master_raw_uri
    else
        code_uri=$release_code_uri
        raw_uri=$release_raw_uri
    fi

    # Form a URL-specific script name
    script=$branch-`basename $script_uri`

    echo "=== Getting script from: $raw_uri"
    wget --quiet --no-check-certificate --tries=10 $raw_uri/$branch/$script_uri -O $script
    if test ! $? -eq 0 ; then
        echo "wget of OMPI nightly tarball create script failed."
        if test -f $script ; then
            echo "Using older version of $script for this run."
        else
            echo "No build script available.  Aborting."
            exit 1
        fi
    fi
    chmod +x $script

    module load "autotools/ompi-$branch"

    echo "=== Running script..."
    ./$script \
        $build_root/$branch \
        $results_addr \
        $outputroot/$branch \
        $code_uri \
        $branch \
        >/dev/null 2>&1

    module unload autotools
    echo "=== Done running script"

    # Failed builds are not removed.  But if a human forgets to come
    # in here and clean up the old failed builds, we can accumulate
    # many over time.  So remove any old failed bbuilds that are over
    # 4 weeks old.
    ${script_dir}/remove-old.pl 28 $build_root/$branch
done
