#!/bin/sh

#####
#
# Configuration options
#
#####

# e-mail address to send results to
results_addr=testing@open-mpi.org

# svn repository uri
code_uri=http://svn.open-mpi.org/svn/ompi

# where to put built tarballs
outputroot=/l/osl/www/www.open-mpi.org/nightly

# where to find the build script
create_script=/contrib/nightly/create_tarball.sh

# helper scripts dir
script_dir=/u/mpiteam/scripts

# The tarballs to make
if [ $# -eq 0 ] ; then
    # We're no longer ever checking the 1.0 - 1.4 branches anymore
    dirs="/trunk /branches/v1.7 /branches/v1.6"
else
    dirs=$@
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

script=`basename $create_script`

# load the modules configuration
. /etc/profile.d/modules.sh
module use ~/modules

# get our nightly build script
mkdir -p $build_root
cd $build_root

# Loop making the tarballs
module unload autotools
for dir in $dirs; do
    ver=`basename $dir`

    module load "autotools/ompi-$ver"

    script_uri="$code_uri$dir/$create_script"
    script_exec="$build_root/$ver/$script"
    echo "=== Getting script from: $script_uri"
    wget --quiet --no-check-certificate --tries=10 $script_uri -O "$script_exec"
    if test ! $? -eq 0 ; then
        echo "wget of Open MPI nightly $ver tarball create script failed."
        exit 1
    fi

    echo "=== Running script..."
    chmod +x "$script_exec"
	"$script_exec" \
		$build_root/$ver \
		$results_addr \
		$code_uri/$dir \
		$outputroot/$ver
    module unload autotools
    echo "=== Done running script"

    # Failed builds are not removed.  But if a human forgets to come
    # in here and clean up the old failed builds, we can accumulate
    # many over time.  So remove any old failed bbuilds that are over
    # 4 weeks old.
    ${script_dir}/remove-old.pl 28 $build_root/$ver
done
