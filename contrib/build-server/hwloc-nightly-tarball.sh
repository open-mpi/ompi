#!/bin/sh

#####
#
# Configuration options
#
#####

# e-mail address to send results to
results_addr=hwloc-devel@lists.open-mpi.org
#results_addr=rhc@open-mpi.org

# git repository URL
code_uri=https://github.com/open-mpi/hwloc.git
raw_uri=https://raw.github.com/open-mpi/hwloc

# where to put built tarballs
outputroot=$HOME/hwloc/nightly

# where to find the build script
script_uri=contrib/nightly/make_snapshot_tarball

# helper scripts dir
script_dir=$HOME/scripts

# The tarballs to make
if [ $# -eq 0 ] ; then
    # Branches v1.6 and earlier were not updated to build nightly
    # snapshots from git, so only check v1.7 and later
    branches="master v1.11"
else
    branches=$@
fi

# Build root - scratch space
build_root=$HOME/hwloc/nightly-tarball-build-root

# Coverity stuff
coverity_token=`cat $HOME/coverity/hwloc-token.txt`

export PATH=$HOME_PREFIX/bin:$PATH
export LD_LIBRARY_PATH=$HOME_PREFIX/lib:$LD_LIBRARY_PATH

#####
#
# Actually do stuff
#
#####

# load the modules configuration
. $MODULE_INIT
module use $AUTOTOOL_MODULE

# get our nightly build script
mkdir -p $build_root
cd $build_root

pending_coverity=$build_root/tarballs-to-run-through-coverity.txt
rm -f $pending_coverity
touch $pending_coverity

# Loop making them
module unload autotools
for branch in $branches; do
    echo "=== Branch: $branch"
    # Get the last tarball version that was made
    prev_snapshot=`cat $outputroot/$branch/latest_snapshot.txt`

    # Form a URL-specific script name
    script=$branch-`basename $script_uri`

    echo "=== Getting script from: $raw_uri"
    wget --quiet --no-check-certificate --tries=10 $raw_uri/$branch/$script_uri -O $script
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

    module load "autotools/hwloc-$branch"
   # module load "tex-live/hwloc-$branch"

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

    # Did the script generate a new tarball?  If so, save it so that we can
    # spawn the coverity checker on it afterwards.  Only for this for the
    # master (for now).
    latest_snapshot=`cat $outputroot/$branch/latest_snapshot.txt`
    echo "=== Latest snapshot: $latest_snapshot"
    if test "$prev_snapshot" != "$latest_snapshot"; then
        if test "$branch" = "master"; then
            echo "=== Saving output for a Coverity run"
            echo "$outputroot/$branch/hwloc-$latest_snapshot.tar.bz2" >> $pending_coverity
        else
            echo "=== NOT saving output for a Coverity run"
        fi
        echo "=== Posting tarball to open-mpi.org"
        # tell the web server to cleanup old nightly tarballs
        ssh -p 2222 ompiteam@192.185.39.252 "git/ompi/contrib/build-server/remove-old.pl 7 public_html/software/hwloc/nightly/$branch"
        # upload the new ones
        scp -P 2222 $outputroot/$branch/hwloc-$latest_snapshot.tar.* ompiteam@192.185.39.252:public_html/software/hwloc/nightly/$branch/
        scp -P 2222 $outputroot/$branch/latest_snapshot.txt ompiteam@192.185.39.252:public_html/software/hwloc/nightly/$branch/
        # direct the web server to regenerate the checksums
        ssh -p 2222 ompiteam@192.185.39.252 "cd public_html/software/hwloc/nightly/$branch && md5sum hwloc* > md5sums.txt"
        ssh -p 2222 ompiteam@192.185.39.252 "cd public_html/software/hwloc/nightly/$branch && sha1sum hwloc* > sha1sums.txt"
    fi

    # Failed builds are not removed.  But if a human forgets to come
    # in here and clean up the old failed builds, we can accumulate
    # many over time.  So remove any old failed builds that are over
    # 4 weeks old.
    ${script_dir}/remove-old.pl 7 $build_root/$branch

done

# If we had any new snapshots to send to coverity, process them now

#for tarball in `cat $pending_coverity`; do
#    /home/common/mpiteam/scripts/hwloc-nightly-coverity.pl \
#        --filename=$tarball \
#        --coverity-token=$coverity_token \
#        --verbose \
#        --logfile-dir=$HOME/coverity \
#        --make-args="-j8"
#done
rm -f $pending_coverity
