#!/bin/sh
#
# Please adjust the below parameters for Your setup.
#
#
SRCDIR=$HOME/WORK/OPENMPI                   # Where the script and the tar-balls reside
DATE_STRING=`date +%Y.%m.%d`                # Date string for tmp-/scratch-dir
TMPDIR=`ws_allocate OpenMPI-$DATE_STRING 3` # Where to build the OMPI 
LAST_VERSION=1.0a1r6896                     # Should be detected by download
TEST_VERSION=r362
CONFIG_FILE=build-$HOSTNAME.txt
PATCHES="ompi_orte.diff"                    # Comma-separated list of patches to apply when building
#EMAIL="testing@open-mpi.org"
EMAIL="keller@hlrs.de"

# download "latest" filename
latest_name="latest_snapshot.txt"


#
# This is required on some machines, that do not 
# have autoconf-tools (cron passes only a short PATH)
#
export PATH=$HOME/bin:$PATH


if [ "x$HOSTNAME" = "xstrider" ] ; then
  export PATH=$PATH:/opt/pathscale/bin
fi

#######################################################
#
# Local functions
#
# find a program from a list and load it into the target variable
find_program() {
    var=$1
    shift

    # first zero out the target variable
    str="$var="
    eval $str

    # loop through the list and save the first one that we find
    am_done=
    while test -z "$am_done" -a -n "$1" ; do
        prog=$1
        shift

        if test -z "$prog" ; then
            am_done=1
        else
            not_found="`which $prog 2>&1 | egrep '^no'`"
            which $prog > /dev/null 2>&1
            if test "$?" = "0" -a -z "$not_found" ; then
                str="$var=$prog"
                eval $str
                am_done=1
            fi
        fi
    done
}


dump () {
  file=$1
  shift
  echo "$1" >> $file
}

############################################
# Main part
############################################

if [ \! -d $SRCDIR ] ; then
  echo "WARNING $SRCDIR does not exist"
  exit
fi

cd $SRCDIR

# figure out what download command to use
find_program download wget lynx curl
if test -z "$download"; then
    echo "cannot find downloading program -- aborting in despair"
    exit 1
fi

# get the latest snapshot version number
# rm -f "$latest_name"
# $download "$url_arg/$latest_name"
# if test ! -f "$latest_name" ; then
#     echo "Could not download latest snapshot number -- aborting"
#     exit 1
# fi
# version=$LAST_VERSION
version="`cat $latest_name`"
test_version=$TEST_VERSION


#
# If the config-file does not exist, create one standard test file
#
if [ \! -r $CONFIG_FILE ] ; then
  echo "CONFIG_FILE:$CONFIG_FILE does not exist, create default file"

  CONFIG_FILE=build_sample_config.txt
  rm -f $CONFIG_FILE
  echo "gcc:1::--enable-picky" >> $CONFIG_FILE
  # echo "default build:::" >> $CONFIG_FILE
  # echo "static build:::--disable-shared --enable-static" >> $CONFIG_FILE
  # echo "non-threaded build:::--without-threads"  >> $CONFIG_FILE
fi


#
# Here comes the main part.
#
perl build_tarball.pl --email $EMAIL --config $CONFIG_FILE --scratch $TMPDIR --file openmpi-$version.tar.bz2 \
                      --patches "$PATCHES" \
                      --leave-install $SRCDIR/ompi-out-$version.txt --install-dir=$HOME/ompi-install --nocheck

if [ \! -r ompi-out-$version.txt ] ; then
  echo "No ompi-out-$version.txt file found; none of the configurations have been built?"
  exit
fi

#
# Now compile and install the tests for different combinations...
#
perl build_tests.pl --email $EMAIL --config $CONFIG_FILE --file openmpi-tests-$test_version.tar.bz2 --leave-install $SRCDIR/ompi-tests-out-$test_version.txt --outfile $SRCDIR/ompi-out-$version.txt --install-dir=$HOME/ompi-tests-install --nocheck

f="ompi-script-$version"
rm -fr $f
dump $f "#!/bin/bash"
dump $f "#PBS -l nodes=4,walltime=0:10:00" # Four nodes, and 10 minutes are enough
dump $f "#PBS -k eo"                   # Combine stderr/stdout
dump $f "#PBS -m ae"                   # Send mail on abort/end of job
dump $f "#PBS -M $EMAIL"               # Mail-adresses to send to
dump $f ""
dump $f "OLD_PATH=\$PATH"
dump $f "OLD_LD_LIBRARY_PATH=\$LD_LIBRARY_PATH"
dump $f "rm -fr $HOME/ompi-tmp/openmpi-sessions-*"
dump $f "killall -9 orted"

for MPI_DIR in `cat $SRCDIR/ompi-out-$version.txt` ; do
  dump $f "  echo Starting to test MPI_DIR:$MPI_DIR"
  dump $f "  echo -------------------------------------------------"
  dump $f "  export PATH=$MPI_DIR/bin:\$OLD_PATH"
  dump $f "  export LD_LIBRARY_PATH=$MPI_DIR/lib:\$OLD_LD_LIBRARY_PATH"
  dump $f "  rm -fr $HOME/ompi-tmp/openmpi-sessions-*"
  # Start the orted daemon in a commonly accessible directory (/tmp is local to each node)
  dump $f "  orted --tmpdir $HOME/ompi-tmp --seed --persistent --scope public"
  for j in `cat $SRCDIR/ompi-tests-out-$test_version.txt` ; do
    dump $f "    cd $j/bin"
    dump $f "    mpirun -x LD_LIBRARY_PATH -hostfile \$PBS_NODEFILE -tmpdir $HOME/ompi-tmp -np 8 ./PMB-MPI1 PingPong PingPing Sendrecv Exchange Allreduce Reduce Allgather Allgatherv Alltoall Bcast Barrier"
    dump $f "    ./tester -m -x LD_LIBRARY_PATH -hostfile \$PBS_NODEFILE -tmpdir $HOME/ompi-tmp -- mpich_all_tests"
    dump $f "    cd -"
    dump $f "    echo -------------------------------------------------"
  done
  dump $f "  killall -9 orted"
  dump $f "  sleep 10"
  dump $f "  echo -------------------------------------------------"
done

# qsub $SRCDIR/ompi-script-$version

