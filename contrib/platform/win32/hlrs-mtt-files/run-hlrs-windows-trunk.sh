#!/bin/sh


#
# This is REALLY important: The output of all commands
# is being parsed by MTT -- this should not be set to
# any other language, other than ENGLISH ,-]
#
unset LANG


#
# Of all the MTT-Phases, only the
# reporter-phase is run on the head-node (frbw),
# all the other ones should be run within a batch-job (hlrs-bwgrid.sh)
#

MTT_DIR=/cygdrive/d/tools/MTT-HLRS/mtt-trunk
export MTT_DIR

#
# need to find somewhere for logfiles
# currently set it to $MTT_DIR/log
if [ -d "log" ]; then
    echo;
else
    mkdir log;
fi

MPI_VERSION=trunk

MTT_TIME_STAMP=`date +%Y.%m.%d`
export MTT_TIME_STAMP

MTT_SCRATCH_NAME=mttscratch-$MPI_VERSION-$MTT_TIME_STAMP
mkdir /cygdrive/d/temp/MTT-run/$MTT_SCRATCH_NAME
OPENMPI_MTT_SCRATCH=/cygdrive/d/temp/MTT-run/$MTT_SCRATCH_NAME
export OPENMPI_MTT_SCRATCH


# this script should be run in the mtt-files folder
# so that it can find all the configure and test files
MTT_FILE_DIR=`pwd`
mkdir $MTT_FILE_DIR/log/$MPI_VERSION-$MTT_TIME_STAMP
MTT_LOG_DIR=$MTT_FILE_DIR/log/$MPI_VERSION-$MTT_TIME_STAMP
MTT_LOG_FILE=$MTT_LOG_DIR/full_log_$MPI_VERSION-$MTT_TIME_STAMP.txt
export MTT_LOG_FILE
export MTT_FILE_DIR

#
# Start the Shell-Script
#
env | sort


#need to disable the IPoIB adapter shortly,
#so that the connection to the MTT server can be established.
#e.g.: 
#     wmic path win32_networkadapter where index=14 call disable
#     # run mpi-get and mpi-install phases
#     $MTT_DIR/client/mtt --verbose --debug --mpi-phases hlrs-windows-$MPI_VERSION.ini
#     # enable the IPoIB adapter, and then do the test-get,build,run phases
#     wmic path win32_networkadapter where index=14 call enable
#     $MTT_DIR/client/mtt --verbose --debug --test-phases hlrs-windows-$MPI_VERSION.ini
#     # disable the IPoIB again, submit to the MTT server now.
#     wmic path win32_networkadapter where index=14 call disable
#     $MTT_DIR/client/mtt --verbose --debug --no-mpi-phases --no-test-phases hlrs-windows-$MPI_VERSION.ini
#
#

$MTT_DIR/client/mtt --verbose --debug hlrs-windows-$MPI_VERSION.ini

cp $OPENMPI_MTT_SCRATCH/*.txt $MTT_LOG_DIR/
cp $OPENMPI_MTT_SCRATCH/*.html $MTT_LOG_DIR/
rm -rf $OPENMPI_MTT_SCRATCH

