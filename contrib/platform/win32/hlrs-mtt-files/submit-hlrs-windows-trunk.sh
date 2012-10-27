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

MTT_DIR=/cygdrive/c/Users/hpcfan/Documents/tools/MTT-HLRS/mtt-trunk
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
mkdir /cygdrive/c/Users/hpcfan/Documents/temp/$MTT_SCRATCH_NAME
OPENMPI_MTT_SCRATCH=/cygdrive/c/Users/hpcfan/Documents/temp/$MTT_SCRATCH_NAME
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


$MTT_DIR/client/mtt --verbose --debug --no-section "get install"--section database hlrs-windows-$MPI_VERSION.ini

cp $OPENMPI_MTT_SCRATCH/*.txt $MTT_LOG_DIR/
cp $OPENMPI_MTT_SCRATCH/*.html $MTT_LOG_DIR/
rm -rf $OPENMPI_MTT_SCRATCH

