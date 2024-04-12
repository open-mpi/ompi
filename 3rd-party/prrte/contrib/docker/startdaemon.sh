#!/bin/bash
#
# Copyright (c) 2020      Intel, Inc.  All rights reserved.
# Copyright (c) 2020      IBM Corporation.  All rights reserved.

#
# Default values
#
IMAGE_NAME=prrte/leap15:latest
OVERLAY_NETWORK=prte-net
RNDVZ_DIR=

COMMON_PREFIX=$USER"-"

SHUTDOWN_FILE=$PWD/tmp/shutdown-`hostname -s`.sh

DRYRUN=0

#
# Argument parsing
#
while [[ $# -gt 0 ]] ; do
    case $1 in
        "-h" | "--help")
            printf "Usage: %s [option]
    -i | --image NAME          Name of the container image (Required)
    -r | --rndvz DIR           Full path to the 'rendezvous' directory
    -d | --dryrun              Dry run. Do not actually start anything.
    -h | --help                Print this help message\n" \
        `basename $0`
            exit 0
            ;;
        "-i" | "--image" | "-img")
            shift
            IMAGE_NAME=$1
            ;;
        "--rndvz")
            shift
            RNDVZ_DIR=$1
            ;;
        "-d" | "--dryrun")
            DRYRUN=1
            ;;
        *)
            printf "Unkonwn option: %s\n" $1
            exit 1
            ;;
    esac
    shift
done

if [ "x$IMAGE_NAME" == "x" ] ; then
    echo "Error: --image must be specified"
    exit 1
fi

#
# Spin up the container
#

ALL_CONTAINERS=()

startup_container()
{
    C_ID=0
    C_HOSTNAME=`printf "%s%s%02d" $COMMON_PREFIX "node" $C_ID`

    if [ 0 != $DRYRUN ] ; then
        echo ""
        echo "Starting: $C_HOSTNAME"
        echo "---------------------"
    else
        echo "Starting: $C_HOSTNAME"
    fi

    # Add other volume mounts here
    _OTHER_ARGS=""

    if [ "x" != "x$RNDVZ_DIR" ] ; then
        _OTHER_ARGS+=" -v $RNDVZ_DIR:/opt/hpc/rndvz"
    fi

    CMD="docker run --rm \
        --cap-add=SYS_NICE --cap-add=SYS_PTRACE --security-opt seccomp=unconfined \
        $_OTHER_ARGS \
        --network $OVERLAY_NETWORK \
        -h $C_HOSTNAME --name $C_HOSTNAME \
        --detach $IMAGE_NAME"
    echo $CMD
    if [ 0 != $DRYRUN ] ; then
        return
    fi

    C_FULL_ID=`$CMD`
    RTN=$?
    if [ 0 != $RTN ] ; then
        echo "Error: Failed to create $C_HOSTNAME"
        echo $C_FULL_ID
        exit 1
    fi

    C_SHORT_ID=`echo $C_FULL_ID | cut -c -12`
    ALL_CONTAINERS+=($C_SHORT_ID)
}

mkdir -p tmp

# Create network
CMD="docker network create --driver overlay --attachable $OVERLAY_NETWORK"
if [ 0 == $DRYRUN ] ; then
    echo "Establish network: $OVERLAY_NETWORK"
    RTN=`$CMD`
else
    echo ""
    echo "Establish network: $OVERLAY_NETWORK"
    echo "---------------------"
    echo $CMD
fi

startup_container

if [ 0 != $DRYRUN ] ; then
    exit 0
fi

#
# Create a shutdown file to help when we cleanup
rm -f $SHUTDOWN_FILE

touch $SHUTDOWN_FILE
chmod +x $SHUTDOWN_FILE
for cid in "${ALL_CONTAINERS[@]}" ; do
    echo "docker stop $cid" >> $SHUTDOWN_FILE
done

CMD="docker network rm $OVERLAY_NETWORK"
if [ 0 == $DRYRUN ] ; then
    echo $CMD >> $SHUTDOWN_FILE
else
    echo ""
    echo "Remove network: $OVERLAY_NETWORK"
    echo "---------------------"
    echo $CMD
fi
