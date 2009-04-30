#! /bin/bash
#
# Copyright (c) 2009 Los Alamos National Security, LLC. All rights reserved
#

if (( $# < 1 )) ; then
    echo "orte-bootproxy.sh: for OMPI internal use only"
    exit 1
fi

# take the first arg
var=$1

# if the var is CLEANUP, then we are in cleanup mode
if [ "${var}" == "CLEANUP" ]; then
    shift 1
    var=$1
    if [ -n "${var}" ] && [ "${var}" == "APPS" ]; then
        # kill specified apps
        shift 1
        var=$1
        while [ -n "${var}" ] && [ "${var}" != "FILES" ]; do
            killall -TERM "${var}"
#            echo "killall" "${var}"
            shift 1
            var=$1
        done
        if [ -n "${var}" ]; then
            shift 1
            var=$1
            # remove specified files
            while [ -n "${var}" ]; do
            rm -f "${var}"
#                echo "rm" "${var}"
                shift 1
                var=$1
            done
        fi
    elif [ "${var}" = "FILES" ]; then
        # remove specified files
        shift 1
        var=$1
        while [ -n "${var}" ]; do
            rm -f "${var}"
#            echo "rm" "${var}"
            shift 1
            var=$1
        done
    fi
    # remove any session directories from this user
#    sdir="${TMPDIR}""openmpi-sessions-""${USER}""@"`hostname`"_0"
#    echo "rm" "${sdir}"
    sdir="/tmp/openmpi-sessions-""${USER}""@"`hostname`"_0"
    rm -rf "${sdir}"
    exit 0
fi

# push all MCA params to the environment
while [ "${var:0:5}" = "OMPI_" ]; do
    if [ "${var:5:6}" = "PREFIX" ]; then
        export LD_LIBRARY_PATH="${var:12}"/lib:$LD_LIBRARY_PATH
        export PATH="${var:12}"/bin:$PATH
    elif [ "${var:5:4}" = "WDIR" ]; then
        cd "${var:10}"
    else
        export $var
    fi
    shift 1
    var=$1
done

# extract the application to be executed
app=$1
shift 1

#exec the app with the remaining args
exec "$app" "$@"
