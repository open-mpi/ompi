#!/bin/sh
#
# Copyright (c) 2022-2023 Amazon.com, Inc. or its affiliates.  All rights
#                         reserved.
# Copyright (c) 2022-2023 Joe Downs.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$

# Abort on error
set -euo pipefail

BUILD_32BIT=0
COMPILER=
DISTCHECK=0
AUTOGEN_ARGS=
CONFIGURE_ARGS=
MAKE_ARGS=
MAKE_J="-j 8"
PREFIX="${WORKSPACE}/install"
MPIRUN_MODE=${MPIRUN_MODE:-runall}

#
# Options Parsing
#
# For each option, we need to remove the quotes from their arguments. Without
# quotes, the command-line options for later commands (such as
# --disable-oshmem), are interpreted (in the following switch statement) as
# options for this script.

strip_quotes() {
    echo `echo "$1" | sed -e "s/\(\"\)\([[:alnum:]|_|-]*\)\(\"\)/\2/"`
}

PARAMS=""
while (( "$#" )); do
    case "$1" in
        --distcheck)
            DISTCHECK=1
            shift
            ;;
        --autogen-args)
            if [ -n "$2" ] && [ ${2:0:1} != "-" ]; then
                AUTOGEN_ARGS=$(strip_quotes $2)
                shift 2
            else
                echo "Error: Argument for $1 is missing" >&2
                exit 1
            fi
            ;;
        --configure-args)
            if [ -n "$2" ] && [ ${2:0:1} != "-" ]; then
                CONFIGURE_ARGS=$(strip_quotes $2)
                shift 2
            else
                echo "Error: Argument for $1 is missing" >&2
                exit 1
            fi
            ;;
        --compiler)
            if [ -n "$2" ] && [ ${2:0:1} != "-" ]; then
                COMPILER=$(strip_quotes $2)
                shift 2
            else
                echo "Error: Argument for $1 is missing" >&2
                exit 1
            fi
            ;;
        --mpirun-mode)
            if [ -n "$2" ] && [ ${2:0:1} != "-" ]; then
                MPIRUN_MODE=$(strip_quotes $2)
                shift 2
            else
                echo "Error: Argument for $1 is missing" >&2
                exit 1
            fi
            ;;
        -*|--*=) # Unsupported flags
            echo "Error: Unsupported flag $1" >&2
            exit 1
            ;;
        *) # Preserve positional arguments
            PARAMS="$PARAMS $1"
            shift
            ;;
    esac
done
# Set positional arguments in their proper place.
eval set -- "$PARAMS"

#
# Start by figuring out what we are...
#
os=`uname -s`
if test "${os}" = "Linux"; then
    eval "PLATFORM_ID=`sed -n 's/^ID=//p' /etc/os-release`"
    eval "VERSION_ID=`sed -n 's/^VERSION_ID=//p' /etc/os-release`"
else
    PLATFORM_ID=`uname -s`
    VERSION_ID=`uname -r`
fi

echo "--> platform: $PLATFORM_ID"
echo "--> version: $VERSION_ID"

#
# See if builder provided a compiler we should use, and translate it to
# CONFIGURE_ARGS.
#
case ${PLATFORM_ID} in
    rhel)
        case "$COMPILER" in
            gcc48|"")
                echo "--> Using default compilers"
                ;;
            *)
                echo "Unsupported compiler ${COMPILER}.  Aborting"
                exit 1
                ;;
        esac
        ;;
    amzn)
        case "$COMPILER" in
            "")
                echo "--> Using default compilers"
                ;;
            gcc44)
                CONFIGURE_ARGS="$CONFIGURE_ARGS CC=gcc44 CXX=g++44 FC=gfortran44"
                ;;
            gcc48)
                CONFIGURE_ARGS="$CONFIGURE_ARGS CC=gcc48 CXX=g++48 FC=gfortran48"
                ;;
            clang36)
                CONFIGURE_ARGS="$CONFIGURE_ARGS CC=clang CXX=clang++ --disable-mpi-fortran"
                ;;
            *)
                echo "Unsupported compiler ${COMPILER}.  Aborting"
                exit 1
                ;;
        esac
        ;;
    ubuntu)
        case "$COMPILER" in
            "")
                echo "--> Using default compilers"
                ;;
            gcc4*)
                version=`echo "$COMPILER" | sed -e 's/gcc4\([0-9]*\)/4.\1/'`
                CONFIGURE_ARGS="CC=gcc-${version} CXX=g++-${version} FC=gfortran-${version}"
                ;;
            gcc*)
                version=`echo "$COMPILER" | sed -e 's/gcc\([0-9]*\)/\1/'`
                CONFIGURE_ARGS="CC=gcc-${version} CXX=g++-${version} FC=gfortran-${version}"
                ;;
            clang3*|clang4*|clang5*|clang6*)
                version=`echo "$COMPILER" |  sed -e 's/clang\([0-9]\)\([0-9]*\)/\1.\2/'`
                CONFIGURE_ARGS="CC=clang-${version} CXX=clang++-${version} --disable-mpi-fortran"
                ;;
            clang*)
                version=`echo "$COMPILER" | sed -e 's/clang\([0-9]*\)/\1/'`
                CONFIGURE_ARGS="CC=clang-${version} CXX=clang++-${version} --disable-mpi-fortran"
                ;;
            *)
                echo "Unsupported compiler ${COMPILER}.  Aborting"
                exit 1
                ;;
        esac
        ;;
    sles)
        case "$COMPILER" in
            "")
                echo "--> Using default compilers"
                ;;
            gcc48)
                CONFIGURE_ARGS="$CONFIGURE_ARGS CC=gcc-48 CXX=g++-48 FC=gfortran-48"
                ;;
            gcc5)
                CONFIGURE_ARGS="$CONFIGURE_ARGS CC=gcc-5 CXX=g++-5 FC=gfortran-5"
                ;;
            gcc6)
                CONFIGURE_ARGS="$CONFIGURE_ARGS CC=gcc-6 CXX=g++-6 FC=gfortran-6"
                ;;
            *)
                echo "Unsupported compiler ${COMPILER}.  Aborting"
                exit 1
                ;;
        esac
        ;;
    FreeBSD)
        CONFIGURE_ARGS="$CONFIGURE_ARGS LDFLAGS=-Wl,-rpath,/usr/local/lib/gcc5 --with-wrapper-ldflags=-Wl,-rpath,/usr/local/lib/gcc5"
        ;;
esac

echo "--> Compiler setup: $CONFIGURE_ARGS"

#
# Add any Autogen or Configure arguments provided by the builder job.
#
if test "$AUTOGEN_ARGS" != ""; then
    # Special case, to work around the fact that Open MPI can't build when
    # there's a space in the build path name. (sigh)
    if test "$AUTOGEN_ARGS" = "--no-orte"; then
        AUTOGEN_ARGS="--no-orte --no-ompi"
    fi
fi

echo "--> Autogen arguments: $AUTOGEN_ARGS"
echo "--> Configure arguments: $CONFIGURE_ARGS"

# Build
sha1=`git rev-parse HEAD`
echo "--> Building commit ${sha1}"

if test -f autogen.pl; then
    echo "--> running ./autogen.pl ${AUTOGEN_ARGS}"
    ./autogen.pl ${AUTOGEN_ARGS}
else
    if test "${AUTOGEN_ARGS}" != ""; then
        echo "--> Being a coward and not running with special autogen arguments and autogen.sh"
        exit 1
    else
        echo "--> running ./atogen.sh"
        ./autogen.sh
    fi
fi

echo "--> running ./configure --prefix=\"${PREFIX}\" ${CONFIGURE_ARGS}"
if ! ./configure --prefix="${PREFIX}" ${CONFIGURE_ARGS}; then
    echo "./configure --prefix=\"${PREFIX}\" ${CONFIGURE_ARGS} failed, ABORTING !"
    if test -f config.log; then
        echo "config.log content :"
        cat config.log
    else
        echo "no config.log was generated"
    fi
    exit 1
fi

# Shortcut for the distcheck case, as it won't run any tests beyond the built-in
# make check tests. We need to install the requirements (Sphinx) so we can build
# the docs.
if test "${DISTCHECK}" = "1"; then
    echo "--> running make ${MAKE_ARGS} distcheck"
    make ${MAKE_ARGS} distcheck
    exit $?
fi

echo "--> running make ${MAKE_J} ${MAKE_ARGS} all"
make ${MAKE_J} ${MAKE_ARGS} all
echo "--> running make check"
make ${MAKE_ARGS} check
echo "--> running make install"
make ${MAKE_ARGS} install

export PATH="${PREFIX}/bin":${PATH}

case "$AUTOGEN_ARGS" in
    *--no-ompi*)
        echo "--> Skipping MPI tests due to --no-ompi"
        exit 0
        ;;
esac

echo "--> running ompi_info"
ompi_info

echo "--> running make all in examples"
cd "examples"
make ${MAKE_ARGS} all
cd ..

# It's hard to determine what the failure was and there's no printing of error
# code with set -e, so for the tests, we do per-command checking...
set +e

run_example() {
    example=`basename ${2}`
    echo "--> Running example: $example"
    ${1} ${2}
    ret=$?
    if test ${ret} -ne 0 ; then
        echo "Example failed: ${ret}"
        echo "Command was: ${1} ${2}"
        exit ${ret}
    fi
}

if test "${MPIRUN_MODE}" != "none"; then
    echo "--> running examples"
    echo "localhost cpu=2" > "${WORKSPACE}/hostfile"
    # Note: using perl here because figuring out a portable sed regexp
    # proved to be a little challenging.
    mpirun_version=`"${WORKSPACE}/install/bin/mpirun" --version | perl -wnE 'say $1 if /mpirun [^\d]*(\d+.\d+)/'`
    echo "--> mpirun version: ${mpirun_version}"
    case ${mpirun_version} in
        1.*|2.0*)
            exec="timeout -s SIGSEGV 3m mpirun -hostfile ${WORKSPACE}/hostfile -np 2 "
            ;;
        *)
            exec="timeout -s SIGSEGV 4m mpirun --get-stack-traces --timeout 180 --hostfile ${WORKSPACE}/hostfile -np 2 "
            ;;
    esac
    singleton="timeout -s SIGSEGV 1m "
    run_example "${exec}" ./examples/hello_c
    run_example "${singleton}" ./examples/hello_c
    run_example "${exec}" ./examples/ring_c
    run_example "${singleton}" ./examples/ring_c
    run_example "${exec}" ./examples/connectivity_c
    if ompi_info --parsable | grep -q bindings:cxx:yes >/dev/null; then
        echo "--> running C++ examples"
        run_example "${exec}" ./examples/hello_cxx
        run_example "${singleton}" ./examples/hello_cxx
        run_example "${exec}" ./examples/ring_cxx
        run_example "${singleton}" ./examples/ring_cxx
    else
        echo "--> skipping C++ examples"
    fi
    if ompi_info --parsable | grep -q bindings:mpif.h:yes >/dev/null; then
        echo "--> running mpif examples"
        run_example "${exec}" ./examples/hello_mpifh
        run_example "${singleton}" ./examples/hello_mpifh
        run_example "${exec}" ./examples/ring_mpifh
        run_example "${singleton}" ./examples/ring_mpifh
    else
        echo "--> skipping mpif examples"
    fi
    if ompi_info --parsable | egrep -q bindings:use_mpi:\"\?yes >/dev/null; then
        echo "--> running usempi examples"
        run_example "${exec}" ./examples/hello_usempi
        run_example "${singleton}" ./examples/hello_usempi
        run_example "${exec}" ./examples/ring_usempi
        run_example "${singleton}" ./examples/ring_usempi
    else
        echo "--> skipping usempi examples"
    fi
    if ompi_info --parsable | grep -q bindings:use_mpi_f08:yes >/dev/null; then
        echo "--> running usempif08 examples"
        run_example "${exec}" ./examples/hello_usempif08
        run_example "${singleton}" ./examples/hello_usempif08
        run_example "${exec}" ./examples/ring_usempif08
        run_example "${singleton}" ./examples/ring_usempif08
    else
        echo "--> skipping usempif08 examples"
    fi
else
    echo "--> Skipping examples (MPIRUN_MODE = none)"
fi

echo "--> All done!"
