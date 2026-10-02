#!/bin/sh
#
# Copyright (c) 2026      Amazon.com, Inc. or its affiliates.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#
# Run two 2-rank jobs at the same time over btl/sm with colliding OPAL
# jobids, and require both to complete with uncorrupted data.  See
# sm_backing_file_collision.c for why and how the collision is forced.
#
#     sm_backing_file_collision.sh PROGRAM
#
# Prints one PASS or FAIL line and exits non-zero on FAIL.  MPIRUN and
# MPIRUN_TIMEOUT default to mpirun and 60.  SM_COLLISION_PRTERUN is the real
# prterun for the test program to exec in its place; by default it is found
# the way mpirun finds it (see below).
#
# Both jobs keep their btl/sm backing files in one fresh directory, which is
# what /dev/shm is to every job on Linux by default; nothing is written to
# the node's /dev/shm.  btl/sm opens its backing file in MPI_Init, so the
# collision happens when job B initializes while job A is running: job A
# holds after MPI_Init until job B has initialized, and then both run.

set -u

prog=$1
case $prog in
    /*) ;;
    *) prog=$(pwd)/$prog ;;
esac
MPIRUN=${MPIRUN:-mpirun}
MPIRUN_TIMEOUT=${MPIRUN_TIMEOUT:-60}
# Everything before the timed run must fit well inside MPIRUN_TIMEOUT.
SETUP_TIMEOUT=$((MPIRUN_TIMEOUT / 2))

fail() {
    echo "FAIL: btl-sm: $1"
    for job in a b; do
        if [ -s "$work/$job.log" ]; then
            echo "--- job $job"
            cat "$work/$job.log"
        fi
    done
    exit 1
}

work=
a=
b=
# Stop any running job, allowing 10 s before killing it, then remove the
# work directory.
cleanup() {
    trap '' HUP INT TERM
    if [ -n "$a$b" ]; then
        kill $a $b 2>/dev/null
        (sleep 10; kill -9 $a $b 2>/dev/null) &
        watchdog=$!
        wait $a $b 2>/dev/null
        kill $watchdog 2>/dev/null
    fi
    [ -n "$work" ] && rm -rf "$work"
}
trap cleanup EXIT
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

[ -x "$prog" ] || fail "$prog is not executable"
mpirun_path=$(command -v "$MPIRUN") || fail "cannot find $MPIRUN"
ompi_info=$(dirname "$mpirun_path")/ompi_info
[ -x "$ompi_info" ] || ompi_info=ompi_info

# Find prterun as mpirun does (ompi/tools/mpirun/main.c): $OMPI_PRTERUN; for
# an external PRRTE, $PRTE_PREFIX/bin and then the directory Open MPI was
# configured with (--with-prrte-bindir, or --with-prrte's bin); for its own
# PRRTE, Open MPI's bindir; then PATH.
if [ -z "${SM_COLLISION_PRTERUN:-}" ]; then
    info=$("$ompi_info" 2>/dev/null)
    configured=$(printf '%s\n' "$info" | sed -n "s/.*'--with-prrte-bindir=\([^']*\)'.*/\1/p")
    if [ -z "$configured" ]; then
        dir=$(printf '%s\n' "$info" | sed -n "s/.*'--with-prrte=\([^']*\)'.*/\1/p")
        case $dir in
            '' | internal | yes | no) ;;
            *) configured=$dir/bin ;;
        esac
    fi
    bindir=$("$ompi_info" --path bindir 2>/dev/null | sed -n 's/^ *Bindir: //p')
    if [ -n "$configured" ]; then
        candidates="${PRTE_PREFIX:+$PRTE_PREFIX/bin/prterun} $configured/prterun"
    else
        candidates="${bindir:+$bindir/prterun} ${PRTE_PREFIX:+$PRTE_PREFIX/bin/prterun}"
    fi
    for p in ${OMPI_PRTERUN:-} $candidates $(command -v prterun); do
        if [ -x "$p" ]; then
            SM_COLLISION_PRTERUN=$p
            break
        fi
    done
fi
[ -x "${SM_COLLISION_PRTERUN:-}" ] || fail "cannot find prterun; set SM_COLLISION_PRTERUN"
export SM_COLLISION_PRTERUN
echo "prterun: $SM_COLLISION_PRTERUN"

work=$(mktemp -d "${TMPDIR:-/tmp}/sm_collision.XXXXXX") || fail "cannot create a work directory"
shm=$work/shm
go=$work/go
mkdir "$shm" || fail "cannot create $shm"
: > "$work/a.log"
: > "$work/b.log"
deadline=$(($(date +%s) + SETUP_TIMEOUT))

# start_job JOB: run job JOB ("a" or "b") in the background.  mpirun execs
# this program in place of prterun (SM_COLLISION_SHIM), which execs the
# real prterun.  shmem/mmap is the component that honours the file name.
start_job() {
    SM_COLLISION_SHIM=1
    OMPI_PRTERUN=$prog
    SM_COLLISION_GO=$go
    SM_COLLISION_GO_TIMEOUT=$MPIRUN_TIMEOUT
    export SM_COLLISION_SHIM OMPI_PRTERUN SM_COLLISION_GO SM_COLLISION_GO_TIMEOUT
    exec "$MPIRUN" --timeout "$MPIRUN_TIMEOUT" -n 2 --map-by ppr:2:node \
        --mca pml ob1 --mca btl self,sm --mca btl_sm_backing_directory "$shm" \
        --mca shmem mmap --mca shmem_mmap_relocate_backing_file 0 \
        "$prog" 2 > "$work/$1.log" 2>&1
}

# await JOB: wait, until the setup deadline, for job JOB to report that it
# has initialized.
await() {
    until grep -q 'ready$' "$work/$1.log"; do
        [ "$(date +%s)" -lt "$deadline" ] || return 1
        sleep 1
    done
}

# nspace_of JOB: the namespace job JOB reported before MPI_Init.
nspace_of() {
    sed -n 's/.*namespace: //p' "$work/$1.log" | head -n 1
}

# jobid_of NAMESPACE: the OPAL jobid (job family and local job number).
jobid_of() {
    printf '%x' $(( ($("$prog" --family "$1") << 16) | ${1##*@} ))
}

start_job a &
a=$!
await a || fail "job A did not initialize within ${SETUP_TIMEOUT} s"
nspace=$(nspace_of a)

# The forced collision relies on how PRRTE names the job and on how Open
# MPI derives the jobid from that name.  Check both against job A.
pid=${nspace##*-}
pid=${pid%@1}
node=${nspace#prterun-}
node=${node%-"$pid"@1}
case $pid in
    '' | *[!0-9]*) node= ;;
esac
if [ -z "$node" ] || [ "prterun-$node-$pid@1" != "$nspace" ]; then
    fail "job A's namespace '$nspace' is not prterun-<node>-<pid>@1"
fi
jobid=$(jobid_of "$nspace")
for f in "$shm"/sm_segment.*."$jobid".*; do
    [ -e "$f" ] || fail "no backing file in $shm names the jobid $jobid predicted from $nspace"
    break
done

# Start job B on job A's job family while A holds, then let both run.
(SM_COLLISION_FAMILY=$("$prog" --family "$nspace") SM_COLLISION_NODENAME=$node
 export SM_COLLISION_FAMILY SM_COLLISION_NODENAME
 start_job b) &
b=$!
b_ready=yes
await b || b_ready=no
files=$(cd "$shm" && printf '%s ' *)
: > "$go"

# mpirun --timeout bounds each job, but teardown can outlast it: stop both
# jobs if they are still running MPIRUN_TIMEOUT + 30 s after release.
(sleep $((MPIRUN_TIMEOUT + 30)); : > "$work/stopped"; kill $a $b 2>/dev/null
 sleep 10; kill -9 $a $b 2>/dev/null) &
guard=$!
wait $a
status_a=$?
a=
wait $b
status_b=$?
b=
kill $guard 2>/dev/null
stopped=
[ -e "$work/stopped" ] && stopped=" (stopped by this test $((MPIRUN_TIMEOUT + 30)) s after release)"

nspace_b=$(nspace_of b)
echo "job A: $nspace; job B: ${nspace_b:-unknown}; backing files: $files"
case $nspace_b in
    *-"$node"-*@*) jobid_b=$(jobid_of "$nspace_b") ;;
    *) jobid_b= ;;
esac
if [ "$jobid_b" != "$jobid" ]; then
    fail "job B got jobid ${jobid_b:-unknown}, not $jobid (namespace ${nspace_b:-unknown})"
elif [ $b_ready = no ]; then
    fail "job B did not initialize within ${SETUP_TIMEOUT} s (exit statuses $status_a and $status_b)"
elif [ $status_a -ne 0 ] || [ $status_b -ne 0 ]; then
    fail "jobs with colliding jobid $jobid exited with $status_a and $status_b$stopped"
fi
echo "PASS: btl-sm: two concurrent jobs with jobid $jobid both completed"
