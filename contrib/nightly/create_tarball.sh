#!/bin/sh
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
# This script is used to create a nightly snapshot tarball of Open MPI.
#
# $1: scratch root
# $2: e-mail address for destination
# $3: SVN root
# $4: dest dir
#

scratch_root="$1"
email="$2"
svnroot="$3"
destdir="$4"

# Set this to any value for additional output; typically only when
# debugging
debug=

# do you want a success mail?
want_success_mail=1

# max length of logfile to send in an e-mail
max_log_len=50

# how many snapshots to keep in the destdir?
max_snapshots=5

############################################################################
# Shouldn't need to change below this line
############################################################################

start_time="`date`"

# Sanity checks
if test -z "$scratch_root" -o -z "$email" -o -z "$svnroot" \
    -o -z "$destdir"; then
    echo "Must specify scratch root directory, e-mail address, SVN root, and destination directory"
    exit 1
fi

# send a mail
# should only be called after logdir is set
send_error_mail() {
    outfile="$scratch_root/output.txt"
    rm -f "$outfile"
    touch "$outfile"
    for file in `/bin/ls $logdir/* | sort`; do
        len="`wc -l $file | awk '{ print $1}'`"
        if test "`expr $len \> $max_log_len`" = "1"; then
            echo "[... previous lines snipped ...]" >> "$outfile"
            tail -n $max_log_len "$file" >> "$outfile"
        else
            cat "$file" >> "$outfile"
        fi
    done
    if test -n "$version"; then
        Mail -s "=== CREATE FAILURE ($version) ===" "$email" < "$outfile"
    else
        Mail -s "=== CREATE FAILURE ===" "$email" < "$outfile"
    fi
    rm -f "$outfile"
}

# send output error message
die() {
    msg="$*"
    cat > "$logdir/00_announce.txt" <<EOF
Creating the nightly tarball ended in error:

$msg
EOF
    send_error_mail
    exit 1
}

# do the work
# should only be called after logdir is set
do_command() {
    cmd="$*"
    logfile="$logdir/20-command.txt"
    rm -f "$logfile"
    if test -n "$debug"; then
        echo "*** Running command: $cmd"
        eval $cmd > "$logfile" 2>&1
        st=$?
        echo "*** Command complete: exit status: $st"
    else
        eval $cmd > "$logfile" 2>&1
        st=$?
    fi
    if test "$st" != "0"; then
        cat > "$logdir/15-error.txt" <<EOF

ERROR: Command returned a non-zero exist status
       $cmd

Start time: $start_time
End time:   `date`

=======================================================================
EOF
        cat > "$logdir/25-error.txt" <<EOF
=======================================================================

Your friendly daemon,
Cyrador
EOF
        send_error_mail
        exit 1
    fi
    rm -f "$logfile"
}

# see if the destination directory exists
if test ! -d "$destdir"; then
    mkdir -p "$destdir"
fi
if test ! -d "$destdir"; then
    die "Could not cd to dest dir: $destdir"
fi

# move into the scratch directory and ensure we have an absolute
# pathname for it
if test ! -d "$scratch_root"; then
    mkdir -p "$scratch_root"
fi
if test ! -d "$scratch_root"; then
    die "Could not cd to scratch root: $scratch_root"
fi
cd "$scratch_root"
scratch_root="`pwd`"

# get the snapshot number
svn co -N svn+ssh://svn.open-mpi.org/l/svn/ompi > /dev/null 2>&1
cd ompi
svnr="r`svn info . | egrep '^Revision: [0-9]+' | awk '{ print $2 }'`"
cd ..
rm -rf ompi
root="$scratch_root/create-$svnr"
rm -rf "$root"
mkdir "$root"
cd "$root"

# startup the logfile
logdir="$root/logs"
mkdir "$logdir"

# checkout a clean version
do_command "svn co $svnroot ompi"

# lets work on it
cd ompi
svnversion="`svnversion .`"

# remove all the unignore files so that we don't include anything that
# shouldn't be in the tarball
find . -name .ompi_unignore -exec rm -f {} \;

# autogen is our friend
do_command "./autogen.sh"

# do config
do_command "./configure --enable-dist"

# do make dist
do_command "make dist"

# move the resulting tarballs to the destdir
gz="`/bin/ls openmpi*tar.gz`"
bz2="`/bin/ls openmpi*tar.bz2`"
mv $gz $bz2 $destdir
cd $destdir

# make the latest_snapshot.txt file containing the last version
version="`echo $gz | sed -e 's/openmpi-\(.*\)\.tar\.gz/\1/g'`"
rm -f latest_snapshot.txt
echo $version > latest_snapshot.txt

# trim the destdir to $max_snapshots
for ext in gz bz2; do
    count="`ls openmpi*.tar.$ext | wc -l | awk '{ print $1 }'`"
    if test "`expr $count \> $max_snapshots`" = "1"; then
        num_old="`expr $count - $max_snapshots`"
        old="`ls -rt openmpi*.tar.$ext | head -n $num_old`"
        rm -f $old
    fi
done

# generate md5 and sha1 sums
rm -f md5sums.txt sha1sums.txt
touch md5sums.txt sha1sums.txt
for file in `/bin/ls *gz *bz2 | grep -v latest`; do
    md5sum $file >> md5sums.txt
    sha1sum $file >> sha1sums.txt
done

# remove temp dirs
cd "$scratch_root"
rm -rf "$root"

# send success mail
if test "$want_success_mail" = "1"; then
    Mail -s "Create success (r$version)" "$email" <<EOF
Creating nightly snapshot SVN tarball was a success.

Snapshot:   $version
Start time: $start_time
End time:   `date`

Your friendly daemon,
Cyrador
EOF
fi
