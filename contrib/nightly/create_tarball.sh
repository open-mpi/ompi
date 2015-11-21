#!/bin/sh
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
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
# $3: dest dir
# $4: git URL
# $5: git branch
#

scratch_root=$1
email=$2
destdir=$3
giturl=$4
gitbranch=$5

# Set this to any value for additional output; typically only when
# debugging
debug=1

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
if test -z "$scratch_root" -o -z "$email" -o -z "$giturl" -o -z "$gitbranch" \
    -o -z "$destdir"; then
    echo "$0 scratch_root email_addr dest_dir git_url git_branch"
    exit 1
fi

# Use the branch name as the "version" string (for if there is an
# error).  This version string will be replaced upon successful "make
# distcheck" with the real version.
version=$gitbranch

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
    Mail -s "=== CREATE FAILURE ($version) ===" "$email" < "$outfile"
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

ERROR: Command returned a non-zero exist status ($version):
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

# make sure we can write to the destdir
file="$destdir/test-write.$$"
touch "$file"
if test ! -f "$file"; then
    die "Could not write to the dest dir: $destdir"
fi
rm -f "$file"

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

# setup target directory where clone+logs will go
clone_root="$scratch_root/ompi-`date +%Y-%m-%d-%H%M%S`"
rm -rf $clone_root
mkdir -p $clone_root

# startup the logfile (must be before do_command)
logdir="$clone_root/logs"
mkdir "$logdir"

# Get a fresh git clone
cd $clone_root
do_command "git clone $giturl ompi"
cd ompi
do_command "git checkout $gitbranch"

# Find the "git describe" string for this branch (remove a leading "ompi-"
# prefix, if there is one).
describe=`git describe --tags --always | sed -e s/^ompi-//`
if test -n "$debug"; then
    echo "** found $gitbranch describe: $describe"
fi
version=$describe

# if there's a $destdir/latest_snapshot.txt, see if anything has
# happened since the describe listed in that file
if test -f "$destdir/latest_snapshot.txt"; then
    snapshot_describe=`cat $destdir/latest_snapshot.txt`
    if test -n "$debug"; then
	echo "** last snapshot describe: $snapshot_describe"
    fi

    # Do we need a new snapshot?
    if test "$describe" = "$snapshot_describe"; then
	if test -n "$debug"; then
	    echo "** git $gitbranch describe is same as latest_snapshot -- not doing anything"
	fi
	# Since we didn't do anything, there's no point in leaving the clone we
	# just created
	cd ..
	rm -rf $clone_root

	# All done... nothing to see here...
	exit 0
    fi
fi

if test -n "$debug"; then
    echo "** making snapshot for describe: $describe"
fi

# Ensure that VERSION is set to indicate that it wants a snapshot, and
# insert the actual value that we want (so that ompi_get_version.sh
# will report exactly that version).
sed -e 's/^repo_rev=.*/repo_rev='$describe/ \
    -e 's/^tarball_version=.*/tarball_version='$describe/ \
    VERSION > VERSION.new
cp -f VERSION.new VERSION
rm -f VERSION.new

# lie about our username in $USER so that autogen will skip all
# .ompi_ignore'ed directories (i.e., so that we won't get
# .ompi_unignore'ed)
USER="ompibuilder"
export USER

# autogen is our friend
do_command "./autogen.pl --force"

# do config
do_command "./configure"

# Do make distcheck (which will invoke config/distscript.csh to set
# the right values in VERSION).  distcheck does many things; we need
# to ensure it doesn't pick up any other installs via LD_LIBRARY_PATH.
# It may be a bit Draconian to totally clean LD_LIBRARY_PATH (i.e., we
# may need something in there), but at least in the current building
# setup, we don't.  But be advised that this may need to change in the
# future...
save=$LD_LIBRARY_PATH
LD_LIBRARY_PATH=
do_command "make -j 8 distcheck"
LD_LIBRARY_PATH=$save
save=

# chmod the whole directory, so that core files are accessible by others
chmod a+rX -R .

# move the resulting tarballs to the destdir
gz="`/bin/ls openmpi*tar.gz`"
bz2="`/bin/ls openmpi*tar.bz2`"
mv $gz $bz2 $destdir
if test "$?" != "0"; then
    cat <<EOF
ERROR -- move final tarballs to web tree failed!

From:  `pwd`
Files: $gz $bz2
To:    $destdir

The nightly snapshots are therefore not available on the web!
EOF
    die "Could not move final tarballs to web dir: $destdir"
fi
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
    Mail -s "Create success ($version)" "$email" <<EOF
Creating nightly snapshot tarball was a success.

Snapshot:   $version
Start time: $start_time
End time:   `date`

Your friendly daemon,
Cyrador
EOF
fi
