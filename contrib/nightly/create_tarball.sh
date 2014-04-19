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
# Copyright (c) 2006-2010 Cisco Systems, Inc.  All rights reserved.
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
# $5: version string for error e-mails, eg. trunk, v1.2, etc. (optional)
#

scratch_root="$1"
email="$2"
svnroot="$3"
destdir="$4"

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
if test -z "$scratch_root" -o -z "$email" -o -z "$svnroot" \
    -o -z "$destdir"; then
    echo "Must specify scratch root directory, e-mail address, SVN root, and destination directory"
    exit 1
fi

# Get a version string to use if there is an error.
# It will get replaced upon succesful "make distcheck" with the real version.
# Extract (from the SVN root) a version string if one wasn't supplied.
if test -n "$5"; then
    version="$5"
else
    version=`basename $svnroot`
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

# if there's a $destdir/latest_snapshot.txt, see if anything has
# happened since that r number.
desired_r=
if test -f "$destdir/latest_snapshot.txt"; then
    # $r will be just an integer (not "r12345")
    r=`cat $destdir/latest_snapshot.txt | sed -e 's/.*r\([0-9]*\)/\1/'`
    if test -n "$debug"; then
        echo "** last snapshot r: $r"
    fi

    # If the current HEAD is on this $svnroot, then we'll get a log
    # message.  Otherwise, we'll get a single line of dashes.
    file=/tmp/svn-log.txt.$$
    svn log -r HEAD $svnroot > $file
    # if we got more than 1 line, then extract the r number from the
    # log message.
    need_build=0
    if test "`wc -l $file | awk '{ print $1}'`" != "1"; then
        # $head_r will be "rXXXXX"
        head_r=`head -n 2 $file | tail -n 1 | awk '{ print $1 }'`
        if test -n "$debug"; then
            echo "** found HEAD r: $head_r"
        fi

        # If the head r is the same as the last_snapshot r, then exit
        # nicely
        rm -f /tmp/svn-log.txt.$$
        if test "r$r" = "$head_r"; then
            if test -n "$debug"; then
                echo "** svn HEAD r is same as last_snapshot -- not doing anything"
            fi
            exit 0
        fi

        # If we get here, it means the head r is different than the
        # last_snapshot r, and therefore we need to build.
        need_build=1
        desired_r=$head_r
    fi

    # If need_build still = 0, we know the r's are different.  But has
    # anything happened on this branch since then?
    if test "$need_build" = "0"; then
        svn log -r HEAD:$r $svnroot > $file

        # We'll definitely have at least one log message because we
        # included the last snapshot number in the svn log command
        # (i.e., we'll at least see the log message for that commit).
        # So there's no need to check for a single line of dashes
        # here.

        # $last_commit_r will be "rXXXXX"
        last_commit_r=`head -n 2 $file | tail -n 1 | awk '{ print $1 }'`
        if test -n "$debug"; then
            echo "** found last commit r: $last_commit_r"
        fi

        # If the head r is the same as the last_snapshot r, then exit
        # nicely
        rm -f $file
        if test "r$r" = "$last_commit_r"; then
            if test -n "$debug"; then
                echo "** Last commit is same r as last_snapshot -- not doing anything"
            fi
            exit 0
        fi

        # If we get here, the r numbers didn't match, and we therefore
        # need a new snapshot.
        desired_r=$last_commit_r
    fi
fi
if test -n "$debug"; then
    echo "** we need a new snapshot"
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

if test -n "$desired_r"; then
    # we got a desired r number from above, so use that
    # $svnr will be rXXXXX
    svnr=$desired_r
else
    # we don't have a desired r number, so get the last r number of a
    # commit
    svn co -N "$svnroot" ompi > /dev/null 2>&1
    cd ompi
    # $svnr will be rXXXXX
    svnr="r`svn info . | egrep '^Last Changed Rev: [0-9]+' | awk '{ print $4 }'`"
    cd ..
    rm -rf ompi
fi
if test -n "$debug"; then
    echo "** making snapshot for r: $svnr"
fi
root="$scratch_root/create-$svnr"
rm -rf "$root"
mkdir "$root"
cd "$root"

# startup the logfile
logdir="$root/logs"
mkdir "$logdir"

# checkout a clean version
r=`echo $svnr | cut -c2-`
do_command "svn co $svnroot -r $r ompi"

# ensure that we append the SVN number on the official version number
cd ompi
svnversion="r`svnversion .`"
version_files="`find . -name VERSION`"
d=`date +'%b %d, %Y'`
for file in $version_files; do
    sed -e 's/^want_repo_rev=.*/want_repo_rev=1/' \
        -e 's/^repo_rev=.*/repo_rev='$svnversion/ \
        -e 's@^date=.*@date="'"$d"' (nightly snapshot tarball)"@' \
        $file > $file.new
    cp -f $file.new $file
    rm -f $file.new
done

# lie about our username in $USER so that autogen will skip all
# .ompi_ignore'ed directories (i.e., so that we won't get 
# .ompi_unignore'ed)
USER="ompibuilder"
export USER

# autogen is our friend
do_command "./autogen.pl"

# do config
do_command "./configure --enable-dist"

# distcheck does many things; we need to ensure it doesn't pick up any 
# other OMPI installs via LD_LIBRARY_PATH.  It may be a bit Draconian
# to totally clean LD_LIBRARY_PATH (i.e., we may need something in there),
# but at least in the current building setup, we don't.  But be advised
# that this may need to change in the future...
save=$LD_LIBRARY_PATH
LD_LIBRARY_PATH=
do_command "make -j 8 distcheck"
LD_LIBRARY_PATH=$save
save=

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
    Mail -s "Create success (r$version)" "$email" <<EOF
Creating nightly snapshot SVN tarball was a success.

Snapshot:   $version
Start time: $start_time
End time:   `date`

Your friendly daemon,
Cyrador
EOF
fi
