#!/bin/sh
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
# This script is used to make a report of how may unimplemented
# top-level MPI functions exist.
#
# $1: scratch root
# $2: e-mail address for destination
# $3: URL_ARG
#

scratch_root_arg="$1"
email_arg="$2"
url_arg="$3"

# Set this to any value for additional output; typically only when
# debugging
debug=

# do you want a success mail?
want_success_mail=1

# download "latest" filename
latest_name="latest_snapshot.txt"

# checksum filenames
md5_checksums="md5sums.txt"
sha1_checksums="sha1sums.txt"

# max length of logfile to send in an e-mail
max_log_len=250

# email subject
subject="Unimplemented function report"

# max number of snapshots to keep downloaded
max_snapshots=3

############################################################################
# Shouldn't need to change below this line
############################################################################

start_time="`date`"

# This gets filled in later
config_guess=

# Sanity checks
if test -z "$scratch_root_arg" -o -z "$email_arg" -o -z "$url_arg"; then
    echo "Must specify scratch root directory, e-mail address, and URL_ARG"
    exit 1
fi

# send a mail
# should only be called after logdir is set
send_error_mail() {
    outfile="$scratch_root_arg/output.txt"
    rm -f "$outfile"
    touch "$outfile"
    for file in `/bin/ls $logdir/* | sort`; do
        len="`wc -l $file | awk '{ print $1}'`"
        if test "`expr $len \> $max_log_len`" = "1"; then
            tail -$max_log_len "$file" >> "$outfile"
        else
            cat "$file" >> "$outfile"
        fi
    done
    $mail -s "Failed report" "$email_arg" < "$outfile"
    rm -f "$outfile"
}

# send output error message
die() {
    msg="$*"
    cat > "$logdir/00_announce.txt" <<EOF
Creating the nightly tarball ended in error:

$msg

Host:   `hostname`
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

Host:      `hostname`
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
        exit 2
    fi
    rm -f "$logfile"
}

# find a program from a list and load it into the target variable
find_program() {
    var=$1
    shift

    # first zero out the target variable
    str="$var="
    eval $str

    # loop through the list and save the first one that we find
    am_done=
    while test -z "$am_done" -a -n "$1"; do
        prog=$1
        shift

        if test -z "$prog"; then
            am_done=1
        else
            not_found="`which $prog 2>&1 | egrep '^no'`"
            which $prog > /dev/null 2>&1
            if test "$?" = "0" -a -z "$not_found"; then
                str="$var=$prog"
                eval $str
                am_done=1
            fi
        fi
    done
}

# Find a mail program
find_program mail Mail mailx mail
if test -z "$mail"; then
    echo "Could not find mail program; aborting in despair"
    exit 1
fi

# figure out what download command to use
find_program download wget lynx curl
if test -z "$download"; then
    echo "cannot find downloading program -- aborting in despair"
    exit 1
fi

# move into the scratch directory, and ensure we have an absolute path
# for it
if test ! -d "$scratch_root_arg"; then
    mkdir -p "$scratch_root_arg"
fi
if test ! -d "$scratch_root_arg"; then
    echo "Could not cd to scratch root: $scratch_root_arg"
    exit 1
fi
cd "$scratch_root_arg"
scratch_root_arg="`pwd`"
logdir="$scratch_root_arg/logs"

# ensure some subdirs exist
for dir in downloads logs; do
    if test ! -d $dir; then
        mkdir $dir
    fi
done

# get the latest snapshot version number
cd downloads
rm -f "$latest_name"
do_command $download "$url_arg/$latest_name"
if test ! -f "$latest_name"; then
    die "Could not download latest snapshot number -- aborting"
fi
version="`cat $latest_name`"

# see if we need to download the tarball
tarball_name="openmpi-$version.tar.gz"
if test ! -f "$tarball_name"; then
    do_command $download "$url_arg/$tarball_name"
    if test ! -f "$tarball_name"; then
        die "Could not download tarball -- aborting"
    fi

    # get the checksums
    rm -f "$md5_checksums" "$sha1_checksums"
    do_command $download "$url_arg/$md5_checksums"
    do_command $download "$url_arg/$sha1_checksums"
fi

# verify the checksums
md5_file="`grep $version.tar.gz $md5_checksums`"
find_program md5sum md5sum
if test -z "$md5sum"; then
    cat > $logdir/05_md5sum_warning.txt <<EOF
WARNING: Could not find md5sum executable, so I will not be able to check
WARNING: the validity of downloaded executables against their known MD5 
WARNING: checksums.  Proceeding anyway...

EOF
elif test -z "$md5_file"; then
    cat > $logdir/05_md5sum_warning.txt <<EOF
WARNING: Could not find md5sum check file, so I will not be able to check
WARNING: the validity of downloaded executables against their known MD5 
WARNING: checksums.  Proceeding anyway...

EOF
else
    md5_actual="`$md5sum $tarball_name 2>&1`"
    if test "$md5_file" != "$md5_actual"; then
        die "md5sum from checksum file does not match actual ($md5_file != $md5_actual)"
    fi
fi

sha1_file="`grep $version.tar.gz $sha1_checksums`"
find_program sha1sum sha1sum
if test -z "$sha1sum"; then
    cat > $logdir/06_sha1sum_warning.txt <<EOF
WARNING: Could not find sha1sum executable, so I will not be able to check
WARNING: the validity of downloaded executables against their known SHA1
WARNING: checksums.  Proceeding anyway...

EOF
elif test -z "$sha1_file"; then
    cat > $logdir/06_sha1sum_warning.txt <<EOF
WARNING: Could not find sha1sum check file, so I will not be able to check
WARNING: the validity of downloaded executables against their known SHA1
WARNING: checksums.  Proceeding anyway...

EOF
else
    sha1_actual="`$sha1sum $tarball_name 2>&1`"
    if test "$sha1_file" != "$sha1_actual"; then
        die "sha1sum from checksum file does not match actual ($sha1_file != $sha1_actual)"
    fi
fi

# Make a root for this build to play in (scratch_root_arg is absolute, so
# root will be absolute)
root="$scratch_root_arg/build-$version"
rm -rf "$root"
mkdir "$root"
cd "$root"

# make the unzip root
startdir="`pwd`"
unzip_root="$root/unimplemented_report"
if test ! -d "$unzip_root"; then
    mkdir "$unzip_root"
fi
cd "$unzip_root"

# expand the tarball (do NOT assume GNU tar)
do_command "gunzip -c $scratch_root_arg/downloads/$tarball_name | tar xf -"
cd openmpi-$version/src/mpi

# setup output files
report_c="$logdir/report_c.$$.txt"
report_f77="$logdir/report_f77.$$.txt"
rm -f "$report_c" "$report_f77"

# count unimplemented C functions
cd c
grep -ls "not yet implemented" *.c > "$report_c"
total_c="`ls -1 *.c | wc -l | awk '{ print $1 }'`"
num_c="`wc -l $report_c | awk '{ print $1 }'`"

# count unimplemented F77 functions
cd ../f77
grep -ls "not yet implemented" *.c > "$report_f77"
total_f77="`ls -1 *.c | wc -l | awk '{ print $1 }'`"
num_f77="`wc -l $report_f77 | awk '{ print $1 }'`"

# trim the downloads dir to $max_snapshots
cd "$scratch_root_arg/downloads"
for ext in gz; do
    count="`ls openmpi*.tar.$ext | wc -l | awk '{ print $1 }'`"
    if test "`expr $count \> $max_snapshots`" = "1"; then
        num_old="`expr $count - $max_snapshots`"
        old="`ls -rt openmpi*.tar.$ext | head -n $num_old`"
        rm -f $old
    fi
done

# send success mail
$mail -s "$subject" "$email_arg" <<EOF
Unimplemented MPI function report

Snapshot:   $version

Total C functions: $total_c
Unimplemented C functions: $num_c

Total F77 functions: $total_f77
Unimplemented F77 functions: $num_f77

Unimplemented C functions:
---------------------------------------------------------------------------
`cat $report_c`
---------------------------------------------------------------------------

Unimplemented F77 functions:
---------------------------------------------------------------------------
`cat $report_f77`
---------------------------------------------------------------------------

Your friendly daemon,
Cyrador
EOF

# all done
rm -rf "$report"
rm -rf "$root"
rm -f "$config_list"
exit 0
