#!/bin/sh
#
# $HEADER$
#
# This script is used to make a nightly snapshot tarball of Open MPI.
#
# $1: scratch root
# $2: e-mail address for destination
# $3: URL_ARG
# $4: config file
# $5: vpath_arg
#

scratch_root_arg="$1"
email_arg="$2"
url_arg="$3"
config_arg="$4"
vpath_arg="$5"

# Set this to any value for additional output; typically only when
# debugging
debug=0

# do you want a success mail?
want_success_mail=1

# download "latest" filename
latest_name="latest_snapshot.txt"

# checksum filenames
md5_checksums="md5sums.txt"
sha1_checksums="sha1sums.txt"

# max length of logfile to send in an e-mail
max_log_len=250

# email subjects
success_subject="Success"
fail_subject="=== TEST FAILURE ==="

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
    echo "Can also optionally specify a config file and whether want VPATH building"
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
    Mail -s "$fail_subject" "$email_arg" < "$outfile"
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
        exit 2
    fi
    rm -f "$logfile"
}

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

# figure out what download command to use
which wget > /dev/null 2>&1
if test "$?" = "0"; then
    download="wget"
else
    which lynx > /dev/null 2>&1
    if test "$?" = "0"; then
        download="lynx --download"
    else
        die "Cannot find wget or lynx to download the tarball -- aborting"
    fi
fi
echo

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
if test -z "$md5_file"; then
    echo "WARNING: could not find md5sum in checksum file!"
    echo "WARNING: proceeding anyway..."
else
    md5_actual="`md5sum $tarball_name 2>&1`"
    if test "$md5_file" != "$md5_actual"; then
        die "md5sum from checksum file does not match actual ($md5_file != $md5_actual)"
    fi
fi

sha1_file="`grep $version.tar.gz $sha1_checksums`"
if test -z "$sha1_file"; then
    echo "WARNING: could not find sha1sum in checksum file!"
    echo "WARNING: proceeding anyway..."
else
    sha1_actual="`sha1sum $tarball_name 2>&1`"
    if test "$sha1_file" != "$sha1_actual"; then
        die "sha1sum from checksum file does not match actual ($sha1_file != $sha1_actual)"
    fi
fi

# subroutine for building a single configuration
try_build() {
    srcroot="$1"
    installdir="$2"
    confargs="$3"
    vpath_mode="$4"

    startdir="`pwd`"

    # make the source root
    if test ! -d "$srcroot"; then
        mkdir "$srcroot"
    fi
    cd "$srcroot"

    # expand the tarball (do NOT assume GNU tar)
    do_command "gunzip -c $scratch_root_arg/downloads/$tarball_name | tar xf -"
    cd openmpi-$version

    # if we didn't already get the platform, get it
    if test -z "$config_guess"; then
        config_guess="`./config/config.guess`"
    fi

    # configure it
    if test -z "$vpath_mode"; then
        conf="./configure"
    else
        mkdir vpath_build
        cd vpath_build
        if test "$vpath_mode" = "relative"; then
            conf="../configure"
        else
            conf="$srcroot/openmpi-$version/configure"
        fi
    fi
    do_command "$conf --prefix=$installdir $confargs"

    # build it
    do_command "make"

    # install it
    do_command "make install"

    # try compiling and linking a simple C application
    cd ..
    if test ! -d test; then
        mkdir test
    fi
    cd test
    cat > hello.c <<EOF
#include <mpi.h>
int main(int argc, char* argv[]) {
  MPI_Init(&argc, &argv);
  MPI_Finalize();
  return 0;
}
EOF
    do_command $installdir/bin/mpicc hello.c -o hello
    rm -f hello

    # if we have a C++ compiler, try compiling and linking a simple
    # C++ application
    have_cxx="`$installdir/bin/ompi_info --parsable | grep bindings:cxx | cut -d: -f3`"
    if test "$have_cxx" = "yes"; then
        cat > hello.cc <<EOF
#include <mpi.h>
int main(int argc, char* argv[]) {
  MPI::nit(argc, argv);
  MPI::Finalize();
  return 0;
}
EOF
        do_command $installdir/bin/mpic++ hello.cc -o hello
        rm -f hello
    fi

    # if we have a F77 compiler, try compiling and linking a simple
    # F77 application
    have_cxx="`$installdir/bin/ompi_info --parsable | grep bindings:f77 | cut -d: -f3`"
    if test "$have_f77" = "yes"; then
        cat > hello.f <<EOF
        program main
        include 'mpif.h'
        call MPI_INIT(ierr)
        call MPI_FINALIZE(ierr)
        stop
        end
EOF
        do_command $installdir/bin/mpif77 hello.f -o hello
        rm -f hello
    fi

    # all done -- clean up
    cd "$startdir"
    rm -rf "$srcroot"
}

# Make a root for this build to play in (scratch_root_arg is absolute, so
# root will be absolute)
root="$scratch_root_arg/build-$version"
rm -rf "$root"
mkdir "$root"
cd "$root"

# start up the log file directory
mkdir logs
logdir="$root/logs"

# loop over all configurations
# be lazy: if no configurations supplied and no vpath, do a default
# configure/build
config_list="$root/configurations.$$.txt"
touch "$config_list"
do_default=
if test -z "$config_arg" -o ! -f "$config_arg"; then
    if test -z "$vpath_arg"; then
        do_default=1
    fi
fi
if test "$do_default" = "1"; then
    dir="$root/default"
    echo "default" >> "$config_list"
    try_build "$dir" "$dir/install" "CFLAGS=-g" ""
elif test -f "$config_arg"; then
    len="`wc -l $config_arg | awk '{ print $1 }'`"
    i=1
    while test "`expr $i \<= $len`" = "1"; do
        config="`head -$i $config_arg | tail -1`"
        echo "$config" >> "$config_list"
        dir="$root/config-$i"
        try_build "$dir" "$dir/install" "$config"
        i="`expr $i + 1`"
    done
fi

# did we want vpath builds?
if test -n "$vpath_arg"; then
    dir="$root/vpath-relative"
    try_build "$dir" "$dir/install" "CFLAGS=-g" relative
    echo "relative vpath default" >> "$config_list"

    dir="$root/vpath-absolute"
    try_build "$dir" "$dir/install" "CFLAGS=-g" absolute
    echo "absolute vpath default" >> "$config_list"
fi

# trim the downloads dir to $max_snapshots
cd "$scratch_root_arg/downloads"
for ext in gz; do
    count="`ls openmpi*.tar.$ext | wc -l | awk '{ print $1 }'`"
    if test "`expr $count \> $max_snapshots`" = "1"; then
        num_old="`expr $count - $max_snapshots`"
        old="`ls -rt openmpi*.tar.$ext | head -$num_old`"
        rm -f $old
    fi
done

# send success mail
Mail -s "$success_subject" "$email_arg" <<EOF
Building nightly snapshot SVN tarball was a success.

Snapshot:   $version
Start time: $start_time
End time:   `date`

Host:       `hostname`
Platform:   $config_guess

Configurations built:

---------------------------------------------------------------------------
`cat $config_list`
---------------------------------------------------------------------------

Your friendly daemon,
Cyrador
EOF

# all done
rm -rf "$root"
rm -f "$config_list"
exit 0
