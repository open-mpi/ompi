#!/bin/env perl
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

use strict;
use Data::Dumper;

# Set this to true for additional output; typically only when
# debugging

my $debug = 1;

# do you want a success mail?
my $want_success_mail = 1;

# download "latest" filename
my $latest_name = "latest_snapshot.txt";

# checksum filenames
my $md5_checksums = "md5sums.txt";
my $sha1_checksums = "sha1sums.txt";

# max length of logfile to send in an e-mail
my $max_log_len = 100;

# email subjects
my $success_subject = "Success";
my $fail_subject = "=== TEST FAILURE ===";

# max number of snapshots to keep downloaded
my $max_snapshots = 3;

############################################################################
# Shouldn't need to change below this line
############################################################################

#--------------------------------------------------------------------------

# send a mail
sub send_mail {
    my ($subject, $to) = @_;
    shift;
    shift;
    my $msg = \@_;

    our $mail;

    open MAIL, "|$mail -s \"$subject\" \"$to\"" ||
        die "Could not open pipe to output e-mail\n";
    my $i = 0;
    while ($i <= $#$msg) {
        print MAIL $$msg[$i];
        ++$i;
    }
    close MAIL;
}

#--------------------------------------------------------------------------

# abort the script, and send an error e-mail as a result
sub test_abort {
    my $msg = \@_;

    our @email_output;
    our $email_arg;

    push(@email_output, "Building the nightly tarball ended in error:\n\n");
    push(@email_output, @$msg);
    send_mail($fail_subject, $email_arg, @email_output);
    exit(1);
}

#--------------------------------------------------------------------------

# run a command and save the stdout / stderr
sub do_command {
    my ($cmd) = @_;

    print "*** Running command: $cmd\n" if ($debug);
    pipe OUTread, OUTwrite;
    pipe ERRread, ERRwrite;

    # Child

    my $pid;
    if (($pid = fork()) == 0) {
#        close OUTread;
#        close ERRread;

        close(STDERR);
        open STDERR, ">&ERRwrite" ||
            die "Can't redirect stderr\n";
        select STDERR;
        $| = 1;

        close(STDOUT);
        open STDOUT, ">&OUTwrite" || 
            die "Can't redirect stdout\n";
        select STDOUT;
        $| = 1;

        exec(split(' ', $cmd)) || 
            die "Can't execute command: $cmd\n";
    }
    close OUTwrite;
    close ERRwrite;

    # Parent

    my (@out, @err);
    my ($rin, $rout);
    my $done = 2;

    $rin = '';
    vec($rin, fileno(OUTread), 1) = 1;
    vec($rin, fileno(ERRread), 1) = 1;

    while ($done > 0) {
        my $nfound = select($rout = $rin, undef, undef, undef);
        if (vec($rout, fileno(OUTread), 1) == 1) {
            my $data = <OUTread>;
            if (!defined($data)) {
                vec($rin, fileno(OUTread), 1) = 0;
                --$done;
            } else {
                push(@out, $data);
                print "OUT:$data" if ($debug);
            }
        }

        if (vec($rout, fileno(ERRread), 1) == 1) {
            my $data = <ERRread>;
            if (!defined($data)) {
                vec($rin, fileno(ERRread), 1) = 0;
                --$done;
            } else {
                push(@err, $data);
                print "ERR:$data" if ($debug);
            }
        }
    }
    close OUTerr;
    close OUTread;

    waitpid($pid, 0);
    my $status = $?;
    print "*** Command complete, exit status: $status\n" if ($debug);

    # Return an anonymous hash containing the relevant data

    return {
        stdout => \@out,
        stderr => \@err,
        status => $status
        };
}

#--------------------------------------------------------------------------

# find a program from a list and load it into the target variable
sub find_program {
    my @names = @_;

    # loop through the list and save the first one that we find
    my $i = 0;
    while ($i <= $#names) {
        my $ret = system("which $names[$i] 2>&1 >/dev/null");
        my $status = $ret >> 8;
        if ($status == 0) {
            return $names[$i];
        }
    }
    return undef;
}

#--------------------------------------------------------------------------

sub trim {
    my ($max, $msg) = @_;

    my @out;

    my $i = 0;
    if ($#$msg > $max) {
        $i = $#$msg - $max + 1;
        push(@out, "...previous $i lines snipped...\n");
    }
    while ($i <= $#$msg) {
        push(@out, $$msg[$i]);
        ++$i;
    }
    \@out;
}

#--------------------------------------------------------------------------

#
# main
#

# grab the args
my $scratch_root_arg = $ARGV[0];
my $email_arg = $ARGV[1];
my $url_arg = $ARGV[2];
my $config_arg = $ARGV[3];
my $vpath_arg = $ARGV[4];

# global vars
our @email_output;
my $ret;

# Sanity checks
if (-z $scratch_root_arg || -z $email_arg || -z $url_arg) {
    print "Must specify scratch root directory, e-mail address, and URL_ARG
Can also optionally specify a config file and whether want VPATH building\n";
    exit(1);
}

# prefix the e-mail
my $unamen = `uname -n`;
my $unameo = `uname -o`;
my $unamer = `uname -r`;
my $unamem = `uname -m`;
chomp($unamen);
chomp($unameo);
chomp($unamer);
chomp($unamem);
push(@email_output, "Host: $unamen / $unameo / $unamer / $unamem\n");

# Find a mail program
our $mail = find_program(qw(Mail mailx mail));
print "Found mail: $mail\n";
die "Could not find mail program; aborting in despair\n"
    if (!defined($mail));

# figure out what download command to use
my $download = find_program(qw(wget lynx curl));
test_abort("Cannot find downloading program -- aborting in despair\n")
    if (!defined($download));

# move into the scratch directory, and ensure we have an absolute path
# for it
if (! -d $scratch_root_arg) {
    mkdir($scratch_root_arg);
}
test_abort("Could not cd to scratch root: $scratch_root_arg\n")
    if (! -d $scratch_root_arg);
chdir($scratch_root_arg);
$scratch_root_arg = `pwd`;
chomp($scratch_root_arg);

# ensure some subdirs exist
foreach my $dir (qw(downloads)) {
    if (! -d $dir) {
        mkdir($dir);
    } 
}

# get the latest snapshot version number
chdir("downloads");
unlink($latest_name);
do_command("$download $url_arg/$latest_name");
test_abort("Could not download latest snapshot number -- aborting")
    if (! -f $latest_name);
my $version = `cat $latest_name`;
chomp($version);
push(@email_output, "Snapshot: $version\n\n");

# see if we need to download the tarball
my $tarball_name = "openmpi-$version.tar.gz";
if (! -f $tarball_name) {
    do_command("$download $url_arg/$tarball_name");
    test_abort "Could not download tarball -- aborting"
        if (! -f $tarball_name);

    # get the checksums
    unlink($md5_checksums);
    do_command("$download $url_arg/$md5_checksums");

    unlink($sha1_checksums);
    do_command("$download $url_arg/$sha1_checksums");
}

# compare the md5sum
my $md5_file = `grep $version.tar.gz $md5_checksums`;
chomp($md5_file);
my $md5sum = find_program(qw(md5sum));
if (!defined($md5sum)) {
    push(@email_output,
"WARNING: Could not find md5sum executable, so I will not be able to check
WARNING: the validity of downloaded executables against their known MD5 
WARNING: checksums.  Proceeding anyway...\n\n");
} elsif (! $md5_file) {
    push(@email_output,
"WARNING: Could not find md5sum check file, so I will not be able to check
WARNING: the validity of downloaded executables against their known MD5 
WARNING: checksums.  Proceeding anyway...\n\n");
} else {
    my $md5_actual = `$md5sum $tarball_name 2>&1`;
    chomp($md5_actual);
    test_abort("md5sum from checksum file does not match actual ($md5_file != $md5_actual)")
        if ($md5_file ne $md5_actual);
}

# compare the sha1sum
my $sha1_file = `grep $version.tar.gz $sha1_checksums`;
chomp($sha1_file);
my $sha1sum = find_program(qw(sha1sum));
if (!defined($sha1sum)) {
    push(@email_output,
"WARNING: Could not find sha1sum executable, so I will not be able to check
WARNING: the validity of downloaded executables against their known SHA1
WARNING: checksums.  Proceeding anyway...\n\n");
} elsif (! $sha1_file) {
    push(@email_output,
"WARNING: Could not find sha1sum check file, so I will not be able to check
WARNING: the validity of downloaded executables against their known SHA1
WARNING: checksums.  Proceeding anyway...\n\n");
} else {
    my $sha1_actual = `$sha1sum $tarball_name 2>&1`;
    chomp($sha1_actual);
    test_abort "sha1sum from checksum file does not match actual ($sha1_file != $sha1_actual)"
        if ($sha1_file ne $sha1_actual);
}


# subroutine for building a single configuration
sub try_build {
    my ($srcroot, $installdir, $confargs, $vpath_mode) = @_;
    my $ret;
    my $startdir = `pwd`;
    chomp($startdir);

    # make the source root
    if (! -d $srcroot) {
        mkdir($srcroot);
    }
    chdir($srcroot);

    # save what we're testing
    my $name = "[default]";
    $name = $confargs
        if ($confargs);

    # expand the tarball (do NOT assume GNU tar).  don't use
    # do_command here because we need to use a pipe.  This isn't an
    # "interesting" command, anyway -- we don't need any stdout or
    # stderr.  So if it fails, it fails -- no big deal.

    system("gunzip -c $scratch_root_arg/downloads/$tarball_name | tar xf -");
    my $status = $? >> 8;
    if ($status != 0) {
        return {
            status => $status,
            message => "Failed to unzip the tarball",
        }
    }
    chdir("openmpi-$version");

    # configure it
    my $config_command = "./configure";
    if ($vpath_mode) {
        mkdir("vpath_build");
        chdir("vpath_build");
        if ($vpath_mode eq "relative") {
            $config_command = "../configure";
        } else {
            $config_command = "$srcroot/openmpi-$version/configure";
        }
    }
    $ret = do_command("$config_command --prefix=$installdir $confargs");
    if ($ret->{status} != 0) {
        $ret->{message} = "Failed to unzip the tarball";
        return $ret;
    }

    # build it
    $ret = do_command("make all");
    if ($ret->{status} != 0) {
        $ret->{message} = "Failed to \"make all\"";
        return $ret;
    }

    # save the compile warnings
    my $make_all_stderr = $ret->{stderr};

    # install it
    $ret = do_command("make install");
    if ($ret->{status} != 0) {
        $ret->{make_all_stderr} = $make_all_stderr;
        $ret->{message} = "Failed to \"make install\"";
        return $ret;
    }

    # try compiling and linking a simple C application
    chdir("..");
    if (! -d test) {
        mkdir("test");
    }
    chdir("test");
    open C, ">hello.c";
    print C "#include <mpi.h>
int main(int argc, char* argv[]) {
  MPI_Init(&argc, &argv);
  MPI_Finalize();
  return 0;
}\n";
    $ret = do_command("$installdir/bin/mpicc hello.c -o hello");
    if ($ret->{status} != 0) {
        $ret->{make_all_stderr} = $make_all_stderr;
        $ret->{message} = "Failed to compile/link C \"hello world\" MPI app";
        return $ret;
    }
    unlink("hello");
    print "COMPILE: C ok\n";

    # if we have a C++ compiler, try compiling and linking a simple
    # C++ application
    open INFO, "$installdir/bin/ompi_info --parsable|";
    my @have_cxx = grep { /^bindings:cxx:/ } <INFO>;
    chomp @have_cxx;
    close INFO;
    if ($have_cxx[0] eq "bindings:cxx:yes") {
        open CXX, ">hello.cc";
        print CXX "#include <mpi.h>
int main(int argc, char* argv[]) {
  MPI::nit(argc, argv);
  MPI::Finalize();
  return 0;
}\n";
        do_command("$installdir/bin/mpic++ hello.cc -o hello");
        if ($ret->{status} != 0) {
            $ret->{make_all_stderr} = $make_all_stderr;
            $ret->{message} =
                "Failed to compile/link C++ \"hello world\" MPI app";
            return $ret;
        }
        unlink("hello");
        print "COMPILE: CXX ok\n";
    }

    # if we have a F77 compiler, try compiling and linking a simple
    # F77 application
    open INFO, "$installdir/bin/ompi_info --parsable|";
    my @have_f77 = grep { /^bindings:f77:/ } <INFO>;
    chomp(@have_f77);
    close INFO;
    if ($have_f77[0] eq "bindings:f77:yes") {
        open F77, ">hello.f";
        print F77 "C
        program main
        include 'mpif.h'
        call MPI_INIT(ierr)
        call MPI_FINALIZE(ierr)
        stop
        end\n";
        do_command("$installdir/bin/mpif77 hello.f -o hello");
        if ($ret->{status} != 0) {
            $ret->{make_all_stderr} = $make_all_stderr;
            $ret->{message} = 
                "Failed to compile/link F77 \"hello world\" MPI app";
            return $ret;
        }
        unlink("hello");
        print "COMPILE: F77 ok\n";
    }

    # all done -- clean up
    chdir($startdir);
    system("rm -rf $srcroot");
    return {
        make_all_stderr => $make_all_stderr,
        status => 0,
    };
}

# Make a root for this build to play in (scratch_root_arg is absolute, so
# root will be absolute)
my $root= "$scratch_root_arg/build-$version";
system("rm -rf $root");
mkdir($root);
chdir($root);

# loop over all configurations
# be lazy: if no configurations supplied and no vpath, do a default
# configure/build
my $results;
my $do_default = 0;
if (! $config_arg || ! -f $config_arg) {
    if (! $vpath_arg) {
        $do_default = 1;
    }
}
if ($do_default) {
    my $dir = "$root/default";
    my $name = "[default]";
    my $config = "CFLAGS=-g --disable-f77 --enable-debug";
    $ret = try_build($dir, "$dir/install", $config, "");
    $results->{$name} = $ret;
    $results->{$name}->{config} = $config;
} elsif (-f $config_arg) {
    open CONF, "$config_arg";
    my $i = 1;
    while (<CONF>) {
        my $line = $_;
        chomp($line);
        my ($name, $config) = split(/:/, $line);
        if (! $config) {
            $config = $name;
        }
        if (! $config && ! $name) {
            $name = "[default]";
            $config = "CFLAGS=-g";
        }

        my $dir = "$root/config-$i";
        $ret = try_build($dir, "$dir/install", "$config", "");
        $results->{$name} = $ret;
        $results->{$name}->{config} = $config;
        ++$i;
    }
    close CONF;
}

# did we want vpath builds?
if ($vpath_arg) {
    my $config = "CFLAGS=-g";
    my ($dir, $name);

    $dir = "$root/vpath-relative";
    $name = "vpath-relative";
    $ret = try_build($dir, "$dir/install", $config, "relative");
    $results->{$name} = $ret;
    $results->{$name}->{config} = $config;

    $dir = "$root/vpath-absolute";
    $name = "vpath-absolute";
    $ret = try_build($dir, "$dir/install", $config, "absolute");
    $results->{$name} = $ret;
    $results->{$name}->{config} = $config;
}

# trim the downloads dir to $max_snapshots
my $dir = "$scratch_root_arg/downloads";
opendir(DOWNLOADS, $dir);
my @tarballs = sort(grep { /^openmpi.+gz\$/ && -f "$dir/$_" } readdir(DOWNLOADS));
closedir(DOWNLOADS);
my $i = $#tarballs;
while ($i > $max_snapshots) {
    unlink($tarballs[$i]);
    ++$i;
}

# send success mail
push(@email_output,
"Summary of results:
Building tarball and compiling/linking MPI \"hello world\"
--------------------------------------------------------------------------\n");
foreach my $config (keys(%$results)) {
    my $str;
    if ($results->{$config}->{status} == 0) {
        $str = "Success   ";
    } else {
        $str = "FAILURE   ";
    }
    $str .= $config . "\n";
    push(@email_output, $str);
}

push(@email_output,
"--------------------------------------------------------------------------
\n");


# Include additional details if relevant
print Dumper($results);

my $displayed = 0;
foreach my $config (keys(%$results)) {
    my $output = 0;
    my $str = "Build: " . $config . "
Config options: " . $results->{$config}->{config} . "
Result: " . ($results->{$config}->{status} == 0 ?
                     "Success" : "FAILURE") . "\n";

    if ($results->{$config}->{status} != 0 &&
        $results->{$config}->{stdout} &&
        $#{$results->{$config}->{stdout}} >= 0) {
        push(@email_output, $str);
        $displayed = $output = 1;

        push(@email_output, 
"--Standard output---------------------------------------------------------\n");
        my $tmp = trim($max_log_len, $results->{$config}->{stdout});
        push(@email_output, @$tmp);
        push(@email_output, 
"--Standard output end-----------------------------------------------------\n");
    }
    if ($results->{$config}->{stderr} &&
        $#{$results->{$config}->{stderr}} >= 0) {
        push(@email_output, $str)
            if ($output == 0);
        $displayed = $output = 1;

        push(@email_output, 
"--Standard error----------------------------------------------------------\n");
        my $tmp = trim($max_log_len, $results->{$config}->{stderr});
        push(@email_output, @$tmp);
        push(@email_output, 
"--Standard error end------------------------------------------------------\n");
    }
    if ($results->{$config}->{make_all_stderr} &&
        $#{$results->{$config}->{make_all_stderr}} >= 0) {
        push(@email_output, $str)
            if ($output == 0);
        $displayed = $output = 1;

        push(@email_output, 
"--\"make all\" standard error---------------------------------------------------\n");
        my $tmp = trim($max_log_len, $results->{$config}->{make_all_stderr});
        push(@email_output, @$tmp);
        push(@email_output, 
"--\"make all\" standard error end-----------------------------------------------\n");
    }

    if ($output == 1) {
        push(@email_output,
"\n==========================================================================\n\n");
    }
}

# if we didn't display anything extra, then say so

if (!$displayed) {
    push(@email_output, "No additional interesting intformation\n\n");
}

push(@email_output, "Your friendly server,\nCyrador\n");
send_mail($success_subject, $email_arg, @email_output);

exit(0);
