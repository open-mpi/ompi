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
#

use strict;
use Getopt::Long;

# Set this to true for additional output; typically only when
# debugging
our $debug = 1;

# download "latest" filename
my $latest_name = "latest_snapshot.txt";

# checksum filenames
my $md5_checksums = "md5sums.txt";
my $sha1_checksums = "sha1sums.txt";

# max length of logfile to send in an e-mail
my $max_log_len = 100;

# email subjects
my $success_subject = "Success (\@version@)";
my $fail_subject = "=== TEST FAILURE (\@version@) ===";

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
    our $version;

    $subject =~ s/\@version\@/$version/;

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
        close OUTread;
        close ERRread;

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
        push(@out, "[...previous $i lines snipped; last $max lines shown...]\n");
    }
    while ($i <= $#$msg) {
        push(@out, $$msg[$i]);
        ++$i;
    }
    if (! ($out[$#out] =~ /\n/)) {
        push(@out, "\n");
    }
    \@out;
}

#--------------------------------------------------------------------------

# subroutine for building a single configuration
sub try_build {
    my ($tarball, $srcroot, $installdir, $vpath_mode, $confargs) = @_;
    my $ret;
    my $startdir = `pwd`;
    our $version;
    our $scratch_root_arg;

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

    system("gunzip -c $tarball | tar xf -");
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
  MPI::Init(argc, argv);
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

#--------------------------------------------------------------------------

#
# main
#

# parse the command line
our $scratch_root_arg;
our $email_arg;
our $url_arg;
our $config_arg;
our $debug_arg;
our $file_arg;
my $help_arg;

&Getopt::Long::Configure("bundling", "require_order");
my $ok = Getopt::Long::GetOptions("url|u=s" => \$url_arg,
                                  "scratch|s=s" => \$scratch_root_arg,
                                  "email|e=s" => \$email_arg,
                                  "config|c=s" => \$config_arg,
                                  "file|f=s" => \$file_arg,
                                  "debug|d" => \$debug_arg,
                                  "help|h" => \$help_arg,
                                  );

if (!$ok || $help_arg) {
    print("Command line error\n") 
        if (!$ok);
    print "Usage: $0 [--scratch|-s scratch_directory_root] [--email|-e address]\n";
    print "[--config|-c config_file] [--help|-h] [--debug|-d]\n";
    print "[[--file|-f local_ompi_tarball] [--url|-u URL_base]]\n";
    exit(0);
}

if ($file_arg && $url_arg) {
    print("ERROR: Both --url and --file specified; which should I do?\n");
    die("Aborting in confusion!\n");
} elsif (! $file_arg && ! $url_arg) {
    print("ERROR: Neither --url nor --file specified; what should I do?\n");
    die("Aborting in confusion!\n");
} elsif (! $email_arg) {
    print("ERROR: Need e-mail address to mail results\n");
    die("Aborting in despair\n");
} elsif (! $scratch_root_arg) {
    my $tmp = "/tmp";
    $tmp = $ENV{TMPDIR}
        if ($ENV{TMPDIR});
    $scratch_root_arg = "$tmp/open-mpi-test-build.$$";
}

# if the --file argument was a relative file, convert it to absolute
# because we're going to chdir before using it
if ($file_arg) {
    my $foo = $file_arg =~ /^\//;
    if (! ($file_arg =~ /^\//)) {
        my $tmp = `pwd`;
        chomp($tmp);
        $file_arg = "$tmp/$file_arg";
    }
}

# ditto for --config arg
if ($config_arg) {
    my $foo = $config_arg =~ /^\//;
    if (! ($config_arg =~ /^\//)) {
        my $tmp = `pwd`;
        chomp($tmp);
        $config_arg = "$tmp/$config_arg";
    }
}

# turn on debugging?
$debug = 1
    if ($debug_arg);

# global vars
our @email_output;
my $ret;

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

# if we were given a URL base, get the latest snapshot version number
our $tarball_name;
our $version;
if ($url_arg) {
    chdir("downloads");
    unlink($latest_name);
    do_command("$download $url_arg/$latest_name");
    test_abort("Could not download latest snapshot number -- aborting")
        if (! -f $latest_name);
    $version = `cat $latest_name`;
    chomp($version);
    push(@email_output, "Snapshot: $version\n\n");
    
    # see if we need to download the tarball
    $tarball_name = "openmpi-$version.tar.gz";
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

    # now adjust the tarball name to be absolute
    $tarball_name = "$scratch_root_arg/downloads/$tarball_name";
} elsif ($file_arg) {
    $tarball_name = $file_arg;
    system("pwd");
    die "ERROR: Cannot read file $tarball_name\n"
        if (! -r $tarball_name);
    $version = $tarball_name;
    $version =~ s/.*openmpi-(.+).tar.gz/$1/;
} 

# Make a root for this build to play in (scratch_root_arg is absolute, so
# root will be absolute)
my $root= "$scratch_root_arg/build-$version";
system("rm -rf $root");
mkdir($root);
chdir($root);

# loop over all configurations
# be lazy: if no configurations supplied, do a default
# configure/build
my $results;
my $do_default = 0;
if (! $config_arg || ! -f $config_arg) {
    my $dir = "$root/default";
    my $name = "[default]";
    my $config = "CFLAGS=-g --disable-f77 --enable-debug";
    $ret = try_build($tarball_name, $dir, "$dir/install", "", $config);
    $results->{$name} = $ret;
    $results->{$name}->{config} = $config;
    $results->{$name}->{want_stderr} = 1;
    $results->{$name}->{vpath_mode} = "";
} else {
    open CONF, "$config_arg";
    my $i = 1;
    while (<CONF>) {
        my $line = $_;
        chomp($line);
        my ($name, $want_stderr, $vpath_mode, $config) = split(/:/, $line);
        if (! $config) {
            $config = $name;
        }
        if (! $config && ! $name) {
            $name = "[default]";
            $config = "CFLAGS=-g";
        }

        my $dir = "$root/config-$i";
        $ret = try_build($tarball_name, $dir, "$dir/install",
                         $vpath_mode, $config);
        $results->{$name} = $ret;
        $results->{$name}->{config} = $config;
        $results->{$name}->{want_stderr} = $want_stderr;
        $results->{$name}->{vpath_mode} = $vpath_mode;
        ++$i;
    }
    close CONF;
}

# trim the downloads dir to $max_snapshots
my $dir = "$scratch_root_arg/downloads";
opendir(DOWNLOADS, $dir);
my @tarballs = sort(grep { /^openmpi.+gz\$/ && -f "$dir/$_" } readdir(DOWNLOADS));
closedir(DOWNLOADS);
my $i = $#tarballs;
while ($i > $max_snapshots) {
    print "UNLINKING: $tarballs[$i]\n";
    unlink($tarballs[$i]);
    ++$i;
}

# send success mail
my $email_subject;
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
        $email_subject = $fail_subject;
    }
    $str .= $config . "\n";
    push(@email_output, $str);
}
$email_subject = $success_subject
    if (!$email_subject);

push(@email_output,
"--------------------------------------------------------------------------
\n");


# Include additional details if relevant
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
    if ($results->{$config}->{want_stderr} &&
        $results->{$config}->{make_all_stderr} &&
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
send_mail($email_subject, $email_arg, @email_output);

exit(0);
