#!/usr/bin/env perl
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

use strict;
use Getopt::Long;

# Set this to true for additional output; typically only when
# debugging
my $debug = 0;

#
# Set this to true, if You want to disable checking
# Useful, if the check might fail on Your machine, but you
# nevertheless want to compile/debug other tests.
#
my $nocheck = 0;

#
# Set this to the installation directory
# (possibly accesible by all nodes in a cluster).
#
my $install_dir;

# download "latest" filename
my $latest_name = "latest_snapshot.txt";

# checksum filenames
my $md5_checksums = "md5sums.txt";
my $sha1_checksums = "sha1sums.txt";

# max length of logfile to send in an e-mail
my $max_log_len = 100;

# email subjects
my $success_subject = "Open MPI-TESTS: Build success (\@version@)";
my $fail_subject = "Open MPI-TESTS: === BUILD FAILURE (\@version@) ===";

# max number of snapshots to keep downloaded
my $max_snapshots = 3;

# max number of old build roots to maintain (note that build roots are
# only left if a) you use --leave-install or b) a build fails)
my $max_build_roots = 3;

my $file_base_name="openmpi-tests";

my $outfile;
my $outfile_arg;

############################################################################
# Shouldn't need to change below this line
############################################################################

# Need to define some globals here -- can't use "our" because we have
# to run on machines with older versions of perl.  #$%#@$%#...

my $version;
my $mail;
my @email_output;
my $tarball_name;
my $ret;
my $last_test_version_name;

my $scratch_root_arg;
my $email_arg;
my $url_arg;
my $config_arg;
my $debug_arg;
my $file_arg;
my $leave_install_arg;
my $help_arg;
my $force_arg;
my $nocheck_arg;
my $install_dir_arg;

#--------------------------------------------------------------------------

# send a mail
sub send_mail {
    my ($subject, $to) = @_;
    shift;
    shift;
    my $msg = \@_;

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

    push(@email_output, "Building the nightly test-tarball ended in error:\n\n");
    push(@email_output, @$msg);
    send_mail($fail_subject, $email_arg, @email_output);
    exit(1);
}

#--------------------------------------------------------------------------

# check to see if we've tested this version before
sub check_last_version {
    my ($new_version) = @_;
    my $old_version;

    print "This version: $new_version\n" if ($debug);
    if (! -f $last_test_version_name) {
        print "Never tested on this system before\n" if ($debug);
        return;
    }

    $old_version = `cat $last_test_version_name`;
    chomp($old_version);
    print "Last tested version: $old_version\n" if ($debug);

    # If the version hasn't changed since the last time we tested,
    # then don't bother testing again.

    if ($new_version eq $old_version) {
        if ($debug || $force_arg) {
            print "Test tarball version is the same as the last version we tested\nOnly testing again because we're in --debug or --force mode\n"
                if ($debug);
        } else {
            exit(0);
        }
    }
}

#--------------------------------------------------------------------------

# run a command and save the stdout / stderr
sub do_command {
    my ($merge_output, $cmd) = @_;

    print "*** Running command: $cmd\n" if ($debug);
    pipe OUTread, OUTwrite;
    pipe ERRread, ERRwrite
        if (!$merge_output);

    # Child

    my $pid;
    if (($pid = fork()) == 0) {
        close OUTread;
        close ERRread
            if (!$merge_output);

        close(STDERR);
        if ($merge_output) {
            open STDERR, ">&OUTwrite" ||
                die "Can't redirect stderr\n";
        } else {
            open STDERR, ">&ERRwrite" ||
                die "Can't redirect stderr\n";
        }
        select STDERR;
        $| = 1;

        close(STDOUT);
        open STDOUT, ">&OUTwrite" || 
            die "Can't redirect stdout\n";
        select STDOUT;
        $| = 1;

        # Turn shell-quoted words ("foo bar baz") into individual tokens

        my @tokens;
        while ($cmd =~ /\".*\"/) {
            my $prefix;
            my $middle;
            my $suffix;
            
            $cmd =~ /(.*?)\"(.*?)\"(.*)/;
            $prefix = $1;
            $middle = $2;
            $suffix = $3;
            
            if ($prefix) {
                foreach my $token (split(' ', $prefix)) {
                    push(@tokens, $token);
                }
            }
            if ($middle) {
                push(@tokens, $middle);
            } else {
                push(@tokens, "");
            }
            $cmd = $suffix;
        }
        if ($cmd) {
            push(@tokens, split(' ', $cmd));
        }

        # Run it!

        exec(@tokens) ||
            die "Can't execute command: $cmd\n";
    }
    close OUTwrite;
    close ERRwrite
        if (!$merge_output);

    # Parent

    my (@out, @err);
    my ($rin, $rout);
    my $done = $merge_output ? 1 : 2;

    # Keep watching over the pipe(s)

    $rin = '';
    vec($rin, fileno(OUTread), 1) = 1;
    vec($rin, fileno(ERRread), 1) = 1
        if (!$merge_output);

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

        if (!$merge_output && vec($rout, fileno(ERRread), 1) == 1) {
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
    close OUTread
        if (!$merge_output);

    # The pipes are closed, so the process should be dead.  Reap it.

    waitpid($pid, 0);
    my $status = $?;
    print "*** Command complete, exit status: $status\n" if ($debug);

    # Return an anonymous hash containing the relevant data

    my $ret = {
        stdout => \@out,
        status => $status
        };

    # If we had stderr, return that, too

    $ret->{stderr} = \@err
        if (!$merge_output);
    return $ret;
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
        ++$i;
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
sub try_untar {
    my ($name, $merge_output, $tarball, $srcroot, $installdir,
        $vpath_mode, $confargs) = @_;
    my $ret;
    my $startdir = `pwd`;
    my $unpacker;
    my $status;

    chomp($startdir);

    # make the source root
    if (! -d $srcroot) {
        mkdir($srcroot, 0777);
    }
    chdir($srcroot);

    # expand the tarball (do NOT assume GNU tar).  don't use
    # do_command here because we need to use a pipe.  This isn't an
    # "interesting" command, anyway -- we don't need any stdout or
    # stderr.  So if it fails, it fails -- no big deal.

    if ($tarball =~ /.*\.bz2$/) {
        $unpacker="bunzip2";
    }
    elsif ($tarball =~ /.*\.gz$/) {
        $unpacker="gunzip";
    }
    else {
        return {
            status => $status,
            message => "Failed to figure out compression of tarball",
        }
    }

    system("$unpacker -c $tarball | tar xf -");
    $status = $? >> 8;
    if ($status != 0) {
        return {
            status => $status,
            message => "Failed to unzip the tarball",
        }
    }

    test_abort ("Tarball does not contain expected directory:$file_base_name-$version -- aborting")
	    if (! -d "$file_base_name-$version");
    chdir("$file_base_name-$version");
}
    
sub try_configure {
    my ($name, $merge_output, $tarball, $srcroot, $installdir,
        $vpath_mode, $confargs) = @_;
    my $ret;

    # configure it
    my $config_command = "./configure";
    if ($vpath_mode) {
        mkdir("vpath_build", 0777);
        chdir("vpath_build");
        if ($vpath_mode eq "relative") {
            $config_command = "../configure";
        }
#
# Disable absolute for now
#         else {
#            $config_command = "$srcroot/$file_base_name-$version/configure";
#        }
    }

    $ret = do_command(1, "$config_command $confargs --prefix=$installdir");
    if ($ret->{status} != 0) {
        $ret->{message} = "Failed to configure the tarball";
        return $ret;
    }

    # build it
    $ret = do_command($merge_output, "make all");
    if ($ret->{status} != 0) {
        $ret->{message} = "Failed to \"make all\"";
        return $ret;
    }

    # save the compile warnings
    my $make_all_stderr = $ret->{stderr};

    # and check it, if the user does not disable it
    if ( $nocheck == 0 ) {
      $ret = do_command($merge_output, "make check");
      if ($ret->{status} != 0) {
          $ret->{make_all_stderr} = $make_all_stderr;
          $ret->{message} = "Failed to \"make check\"";
          return $ret;
      }
    }

    # install it
    $ret = do_command(1, "make install");
    if ($ret->{status} != 0) {
        $ret->{make_all_stderr} = $make_all_stderr;
        $ret->{message} = "Failed to \"make install\"";
        return $ret;
    }

    if ($vpath_mode) {
      chdir ("..");
    }

    return {
        make_all_stderr => $make_all_stderr,
        status => 0,
    };
}


sub cleanup {
    my ($name, $merge_output, $tarball, $srcroot, $installdir,
        $vpath_mode, $confargs) = @_;
    my $startdir=`pwd`;

    # all done -- clean up (unless user specified --leave-install)
    chdir($startdir);
    if ($leave_install_arg) {
        system("rm -rf $srcroot/openmpi* $srcroot/test");
    } else {
        system("rm -rf $srcroot");
    }
}

#--------------------------------------------------------------------------

#
# main
#

# parse the command line
&Getopt::Long::Configure("bundling", "require_order");
my $ok = Getopt::Long::GetOptions("url|u=s" => \$url_arg,
                                  "scratch|s=s" => \$scratch_root_arg,
                                  "email|e=s" => \$email_arg,
                                  "config|c=s" => \$config_arg,
                                  "file|f=s" => \$file_arg,
                                  "debug|d" => \$debug_arg,
                                  "leave-install|l=s" => \$leave_install_arg,
                                  "install-dir=s" => \$install_dir_arg,
                                  "outfile=s" => \$outfile_arg,
                                  "help|h" => \$help_arg,
                                  "force" => \$force_arg,
                                  "nocheck" => \$nocheck_arg
                                  );

if (!$ok || $help_arg) {
    print("Command line error\n") 
        if (!$ok);
    print "Usage: $0 [--scratch|-s scratch_directory_root] [--email|-e address]\n";
    print "  [--config|-c config_file] [--help|-h] [--debug|-d]\n";
    print "  [[--file|-f local_ompi_tarball] [--url|-u URL_base]]\n";
    print "  [--leave-install output_filename] [--outfile output_filename]\n";
    print "  [--install-dir install_dir] [--nocheck]\n";
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
    if (! ($file_arg =~ /^\//)) {
        my $tmp = `pwd`;
        chomp($tmp);
        $file_arg = "$tmp/$file_arg";
    }
}

# ditto for --config arg
if ($config_arg) {
    if (! ($config_arg =~ /^\//)) {
        my $tmp = `pwd`;
        chomp($tmp);
        $config_arg = "$tmp/$config_arg";
    }
}

# turn on debugging?
$debug = 1
    if ($debug_arg);

# Disable checking?
$nocheck = 1
    if ($nocheck_arg);

# prefix the e-mail
my $str = "Host: ";
my $first = 1;
foreach my $option (qw/n o r m/) {
    my $out = `uname -$option 2>/dev/null`;
    chomp($out);
    if ($out) {
        $str .= " / "
            if (! $first);
        $str .= $out;
        $first = 0;
    }
}
push(@email_output, $str . "\n");

# now that we have scratch root, fill in the filename for the last
# version that we tested -- separate it by hostname and config name so
# that multiple instances of this script can share a downloads
# directory
my $dir = "$scratch_root_arg/last-test-versions";
if (! -d $dir) {
    mkdir($dir);
}
my $hostname = `uname -n`;
chomp($hostname);
my $config = $config_arg ? `basename $config_arg` : "default";
chomp($config);
$last_test_version_name = "$dir/$hostname-$config";

# Find a mail program
$mail = find_program(qw(Mail mailx mail));
die "Could not find mail program; aborting in despair\n"
    if (!defined($mail));

# figure out what download command to use
# if we need to download a version -- with file_arg, we do not
my $download = "true";
if ($url_arg) {
    $download = find_program(qw(wget lynx curl));
    test_abort("Cannot find downloading program -- aborting in despair\n")
        if (!defined($download));
}

# move into the scratch directory, and ensure we have an absolute path
# for it
if (! -d $scratch_root_arg) {
    mkdir($scratch_root_arg, 0777);
}
test_abort("Could not cd to scratch root: $scratch_root_arg\n")
    if (! -d $scratch_root_arg);
chdir($scratch_root_arg);
$scratch_root_arg = `pwd`;
chomp($scratch_root_arg);

# if we were given a URL base, get the latest snapshot version number
if ($url_arg) {

    # ensure some subdirs exist
    foreach my $dir (qw(downloads)) {
      if (! -d $dir) {
          mkdir($dir, 0777);
      }
    }

    chdir("downloads");

    unlink($latest_name);
    do_command(1, "$download $url_arg/$latest_name");
    test_abort("Could not download latest snapshot number -- aborting")
        if (! -f $latest_name);
    $version = `cat $latest_name`;
    chomp($version);

    check_last_version($version);

    push(@email_output, "Snapshot: $version\n\n");

    # see if we need to download the tarball
    $tarball_name = "$file_base_name-$version.tar.gz";
    if (! -f $tarball_name) {
        do_command(1, "$download $url_arg/$tarball_name");
        test_abort ("Could not download tarball -- aborting")
            if (! -f $tarball_name);
        
        # get the checksums
        unlink($md5_checksums);
        do_command(1, "$download $url_arg/$md5_checksums");
        
        unlink($sha1_checksums);
        do_command(1, "$download $url_arg/$sha1_checksums");
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

    die "ERROR: Cannot read file $tarball_name\n"
        if (! -r $tarball_name);
    $version = $tarball_name;
    $version =~ s/.*$file_base_name-(.+)\.tar.*/$1/;

    check_last_version($version);

    push(@email_output, "Snapshot: $version\n\n");
} 

# Make a root for this build to play in (scratch_root_arg is absolute, so
# root will be absolute)
my $root= "$scratch_root_arg/build-$version";
system("rm -rf $root");
mkdir($root, 0777);
chdir($root);

$outfile="out-$version.txt";

if ($outfile_arg) {
   $outfile="$outfile_arg";
}

test_abort("Could not find output file:$outfile -- aborting")
  if ( ! -f $outfile );

open OUTFILE, "$outfile";

# loop over all configurations
# be lazy: if no configurations supplied, do a default
# configure/build
my $results;
my $do_default = 0;

if (! $config_arg || ! -f $config_arg) {
    my $dir = "$root/default";
    my $name = "[default]";
    my $config = "CFLAGS=-g --disable-f77 --enable-debug";

    die "NOT SUPPORTED YET\n";

    $ret = try_untar($name, 0, $tarball_name,
                     $dir, "$dir/install", "", $config);
    $ret = try_configure($name, 0, $tarball_name,
                         $dir, "$dir/install", "", $config);
    $ret = cleanup($name, 0, $tarball_name,
                   $dir, "$dir/install", "", $config);
    $results->{$name} = $ret;
    $results->{$name}->{config} = $config;
    $results->{$name}->{want_stderr} = 1;
    $results->{$name}->{vpath_mode} = "";
    $results->{$name}->{installdir} = "$dir/install";
} else {
    open CONF, "$config_arg";
    my $i = 1;
    while (<CONF>) {
        my $line = $_;
        chomp($line);

        # skip comments and blank lines

        if ($line && ! ($line =~ /[ \t]*\#/) && ! ($line =~ /^[ \t]+$/)) {

            # Parse out the parts

            my ($name, $want_stderr, $vpath_mode, $config) = split(/:/, $line);
            if (! $name) {
                $name = "[build-$i]";
            }

            # try to actually build it

            my $dir = "$root/$name";
            $dir =~ s/[\[\] \t"']/_/g;
            # stupid emacs: '"
            my $merge_output = $want_stderr ? 0 : 1;
            $ret = try_untar($name, $merge_output, $tarball_name,
                             $dir, "$dir/install",
                             $vpath_mode, $config);

            while (<OUTFILE>) {
                my $mpi_dir=$_;
                my $added_config="$config --with-mpi-dir=$mpi_dir";

                $install_dir="$dir/install";

                if ($install_dir_arg) {
                    $install_dir="$install_dir_arg/$name";
                }
 
                chdir ("PMB-2.2");
                $ret = try_configure($name, $merge_output, $tarball_name,
                                     $dir, "$install_dir",
                                     $vpath_mode, $added_config);
                chdir ("..");

                chdir ("mpich_tester");
                $ret = try_configure($name, $merge_output, $tarball_name,
                                     $dir, "$install_dir",
                                     $vpath_mode, $added_config);
                chdir ("..");
                $results->{$name} = $ret;
                $results->{$name} = $ret;
                $results->{$name} = $ret;
                $results->{$name}->{config} = $config;
                $results->{$name}->{want_stderr} = $want_stderr;
                $results->{$name}->{vpath_mode} = $vpath_mode;
                $results->{$name}->{installdir} = "$install_dir";

            }
            $ret = cleanup($name, $merge_output, $tarball_name,
                           $dir, "$install_dir",
                           $vpath_mode, $config);
            ++$i;
        }
    }
    close CONF;
}

# remove this root if we're not leaving the install root
chdir($scratch_root_arg);
system("rm -rf build-$version")
    if (!$leave_install_arg);

my $dir = "$scratch_root_arg";
opendir(ROOT, $dir);
my @roots = sort(grep { /^build.+[0-9]$/ && -d "$dir/$_" } readdir(ROOT));

my $i = $#roots;
my $j = 0;
while ($i >= $max_build_roots) {
    print "Removing directory $roots[$j]\n" if ($debug);
    system("rm -rf $roots[$j]");
    ++$j;
    --$i;
}

# trim the downloads dir to $max_snapshots
my $dir = "$scratch_root_arg/downloads";
opendir(DOWNLOADS, $dir);
my @tarballs = sort(grep { /^openmpi.+gz$/ && -f "$dir/$_" } readdir(DOWNLOADS));
closedir(DOWNLOADS);
$i = $#tarballs;
$j = 0;
while ($i >= $max_snapshots) {
    print "Unlinking $tarballs[$j]\n" if ($debug);
    unlink("$dir/$tarballs[$j]");
    ++$j;
    --$i;
}

# save the version that we just tested
open LAST, ">$last_test_version_name";
print LAST "$version\n";
close LAST;

# if we're leaving the installdirs, output their names into the file
# indicated by the --leave-install arg
open LEAVE, ">$leave_install_arg"
    if ($leave_install_arg);

# send results mail
my $email_subject;
push(@email_output,
"SUMMARY OF RESULTS:
  (Building and compiling/linking PMB-2.2 and MPIch-testsuite)
--------------------------------------------------------------------------\n");

foreach my $config (keys(%$results)) {
    my $str;
    if ($results->{$config}->{status} == 0) {
        $str = "Success   ";
        print LEAVE "$results->{$config}->{installdir}\n"
            if ($leave_install_arg);
    } else {
        $str = "FAILURE   ";
        $email_subject = $fail_subject;
    }
    $str .= $config . "\n";
    push(@email_output, $str);
}
close(LEAVE)
    if ($leave_install_arg);
$email_subject = $success_subject
    if (!$email_subject);

push(@email_output,
"--------------------------------------------------------------------------
\n");


# Include additional details if relevant
my $header = 0;
my $displayed = 0;
push(@email_output, "DETAILED RESULTS (as necessary):\n\n");
foreach my $config (keys(%$results)) {
    my $output = 0;
    my $str;

    $str .= "==> Build: " . $config . "
==> Config options: " . $results->{$config}->{config} . "
==> Result: " . ($results->{$config}->{status} == 0 ?
             "Success" : "FAILURE") . "
==> Output: " . ($results->{$config}->{want_stderr} ? 
             "stderr and stdout shown separately below" : 
             "stdout and stderr merged below") . "\n";

    if ($results->{$config}->{status} != 0 &&
        $results->{$config}->{stdout} &&
        $#{$results->{$config}->{stdout}} >= 0) {
        push(@email_output, $str);
        $header = $displayed = $output = 1;

        if ($results->{$config}->{want_stderr}) {
            push(@email_output, 
"--Standard output---------------------------------------------------------\n");
        } else {
            push(@email_output, 
"--Standard output and standard error--------------------------------------\n");
        }
        my $tmp = trim($max_log_len, $results->{$config}->{stdout});
        push(@email_output, @$tmp);
        if ($results->{$config}->{want_stderr}) {
            push(@email_output, 
"--Standard output end-----------------------------------------------------\n");
        } else {
            push(@email_output, 
"--Standard output and standard error end----------------------------------\n");
        }
    }
    if ($results->{$config}->{stderr} &&
        $#{$results->{$config}->{stderr}} >= 0) {
        push(@email_output, $str)
            if ($output == 0);
        $header = $displayed = $output = 1;

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
        $header = $displayed = $output = 1;

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

# All done

exit(0);
