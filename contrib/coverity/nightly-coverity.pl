#!/usr/bin/env perl

use warnings;
use strict;

use Getopt::Long;
use File::Temp qw/ tempfile tempdir /;
use File::Basename;

my $filename_arg;
my $coverity_token_arg;
my $dry_run_arg = 0;
my $verbose_arg = 0;
my $debug_arg = 0;
my $logfile_dir_arg;
my $configure_args = "";
my $make_args = "-j 32";
my $help_arg = 0;

&Getopt::Long::Configure("bundling");
my $ok = Getopt::Long::GetOptions("filename=s" => \$filename_arg,
                                  "coverity-token=s" => \$coverity_token_arg,
                                  "logfile-dir=s" => \$logfile_dir_arg,
                                  "configure-args=s" => \$configure_args,
                                  "make-args=s" => \$make_args,
                                  "dry-run!" => \$dry_run_arg,
                                  "verbose!" => \$verbose_arg,
                                  "debug!" => \$debug_arg,
                                  "help|h" => \$help_arg);

$ok = 0
    if (!defined($filename_arg));
$ok = 0
    if (!defined($coverity_token_arg));
if (!$ok || $help_arg) {
    print "Usage: $0 --filename=FILENAME --coverity-token=TOKEN [--dry-run] [--verbose] [--help]\n";
    exit($ok);
}

die "Cannot read $filename_arg"
    if (! -r $filename_arg);

$verbose_arg = 1
    if ($debug_arg);

######################################################################

sub verbose {
    print @_
        if ($verbose_arg);
}

# run a command and save the stdout / stderr
sub safe_system {
    my ($cmd) = shift;
    my ($logfilename) = shift;

    print "*** Running command: $cmd\n" if ($debug_arg);
    pipe OUTread, OUTwrite;

    # Child

    my $pid;
    if (($pid = fork()) == 0) {
        close OUTread;

        close(STDERR);
        open STDERR, ">&OUTwrite"
            || die "Can't redirect stderr\n";
        select STDERR;
        $| = 1;

        close(STDOUT);
        open STDOUT, ">&OUTwrite"
            || die "Can't redirect stdout\n";
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

    # Parent

    my (@out);
    my ($rin, $rout);
    my $done = 1;

    # Keep watching over the pipe(s)

    $rin = '';
    vec($rin, fileno(OUTread), 1) = 1;

    while ($done > 0) {
        my $nfound = select($rout = $rin, undef, undef, undef);

        if (vec($rout, fileno(OUTread), 1) == 1) {
            my $data = <OUTread>;
            if (!defined($data)) {
                vec($rin, fileno(OUTread), 1) = 0;
                --$done;
            } else {
                push(@out, $data);
                print "OUT:$data" if ($debug_arg);
            }
        }
    }

    # The pipes are closed, so the process should be dead.  Reap it.

    waitpid($pid, 0);
    my $status = $?;
    print "*** Command complete, exit status: $status\n" if ($debug_arg);

    # Return an anonymous hash containing the relevant data

    my $ret = {
        stdout_and_stderr => \@out,
        status => $status
        };

    # If the command failed, just quit
    if ($status != 0) {
        print "*** Command \"$cmd\" exited with non-zero status ($status); exiting\n";
        chdir("/");
        exit($status);
    }

    # If a log filename was given, and we have a logfile dir, then
    # write logfiles for stdout/stderr.
    if (defined($logfilename) && defined($logfile_dir_arg)) {
        my $filename = "$logfile_dir_arg/$logfilename";

        # Exit status
        open(OUT, ">$filename-status.out") ||
            die "Can't write to $filename-status.out";
        print OUT "Exit status: $status\n";
        close(OUT);

        # Stdout+stderr
        if ($#out >= 0) {
            open(OUT, ">$filename-stdout-stderr.out") ||
                die "Can't write to $filename-stdout-stderr.out";
            print OUT @out;
            close(OUT);
        }
    }

    # If we failed, just die
    if ($ret->{status} != 0) {
        print "=== Failed to $cmd\n";
        print "=== Last few lines of stdout/stderr:\n";
        my $i = $#{$ret->{stdout}} - 500;
        $i = 0
            if ($i < 0);
        while ($i <= $#{$ret->{stdout}}) {
            print $ret->{stdout}[$i];
            ++$i;
        }
        chdir("/");
        exit(1);
    }

    return $ret;
}

######################################################################

# Make an area to work

my $dir = tempdir(CLEANUP => 1);
chdir($dir);
verbose "*** Working in $dir\n";

######################################################################

# Get the coverity tool, put it in our path

verbose "*** Downloading coverity tool\n";
safe_system("wget https://scan.coverity.com/download/linux-64 --post-data \"token=$coverity_token_arg\&project=OpenMPI\" -O coverity_tool.tgz");
safe_system("tar xf coverity_tool.tgz");
opendir(my $dh, ".") ||
    die "Can't opendir .";
my @files = grep { /^cov/ && -d "./$_" } readdir($dh);
closedir($dh);

my $cov_dir = "$dir/$files[0]/bin";
$ENV{PATH} = "$cov_dir:$ENV{PATH}";

######################################################################

# Expand the OMPI tarball, build it

verbose "*** Extracting OMPI tarball\n";
safe_system("tar xf $filename_arg");
my $tarball_filename = basename($filename_arg);
$tarball_filename =~ m/^(.+)\.tar.+$/;
my $ompi_ver = $1;
chdir($ompi_ver);

verbose "*** Configuring OMPI tarball\n";
safe_system("./configure $configure_args");

verbose "*** Building OMPI tarball\n";
safe_system("cov-build --dir cov-int make $make_args");

# Tar up the Coverity results
verbose "*** Tarring up results\n";
safe_system("tar jcf $ompi_ver-analyzed.tar.bz2 cov-int");

# If not dry-run, submit to Coverity
if ($dry_run_arg) {
    verbose "*** Would have submitted, but this is a dry run\n";
} else {
    verbose "*** Submitting results\n";
    safe_system("curl --form token=$coverity_token_arg " .
                "--form email=jsquyres\@cisco.com " .
                "--form file=\@tarball/file/location " .
                "--form version=\"Version\" " .
                "--form description=\"Description\" " .
                "https://scan.coverity.com/builds?project=OpenMPI");
}

verbose("*** All done\n");
chdir("/");
exit(0);
