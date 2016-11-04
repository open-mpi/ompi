#!/usr/bin/env perl

use warnings;
use strict;

use Getopt::Long;
use File::Temp qw/ tempfile tempdir /;
use File::Basename;

my $coverity_project = "hwloc";
# Coverity changes this URL periodically
my $coverity_tool_url = "https://scan.coverity.com/download/cxx/linux64";

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
    my $allowed_to_fail = shift;
    my $cmd = shift;
    my $stdout_file = shift;

    # Redirect stdout if requested or not verbose
    if (defined($stdout_file)) {
        $stdout_file = "$logfile_dir_arg/$stdout_file";
        unlink($stdout_file);
        $cmd .= " >$stdout_file";
    } elsif (!$debug_arg) {
        $cmd .= " >/dev/null";
    }
    $cmd .= " 2>&1";

    my $rc = system($cmd);
    if (0 != $rc && !$allowed_to_fail) {
        # If we die/fail, ensure to change out of the temp tree so
        # that it can be removed upon exit.
        chdir("/");
        print "Command $cmd failed: exit status $rc\n";
        if (defined($stdout_file) && -f $stdout_file) {
            print "Last command output:\n";
            system("cat $stdout_file");
        }
        die "Cannot continue";
    }
    system("cat $stdout_file")
        if ($debug_arg && defined($stdout_file) && -f $stdout_file);
}

######################################################################

# Make an area to work

my $dir = tempdir(CLEANUP => 0);
chdir($dir);
verbose "*** Working in $dir\n";

######################################################################

# Get the coverity tool, put it in our path

my $cdir = "$ENV{HOME}/coverity";
safe_system(0, "mkdir $cdir")
    if (! -d $cdir);

# Optimization: the tool is pretty large.  If our local copy is less
# than a day old, just use that without re-downloading.
my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
    $atime,$mtime,$ctime,$blksize,$blocks) =
    stat("$cdir/coverity_tool.tgz");
my $now = time();
if (!defined($mtime) || $mtime < $now - 24*60*60) {
    verbose "*** Downloading new copy of the coverity tool\n";
    safe_system(0, "wget $coverity_tool_url --post-data \"token=$coverity_token_arg&project=$coverity_project\" -O coverity_tool.tgz");
    safe_system(0, "cp coverity_tool.tgz $cdir");
}

verbose "*** Expanding coverity tool tarball\n";
safe_system(0, "tar xf $cdir/coverity_tool.tgz");
opendir(my $dh, ".") ||
    die "Can't opendir .";
my @files = grep { /^cov/ && -d "./$_" } readdir($dh);
closedir($dh);

my $cov_dir = "$dir/$files[0]/bin";
$ENV{PATH} = "$cov_dir:$ENV{PATH}";

######################################################################

# Expand the HWLOC tarball, build it

verbose "*** Extracting HWLOC tarball\n";
safe_system(0, "tar xf $filename_arg");
my $tarball_filename = basename($filename_arg);
$tarball_filename =~ m/^hwloc-(.+)\.tar.+$/;
my $hwloc_ver = $1;
chdir("hwloc-$hwloc_ver");

verbose "*** Configuring HWLOC tarball\n";
safe_system(0, "./configure $configure_args", "configure");

verbose "*** Building HWLOC tarball\n";
safe_system(0, "cov-build --dir cov-int make $make_args", "cov-build");

# Tar up the Coverity results
verbose "*** Tarring up results\n";
safe_system(0, "tar jcf $hwloc_ver-analyzed.tar.bz2 cov-int");

# If not dry-run, submit to Coverity
if ($dry_run_arg) {
    verbose "*** Would have submitted, but this is a dry run\n";
} else {
    verbose "*** Submitting results\n";
    safe_system(0, "curl --form token=$coverity_token_arg " .
                "--form email=brice.goglin\@labri.fr " .
                "--form file=\@$hwloc_ver-analyzed.tar.bz2 " .
                "--form version=$hwloc_ver " .
                "--form description=nightly-master " .
                "https://scan.coverity.com/builds?project=hwloc",
                "coverity-submit");
}

verbose("*** All done\n");

# Chdir out of the tempdir so that it can be removed
chdir("/");

exit(0);
