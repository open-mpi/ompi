#!/usr/bin/env perl

use strict;
use POSIX qw(strftime);

my $happy = 1;

my $savedays = $ARGV[0];
my $dir = $ARGV[1];

$happy = 0
  if ($savedays <= 0 || ! -d $dir);
die "Must specify number of days and a directory"
  if (!$happy);

#------------------------------------------------------------------

# Read in all the dir entries
opendir(DIR, $dir) || die "Cannot open $dir";
my @files = readdir(DIR);
closedir(DIR);

# How many days to keep?
my $t = time() - ($savedays * 60 * 60 * 24);
print "Deleting anything before: " . strftime("%D", localtime($t)) . "\n";
my $to_delete;

# Check everything in the dir; if is a dir, is not . or .., and is
# older than the save date, keep it for deleting later.
foreach my $file (@files) {
  if (-d "$dir/$file" && $file ne "." && $file ne "..") {
    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
        $atime,$mtime,$ctime,$blksize,$blocks) = stat("$dir/$file");
    my $str = "SAVE";
    if ($mtime < $t) {
      $to_delete = "$to_delete $dir/$file";
      $str = "DELETE";
    }
    print "Found dir ($str): $file (mtime: " . strftime("%D", localtime($mtime)) . ")\n";
  }
}

# If we found anything to delete, do so.
if ($to_delete ne "") {
    print "Deleting: $to_delete\n";
    system("chmod -R u=rwx $to_delete");
    system("rm -rf $to_delete");
  } else {
    print "Nothing to delete!\n";
}

exit(0);
