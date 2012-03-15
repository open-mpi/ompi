#!/usr/bin/env perl
#
# Copyright (c) 2010-2012 Oracle and/or its affiliates.  All rights reserved.
#
# Script to see if ob1 directory has moved ahead.
#
#

use strict;

# This revision represents that last known change to the ob1 directory.
# Therefore, we check to see if there is a difference between this revision
# and the head of the trunk.
#
# To run this: check-ob1-revision.pl
#
# After the changes are ported, one can bump up the $revision variable in
# in this file to whatever the ob1 directory is at.
#
my $revision = "-r26145";
my $cmd;
my $cmd_output;
my $pmlsdir = "../ompi/mca/pml";

$cmd = "svn diff $revision $pmlsdir/ob1";

open (CMD, "$cmd|");
$cmd_output .= $_
    while (<CMD>);
close(CMD);

print "Running $cmd\n";

if ($cmd_output eq "") {
  print "No new changes detected in ob1.  Everything is fine.\n";
} else {
  print "The following changes should be reviewed to see if they should \n";
  print "be ported to bfo.\n\n";
  print "$cmd_output";
}

