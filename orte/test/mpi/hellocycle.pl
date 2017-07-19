#!/usr/bin/env perl
#
use strict;
use warnings;
use Date::Parse;

#
$ENV{OMPI_MCA_btl} = "self";
#
sub prtime {
   my $count = shift;
   my $str = localtime;
   print "$count: $str\n";
}


my $totalcount = 5000;
my $count = $totalcount;
prtime($count);
my $start = time();
while ($count > 0) {
   system("./hello > /dev/null 2>&1");
   $count--;

   if ($count % 1000 == 0) {
      prtime($count);
   }
}
prtime($count);

my $stop = time();
my $rate = $totalcount / ($stop - $start);
print "Rate: $rate\n";
