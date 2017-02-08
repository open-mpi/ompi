#!/usr/bin/env perl
#
# Copyright (c) 2010      Sandia National Laboratories. All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

my $components;
my @result;

while (@ARGV) {
    my $component;
    $component->{"name"} = shift(@ARGV);
    $component->{"value"} = shift(@ARGV);
    push(@{$components}, $component);
}

foreach my $component (sort { $b->{value} <=> $a->{value} } @{$components}) {
    push(@result, $component->{name});
}
sub commify_series {
    (@_ == 0) ? ''                                      :
    (@_ == 1) ? $_[0]                                   :
                join(", ", @_[0 .. ($#_-1)], "$_[-1]");
}

print commify_series(@result);
