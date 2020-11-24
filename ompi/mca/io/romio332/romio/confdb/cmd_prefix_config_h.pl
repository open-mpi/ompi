#!/usr/bin/perl
use strict;

# This script is to be run by AC_CONFIG_COMMANDS in configure.ac.
# USAGE:
#     AC_CONFIG_COMMANDS([prefix-config],[perl cmd_prefix_config_h.pl PREFIX input_config.h output_config.h])
#
# The script will read "input_config.h", and write "output_config.h", adding prefix to every defined macros. This script is a replacement to AX_PREFIX_CONFIG_H.

sub add_prefix {
    my ($name, $prefix) = @_;
    if($name=~/^(inline|const|restrict)/){
        # leave c99 keywords alone
    }
    elsif($name=~/^_/){
        # leave underscore keywords alone, e.g _MINIX
    }
    elsif($name=~/^$prefix\_/i){
        # avoid double prefix
    }
    elsif($name=~/^[A-Z0-9_]+$/){
        $name = uc($prefix)."_$name";
    }
    else{
        $name = "_".lc($prefix)."_$name";
    }
    return $name;
}

my ($prefix, $config_in, $config_out)=@ARGV;
if(!$prefix){
    die "missing prefix!\n";
}
if(!$config_in){
    $config_in = "config.h";
}
if(!$config_out){
    $config_out = $config_in;
}
my @lines;
open In, "$config_in" or die "Can't open $config_in.\n";
while(<In>){
    if(/^#define\s+(\w+)\s*(.+)/){
        my $name = add_prefix($1, $prefix);
        push @lines, "#ifndef $name\n";
        push @lines, "#define $name $2\n";
        push @lines, "#endif\n";
        next;
    }
    elsif(/^\/\*\s*#undef (\w+)/){
        my $name = add_prefix($1, $prefix);
        push @lines, "/* #undef $name */\n";
        next;
    }
    push @lines, $_;
}
close In;
my $DEFH=uc($config_out);
$DEFH=~s/\W/_/g;
open Out, ">$config_out" or die "Can't write $config_out.\n";
print Out "#ifndef $DEFH\n";
print Out "#define $DEFH 1\n\n";
print Out "\x2f* $config_out. Generated automatically at end of configure. *\x2f\n";
print Out @lines;
print Out "\x2f* once: $DEFH *\x2f\n";
print Out "#endif\n";
close Out;
