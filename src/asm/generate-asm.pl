#!/usr/bin/perl -w


my $asmarch = shift;
my $asmformat = shift;
my $basedir = shift;
my $output = shift;

if ( ! $asmarch) { 
    print "usage: generate-asm.pl [ASMARCH] [ASMFORMAT] [BASEDIR] [OUTPUT NAME]\n";
    exit(1);
}

open(INPUT, "$basedir/$asmarch.asm") || 
    die "Could not open $basedir/$asmarch.asm: $!\n";
open(OUTPUT, ">$output") || die "Could not open $output: $1\n";

my $TEXT = "";
my $GLOBAL = "";
my $SUFFIX = "";
my $GSYM = "";
my $LSYM = "";
my $TYPE = "";
my $SIZE = 0;
my $ALIGN_LOG = 0;
my $DEL_R_REG = 0;
my $IS64BIT = 0;

($TEXT, $GLOBAL, $SUFFIX, $GSYM, $LSYM, $TYPE, $SIZE, $ALIGN_LOG, $DEL_R_REG, $IS64BIT) = (
    $asmformat =~ /(.*)\-(.*)\-(.*)\-(.*)\-(.*)\-(.*)\-(.*)\-(.*)\-(.*)\-(.*)/);

my $current_func = "";
my $delete = 0;

while (<INPUT>) {
    s/TEXT/$TEXT/g;
    s/GLOBAL/$GLOBAL/g;
    s/GSYM\((.*)\)/$GSYM$1$SUFFIX/g;
    s/LSYM\((.*)\)/$LSYM$1$SUFFIX/g;
    if ($DEL_R_REG == 0) {
        s/r([0-9][0-9]?)/$1/g;
    }

    if (/START_FUNC\((.*)\)/) {
        $current_func = $1;
        $_ = "\t$GLOBAL $GSYM$current_func\n";
        if (! $TYPE eq "") { 
            $_ .= "\t.type $current_func, $TYPE" . "function\n";
        }
        $_ .= "$GSYM$current_func$SUFFIX\n";
    }

    if (/END_FUNC\((.*)\)/) {
        s/END_FUNC\((.*)\)//g;
        if ($SIZE != 0) {
            $_ = "\t.size $current_func, .-$current_func\n";
        } else {
            chomp;
        }
    }

    if ($ALIGN_LOG == 0) {
        s/ALIGN\((\d*)\)/.align $1/g;
    } else {
        # Ugh...
        if (m/ALIGN\((\d*)\)/) {
            $val = $1;
            $result = 0;
            while ($val > 1) { $val /= 2; $result++ }
            s/ALIGN\((\d*)\)/.align $result/;
        }
    }

    if (/^\#START_64BIT/) {
        $_ = "";
        if ($IS64BIT == 0) {
            $delete = 1;
    }
    }
    if (/^\#END_64BIT/) { 
        $_ = "";
        $delete = 0; 
    }

    if ($delete == 0) {
        print OUTPUT $_;
    }

}

close(INPUT);
close(OUTPUT);
