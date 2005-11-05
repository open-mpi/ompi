#!/usr/bin/env perl
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
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
use Carp;
use Cwd;
use Getopt::Long;
use File::Basename;

############################################################################
# Local variables
############################################################################

my $ompi_topdir;
my $component_topdir;
my %config_values;
my %config_params;

my $initial_cwd = cwd();
my $announce_str = "Open MPI MCA component configure generator";
my %config_param_names = (PIFILE => "PARAM_INIT_FILE",
                          PCFGAUXDIR => "PARAM_CONFIG_AUX_DIR",
                          PC => "PARAM_WANT_C",
                          PCXX => "PARAM_WANT_CXX",
                          PVARPREFIX => "PARAM_VAR_PREFIX",
                          PAMNAME => "PARAM_AM_NAME",
                          PCFGHDRFILE => "PARAM_CONFIG_HEADER_FILE",
                          PCFGFILES => "PARAM_CONFIG_FILES",
                          PCOMPILEEXT => "PARAM_WANT_COMPILE_EXTERNAL",
                          );

############################################################################
# Command line arg procemcang
############################################################################

Getopt::Long::Configure("bundling", "require_order");
my $ok = Getopt::Long::GetOptions("ompidir|l=s" => \$ompi_topdir,
                                  "componentdir|m=s" => \$component_topdir);
if (!$ok) {
    print "Usage: $0 [--ompidir=DIR] [--componentdir=DIR]\n";
    exit(1);
}

############################################################################
# Try to figure out the ompi and component topdirs
############################################################################

print "$announce_str starting\n";

if (!$ompi_topdir) {
    $ompi_topdir = dirname($0);
}
chdir($ompi_topdir);
$ompi_topdir = cwd();
chdir($initial_cwd);
if (!$ompi_topdir || ! -f "$ompi_topdir/autogen.sh") {
    croak("Unable to find OMPI base directory (try using --ompidir)");
}

if (!$component_topdir) {
    $component_topdir = $initial_cwd;
    if ($component_topdir eq $ompi_topdir) {
        croak("Unable to determine which component to operate on");
    }
}
chdir($component_topdir);
$component_topdir = cwd();
chdir($initial_cwd);
if (!$ompi_topdir || ! -d $component_topdir) {
    croak("Unable to find component directory (try using --componentdir)");
}

# Print them out

print "--> Found OMPI top dir: $ompi_topdir\n";
print "--> Found component top dir: $component_topdir\n";

# If we have a configure.params file in the component topdir, we're good to
# go.

if (! -f "$component_topdir/configure.params") {
    die("No configure.params in component topdir; nothing to do");
}

# Make a backup

if (-f "$component_topdir/acinclude.m4") {
    printf("    *** WARNING: Replacing old acinclude.m4\n");
    unlink("$component_topdir/acinclude.m4.bak");
    rename("$component_topdir/acinclude.m4", "$component_topdir/acinclude.m4.bak");
}
if (-f "$component_topdir/configure.ac") {
    printf("    *** WARNING: Replacing old configure.ac\n");
    unlink("$component_topdir/configure.ac.bak");
    rename("$component_topdir/configure.ac", "$component_topdir/configure.ac.bak");
}

############################################################################
# Set and calculate sensible default parameter values
############################################################################

# Unchangeable values
# MCA_TYPE: calculate

$config_values{"MCA_TYPE"} = dirname($component_topdir);
$config_values{"MCA_TYPE"} = basename($config_values{"MCA_TYPE"});

# PROCESSED_MCA_TYPE: For "special" MCA types, like "crompi" and
# "crmpi".

$config_values{"PROCESSED_MCA_TYPE"} = $config_values{"MCA_TYPE"};
if ($config_values{"PROCESSED_MCA_TYPE"} eq "crompi" ||
    $config_values{"PROCESSED_MCA_TYPE"} eq "crmpi") {
    $config_values{"PROCESSED_MCA_TYPE"} = "cr";
}

# MCA_NAME: calculate

$config_values{"MCA_COMPONENT_NAME"} = basename($component_topdir);

# Parameter (changeable) values
# PARAM_COMPILE_EXTERNAL: set

$config_params{$config_param_names{PCOMPILEEXT}} = 0;

# PARAM_CONFIG_AUX_DIR: set

if (-d "$component_topdir/config") {
    $config_params{$config_param_names{PCFGAUXDIR}} = "config";
} else {
    $config_params{$config_param_names{PCFGAUXDIR}} = ".";
}
$config_params{$config_param_names{PC}} = 1;
$config_params{$config_param_names{PCXX}} = 0;

# PARAM_VAR_PREFIX: calculate

$config_params{$config_param_names{PVARPREFIX}} = 
    "MCA_" . $config_values{"MCA_TYPE"} .
    "_" . $config_values{"MCA_COMPONENT_NAME"};

# PARAM_AM_NAME: calculate

$config_params{$config_param_names{PAMNAME}} = 
    lc($config_values{"MCA_TYPE"}) .
    "-" . lc($config_values{"MCA_COMPONENT_NAME"});

# PARAM_CONFIG_HEADER_FILE: calculate

$config_params{$config_param_names{PCFGHDRFILE}} = 
    "src/" . lc($config_values{"MCA_TYPE"}) .
    "_" . lc($config_values{"MCA_COMPONENT_NAME"}) . "_config.h";

# Is there a config.stub file in the component topdir?

if (-f "$component_topdir/configure.stub") {
    $config_values{CONFIGURE_STUB_SINCLUDE} = "#
# Component-specific tests
#

sinclude(configure.stub)\n";
    $config_values{CONFIGURE_STUB_MACRO} = 
        "ompi_show_subtitle \"MCA " . $config_values{"MCA_TYPE"} . " " .
        $config_values{"MCA_COMPONENT_NAME"} . "-specific setup\"
MCA_CONFIGURE_STUB";

    # See if there's a CONFIGURE_DIST_STUB in configure.stub

    open(STUB, "$component_topdir/configure.stub");
    my $found = 0;
    while (<STUB>) {
        $found = 1 
            if ($_ =~ /MCA_CONFIGURE_DIST_STUB/);
    }
    close(STUB);
    if ($found == 1) {
        $config_values{CONFIGURE_DIST_STUB_MACRO} = 
            "ompi_show_subtitle \"MCA " . $config_values{"MCA_TYPE"} . " " .
            $config_values{"MCA_COMPONENT_NAME"} . 
            "-specific setup (dist specific!)\"
MCA_CONFIGURE_DIST_STUB";
    } else {
        $config_values{CONFIGURE_DIST_STUB_MACRO} = "true";
    }
} else {
    $config_values{CONFIGURE_STUB_SINCLUDE} = "";
    $config_values{CONFIGURE_STUB_MACRO} = "true";
    $config_values{CONFIGURE_DIST_STUB_MACRO} = "true";
}

############################################################################
# Read in the configure.params file (possibly overriding the defaults
# set above)
############################################################################

my $found = 0;
my @names = values %config_param_names;
open(PARAMS, "$component_topdir/configure.params") ||
    die("Could not open configure.params in $component_topdir");
while (<PARAMS>) {
    chomp;

    # Ignore comments and blank lines

    my $line = $_;
    $line =~ s/^[ \t]+(.*)[ \t]+$/\1/;
    $line =~ s/(.*)[\#]+.*/\1/;
    next if (length($line) == 0);

    # So we have a key=value line
    # Split into componenty, and remove quotes

    my ($key, $value) = split(/=/, $line);
    $key =~ s/^[ \t]+(.*)[ \t]+$/\1/;
    $value =~ s/^[ \t]+(.*)[ \t]+$/\1/;
    $value =~ s/^[\"](.*)[\"]$/\1/;
    $value =~ s/^[\'](.*)[\']$/\1/;

    for (my $i = 0; $i <= $#names; ++$i) {
        if ($key eq $names[$i]) {
            if (!$found) {
                printf("--> Found parameter override(s):\n");
                $found = 1;
            }
            printf("    $key = $value\n");
            $config_params{$key} = $value;
            last;
        }
    }
}

# Print out the values

print "--> Final configuration values:\n";
foreach my $key (sort keys(%config_param_names)) {
    print "    $config_param_names{$key} = " .
        $config_params{$config_param_names{$key}} . "\n";

}

# Do some error checking on the values that we've determined

if (! -f $config_params{PARAM_INIT_FILE}) {
    print "*** WARNING: PARAM_INIT_FILE does not exist:\n";
    print "*** WARNING:    $config_params{PARAM_INIT_FILE}\n";
    print "*** WARNING: resulting configure script will not run properly!\n";
    exit(1);
}

my @files = split(/ /, $config_params{PARAM_CONFIG_FILES});
foreach my $file (@files) {
    if (! -f "$file.in" && ! -f "$file.am") {
        print "*** WARNING: PARAM_CONFIG_FILES file does not exist:\n";
        print "*** WARNING:    $file.[in|am]\n";
        print "*** WARNING: resulting configure script may not run correctly!!\n";
        exit(1);
    }
}

if (! -d $config_params{PARAM_CONFIG_AUX_DIR}) {
    print "*** WARNING: PARAM_CONFIG_AUX_DIR does not exit:\n";
    print "*** WARNING:    $config_params{PARAM_CONFIG_AUX_DIR}\n";
    print "*** WARNING: Taking the liberty of trying to make it...\n";
    if (!mkdir($config_params{PARAM_CONFIG_AUX_DIR})) {
        print "*** ERROR: Failed to make AUX_DIR: $config_params{PARAM_CONFIG_AUX_DIR}\n";
        print "*** ERROR: Cannot continue\n";
        exit(1);
    }
}

# If we want to be able to compile outside the Open MPI tree, we need
# to copy some files to the auxdir

if ($config_params{PARAM_WANT_COMPILE_EXTERNAL} != 0) {
    my $auxdir = $config_params{PARAM_CONFIG_AUX_DIR};
    open (ACINCLUDE, "$ompi_topdir/config/mca_acinclude.m4");
{
        while (<ACINCLUDE>) {
            chomp;
            my $filename = $_;
            if ($filename =~ /^.*sinclude\(\@M4DIR\@\/(.+)\).*$/) {
                $filename =~ s/^.*sinclude\(\@M4DIR\@\/(.+)\).*$/\1/;
                unlink("$auxdir/$filename")
                    if (-f "$auxdir/$filename");
                print "--> Copying m4 file: $filename ==> $auxdir\n";
                system("cp -f $ompi_topdir/config/$filename $config_params{PARAM_CONFIG_AUX_DIR}");
            }
        }
        close(ACINCLUDE);
    }
}

############################################################################
# Read in the configure.ac template
############################################################################

sub make_template {
    my ($src, $dest, $mode) = @_;
    my $template;
    my $search;
    my $replace;

    # Read in the template file

    print "--> Reading template file: $src\n";
    open(TEMPLATE, $src) ||
        die("Cannot open template file: $src");
    while (<TEMPLATE>) {
        $template .= $_;
    }
    close(TEMPLATE);

    # Transform the template

    # If we want C or C++, substitute in the right setup macros

    $search = "\@C_COMPILER_SETUP\@";
    $replace = $config_params{$config_param_names{"PC"}} ?
        "OMPI_SETUP_CC" : "";
    $template =~ s/$search/$replace/;

    $search = "\@CXX_COMPILER_SETUP\@";
    $replace = $config_params{$config_param_names{"PCXX"}} ?
        "OMPI_SETUP_CXX" : "";
    $template =~ s/$search/$replace/;

    # If we want to be able to compile outside the Open MPI tree, set
    # the right include path for the M4 files

    $search = "\@M4DIR\@";
    $replace = ($config_params{PARAM_WANT_COMPILE_EXTERNAL} == 1) ?
        $config_params{PARAM_CONFIG_AUX_DIR} : "../../../../config";
    $template =~ s/$search/$replace/g;

    # Do all the parameters.  This is done last so that any of the
    # above can use paramter values in their values, and expect to
    # have their respective values substituted in (i.e., a [semi]
    # recursive substitution).

    print "--> Filling in the template...\n";
    foreach my $key (sort keys(%config_values)) {
        $search = "@" . $key . "@";
        $template =~ s/$search/$config_values{$key}/g;
    }
    foreach my $key (sort keys(%config_param_names)) {
        next if ($key eq "PC" || $key eq "PCXX");

        $search = "@" . $config_param_names{$key} . "@";
        $template =~ 
            s/$search/$config_params{$config_param_names{$key}}/g;
    }

    # Write it out

    print "--> Writing output file: $dest\n";
    open(OUTPUT, ">$dest") ||
        die("Cannot open output flie: $dest");
    print OUTPUT $template;
    close(OUTPUT);

    chmod($dest, $mode);
}

# Read and fill in the templates

make_template("$ompi_topdir/config/mca_configure.ac", 
              "$component_topdir/configure.ac", 0644);
make_template("$ompi_topdir/config/mca_acinclude.m4",
              "$component_topdir/acinclude.m4", 0644);

############################################################################
# All done
############################################################################

print "\n$announce_str finished\n";

exit(0);
