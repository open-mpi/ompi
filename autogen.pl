#!/usr/bin/env perl
#
# Copyright (c) 2009-2022 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2015-2021 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2015-2022 IBM Corporation.  All rights reserved.
# Copyright (c) 2020      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

use strict;

use Cwd;
use File::Basename;
use File::Find;
use Data::Dumper;
use Getopt::Long;

#
# Global variables
#

# Sentinel file to remove if we fail
my $sentinel;

# The m4 file we'll write at the end
my $m4_output_file = "config/autogen_found_items.m4";
my $m4;
# Sanity check file
my $topdir_file = "opal/include/opal_config_bottom.h";
my $dnl_line = "dnl ---------------------------------------------------------------------------";

# Data structures to fill up with all the stuff we find
my $mca_found;
my $mpiext_found;
my @subdirs;

# Command line parameters
my $no_3rdparty_arg = "";
my $quiet_arg = 0;
my $debug_arg = 0;
my $help_arg = 0;
my $platform_arg = 0;
my $include_arg = 0;
my $exclude_arg = 0;
my $force_arg = 0;
my $automake_jobs;

# Include/exclude lists
my $include_list;
my $exclude_list;

# Minimum versions
my $ompi_automake_version;
my $ompi_autoconf_version;
my $ompi_libtool_version;

# Search paths
my $ompi_autoconf_search = "autoconf";
my $ompi_automake_search = "automake";
my $ompi_libtoolize_search = "libtoolize;glibtoolize";

# version of packages we ship as tarballs
my $libevent_version="2.1.12-stable";
my $hwloc_version="2.7.1";

# One-time setup
my $username;
my $hostname;
my $full_hostname;

# Patch program
my $patch_prog = "patch";
# Solaris "patch" doesn't understand unified diffs, and will cause
# autogen.pl to hang with a "File to patch:" prompt. Default to Linux
# "patch", but use "gpatch" on Solaris.
if ($^O eq "solaris") {
    $patch_prog = "gpatch";
}

$username = $ENV{USER} || getpwuid($>);
$full_hostname = $ENV{HOSTNAME} || `hostname`;
chomp($full_hostname);
$hostname = $full_hostname;
$hostname =~ s/^([\w\-]+)\..+/\1/;

##############################################################################

sub my_die {
    unlink($sentinel)
        if ($sentinel);
    die @_;
}

sub my_exit {
    my ($ret) = @_;
    unlink($sentinel)
        if ($sentinel && $ret != 0);
    exit($ret);
}

##############################################################################

sub verbose {
    print @_
        if (!$quiet_arg);
}

sub debug {
    print @_
        if ($debug_arg);
}

sub debug_dump {
    my $d = new Data::Dumper([@_]);
    $d->Purity(1)->Indent(1);
    debug $d->Dump;
}

##############################################################################

sub list_contains {
    my $searched_string = shift;
    foreach my $str (@_) {
	if ($searched_string eq $str) {
	    return 1;
	}
    }
    return 0;
}

##############################################################################

sub read_config_params {
    my ($filename, $dir_prefix) = @_;

    my $dir = dirname($filename);
    open(FILE, $filename) ||
        my_die "Can't open $filename";
    my $file;
    $file .= $_
        while(<FILE>);
    close(FILE);

    # Save all lines of the form "foo = bar" in a hash
    my $ret;
    while ($file =~ s/^\s*(\w+)\s*=\s*(.+)\s*$//m) {
        my $key = $1;
        my $val = $2;

        # Strip off any leading and trailing "'s
        $val = $1
            if ($val =~ m/^\"(.+)\"$/);

        $ret->{$key} = $val;
    }

    # Split PARAM_CONFIG_FILES into an array
    if (exists($ret->{PARAM_CONFIG_FILES})) {
        my @out;
        foreach my $f (split(/\s+/, $ret->{PARAM_CONFIG_FILES})) {
            push(@out, "$dir_prefix/$f");
        }
        $ret->{PARAM_CONFIG_FILES} = \@out;
    }

    debug_dump($ret);
    return $ret;
}

##############################################################################

# Process a "subdir", meaning that the directory isn't a component or
# an extension; it probably just needs an autoreconf, autogen, etc.
sub process_subdir {
    my ($dir) = @_;

    # Chdir to the subdir
    print "\n=== Processing subdir: $dir\n";
    my $start = Cwd::cwd();
    my $check = chdir($dir);
    if (0 == $check) {
        my_die "Could not change to $dir\n";
    }

    # Run an action depending on what we find in that subdir
    if (-x "autogen.pl") {
        print "--- Found autogen.pl; running...\n";
        safe_system("./autogen.pl");
    } elsif (-x "autogen.sh") {
        print "--- Found autogen.sh; running...\n";
        safe_system("./autogen.sh");
    } elsif (-f "configure.in" || -f "configure.ac") {
        print "--- Found configure.in|ac; running autoreconf...\n";
        safe_system("autoreconf -ivf");
        print "--- Patching autotools output... :-(\n";
    } else {
        my_die "Found subdir, but no autogen.sh or configure.in|ac to do anything";
    }

    # Ensure that we got a good configure executable.
    my_die "Did not generate a \"configure\" executable in $dir.\n"
        if (! -x "configure");

    # Fix known issues in Autotools output
    patch_autotools_output($start);

    # Chdir back to where we came from
    chdir($start);
}

##############################################################################

sub process_autogen_subdirs {
    my ($dir) = @_;

    my $file = "$dir/autogen.subdirs";
    if (-f $file) {
        open(FILE, $file) || my_die "Can't open $file";
        while (<FILE>) {
            chomp;
            $_ =~ s/#.*$//;
            $_ =~ s/^\s*//;
            $_ =~ s/\s*$//;
            if ($_ ne "") {
                print "    Found subdir: $_ (will process later)\n";

                # Note: there's no real technical reason to defer
                # processing the subdirs.  It's more of an aesthetic
                # reason -- don't interrupt the current flow of
                # finding mca / ext (which is a nice, fast
                # process).  Then process the subdirs (which is a slow
                # process) all at once.
                push(@subdirs, "$dir/$_");
            }
        }
        close(FILE);
    }
}

##############################################################################

sub mca_process_component {
    my ($topdir, $project, $framework, $component) = @_;

    my $pname = $project->{name};
    my $pdir = $project->{dir};
    my $cdir = "$topdir/$pdir/mca/$framework/$component";

    return
        if (! -d $cdir);

    # Process this directory (pretty much the same treatment as for
    # mpiext, so it's in a sub).
    my $found_component;

    # Does this directory have a configure.m4 file?
    if (-f "$cdir/configure.m4") {
        $found_component->{"configure.m4"} = 1;
        verbose "    Found configure.m4 file\n";
    }

    $found_component->{"name"} = $component;

    # Push the results onto the $mca_found hash array
    push(@{$mca_found->{$pname}->{$framework}->{"components"}},
         $found_component);

    # Is there an autogen.subdirs in here?
    process_autogen_subdirs($cdir);
}

##############################################################################

sub ignored {
    my ($dir) = @_;

    # If this directory does not have .opal_ignore, or if it has a
    # .opal_unignore that has my username in it, then add it to the
    # list of components.
    my $ignored = 0;

    if (-f "$dir/.opal_ignore") {
        $ignored = 1;
    }
    if (-f "$dir/.opal_unignore") {
        open(UNIGNORE, "$dir/.opal_unignore") ||
            my_die "Can't open $dir/.opal_unignore file";
        my $unignore;
        $unignore .= $_
            while (<UNIGNORE>);
        close(UNIGNORE);

        $ignored = 0
            if ($unignore =~ /^$username$/m ||
                $unignore =~ /^$username\@$hostname$/m ||
                $unignore =~ /^$username\@$full_hostname$/m);
    }

    return $ignored;
}

##############################################################################

sub mca_process_framework {
    my ($topdir, $project, $framework) = @_;

    my $pname = $project->{name};
    my $pdir = $project->{dir};

    # Does this framework have a configure.m4 file?
    my $dir = "$topdir/$pdir/mca/$framework";
    if (-f "$dir/configure.m4") {
        $mca_found->{$pname}->{$framework}->{"configure.m4"} = 1;
        verbose "    Found framework configure.m4 file\n";
    }

    # Did we exclude all components for this framework?
    if (exists($exclude_list->{$framework}) &&
        $exclude_list->{$framework}[0] eq "AGEN_EXCLUDE_ALL") {
        verbose "    => Excluded\n";
    } else {
        # Look for component directories in this framework
        if (-d $dir) {
            $mca_found->{$pname}->{$framework}->{found} = 1;
            opendir(DIR, $dir) ||
                my_die "Can't open $dir directory";
            foreach my $d (sort(readdir(DIR))) {
                # Skip any non-directory, "base", or any dir that
                # begins with "."
                next
                    if (! -d "$dir/$d" || $d eq "base" ||
                        substr($d, 0, 1) eq ".");

                # Skip any component that doesn't have a configure.m4
                # or Makefile.am as we couldn't build it anyway
                if (! -f "$dir/$d/configure.m4" &&
                    ! -f "$dir/$d/Makefile.am" &&
                    ! -f "$dir/$d/configure.ac" &&
                    ! -f "$dir/$d/configure.in") {
                     verbose "    => No sentinel file found in $dir/$d -> Excluded\n";
                     next;
                }

                verbose "--- Found $pname / $framework / $d component\n";

                # Skip if specifically excluded
                if (exists($exclude_list->{$framework})) {
                    my $tst = 0;
                    foreach my $ck (@{$exclude_list->{$framework}}) {
                        if ($ck eq $d) {
                            verbose "    => Excluded\n";
                            $tst = 1;
                            last;
                        }
                    }
                    if ($tst) {
                        next;
                    }
                }

                # Skip if the framework is on the include list, but
                # doesn't contain this component
                if (exists($include_list->{$framework})) {
                    my $tst = 0;
                    foreach my $ck (@{$include_list->{$framework}}) {
                        if ($ck ne $d) {
                            verbose "    => Not included\n";
                            $tst = 1;
                            last;
                        }
                    }
                    if ($tst) {
                        next;
                    }
                }

                # Check ignore status
                if (ignored("$dir/$d")) {
                    verbose "    => Ignored (found .opal_ignore file)\n";
                } else {
                    mca_process_component($topdir, $project, $framework, $d);
                }
            }
        }
        closedir(DIR);
    }
}

##############################################################################

sub mca_generate_framework_header(\$\@) {
    my ($project, @frameworks) = @_;
    my $framework_array_output="";
    my $framework_decl_output="";

    foreach my $framework (@frameworks) {
        # There is no common framework object
        if ($framework ne "common") {
            my $framework_name = "${project}_${framework}_base_framework";
            $framework_array_output .= "    &$framework_name,\n";
            $framework_decl_output .= "extern mca_base_framework_t $framework_name;\n";
        }
    }

    my $ifdef_string = uc "${project}_FRAMEWORKS_H";
    open(FRAMEWORKS_OUT, ">$project/include/$project/frameworks.h");
    printf FRAMEWORKS_OUT "%s", "/*
 * This file is autogenerated by autogen.pl. Do not edit this file by hand.
 */
#ifndef $ifdef_string
#define $ifdef_string

#include \"opal/mca/base/mca_base_framework.h\"

$framework_decl_output
static mca_base_framework_t *${project}_frameworks[] = {
$framework_array_output    NULL
};

#endif /* $ifdef_string */\n\n";
    close(FRAMEWORKS_OUT);
}

##############################################################################

sub mca_process_project {
    my ($topdir, $project) = @_;

    my $pname = $project->{name};
    my $pdir = $project->{dir};

    # Does this project have a configure.m4 file?
    if (-f "$topdir/$pdir/configure.m4") {
        $mca_found->{$pname}->{"configure.m4"} = 1;
        verbose "    Found $topdir/$pdir/configure.m4 file\n";
    }

    # Look for framework directories in this project
    my $dir = "$topdir/$pdir/mca";
    if (-d $dir) {
        opendir(DIR, $dir) ||
            my_die "Can't open $dir directory";
        my @my_dirs = readdir(DIR);
        @my_dirs = sort(@my_dirs);

        foreach my $d (@my_dirs) {
            # Skip any non-directory, "base", or any dir that begins with "."
            next
                if (! -d "$dir/$d" || $d eq "base" || substr($d, 0, 1) eq ".");

            my $framework_header = "$dir/$d/$d.h";

            # If there's a $dir/$d/autogen.options file, read it
            my $ao_file = "$dir/$d/autogen.options";
            if (-r $ao_file) {
                verbose "\n>>> Found $dir/$d/autogen.options file\n";
                open(IN, $ao_file) ||
                    die "$ao_file present, but cannot open it";
                while (<IN>) {
                    if (m/\s*framework_header\s*=\s*(.+?)\s*$/) {
                        verbose "    Framework header entry: $1\n";
                        $framework_header = "$dir/$d/$1";
                    }
                }
                close(IN);
            }

            # If this directory has a framework header and a base/
            # subdirectory, or its name is "common", then it's a
            # framework.
            if ("common" eq $d || !$project->{need_base} ||
                (-f $framework_header && -d "$dir/$d/base")) {
                verbose "\n=== Found $pname / $d framework\n";
                mca_process_framework($topdir, $project, $d);
            }
        }
        closedir(DIR);
    }
}

##############################################################################

sub mca_run_global {
    my ($projects) = @_;

    # For each project, go find a list of frameworks, and for each of
    # those, go find a list of components.
    my $topdir = Cwd::cwd();
    foreach my $p (@$projects) {
        if (-d "$topdir/$p->{dir}") {
            verbose "\n*** Found $p->{name} project\n";
            mca_process_project($topdir, $p);
        }
    }

    # Debugging output
    debug_dump($mca_found);

    # Save (just) the list of MCA projects in the m4 file
    my $str;
    foreach my $p (@$projects) {
        my $pname = $p->{name};
        # Check if this project is an MCA project (contains MCA framework)
        if (exists($mca_found->{$pname})) {
            $str .= "$p->{name}, ";
        }
    }
    $str =~ s/, $//;
    $m4 .= "\ndnl List of MCA projects found by autogen.pl
m4_define([mca_project_list], [$str])\n";

    #-----------------------------------------------------------------------

    $m4 .= "\n$dnl_line
$dnl_line
$dnl_line

dnl MCA information\n";

    # Array for all the m4_includes that we'll need to pick up the
    # configure.m4's.
    my @includes;

    # Next, for each project, write the list of frameworks
    foreach my $p (@$projects) {

        my $pname = $p->{name};
        my $pdir = $p->{dir};

        if (exists($mca_found->{$pname})) {
            my $frameworks_comma;

            # Does this project have a configure.m4 file?
            push(@includes, "$pdir/configure.m4")
                if (exists($mca_found->{$p}->{"configure.m4"}));

            # Print out project-level info
            my @mykeys = keys(%{$mca_found->{$pname}});
            @mykeys = sort(@mykeys);

            # Ensure that the "common" framework is listed first
            # (if it exists)
            my @tmp;
            push(@tmp, "common")
                if (grep(/common/, @mykeys));
            foreach my $f (@mykeys) {
                push(@tmp, $f)
                    if ($f ne "common");
            }
            @mykeys = @tmp;

            foreach my $f (@mykeys) {
                $frameworks_comma .= ", $f";

                # Does this framework have a configure.m4 file?
                push(@includes, "$pdir/mca/$f/configure.m4")
                    if (exists($mca_found->{$pname}->{$f}->{"configure.m4"}));

                # This framework does have a Makefile.am (or at least,
                # it should!)
                my_die "Missing $pdir/mca/$f/Makefile.am"
                    if (! -f "$pdir/mca/$f/Makefile.am");
            }
            $frameworks_comma =~ s/^, //;

            &mca_generate_framework_header($pname, @mykeys);

            $m4 .= "$dnl_line

dnl Frameworks in the $pname project and their corresponding directories
m4_define([mca_${pname}_framework_list], [$frameworks_comma])

";

            # Print out framework-level info
            foreach my $f (@mykeys) {
                my $components;
                my $m4_config_component_list;
                my $no_config_component_list;

                # Troll through each of the found components
                foreach my $comp (@{$mca_found->{$pname}->{$f}->{components}}) {
                    my $c = $comp->{name};
                    $components .= "$c ";

                    # Does this component have a configure.m4 file?
                    if (exists($comp->{"configure.m4"})) {
                        push(@includes, "$pdir/mca/$f/$c/configure.m4");
                        $m4_config_component_list .= ", $c";
                    } else {
                        $no_config_component_list .= ", $c";
                    }
                }
                $m4_config_component_list =~ s/^, //;
                $no_config_component_list =~ s/^, //;

                $m4 .= "dnl Components in the $pname / $f framework
m4_define([mca_${pname}_${f}_m4_config_component_list], [$m4_config_component_list])
m4_define([mca_${pname}_${f}_no_config_component_list], [$no_config_component_list])

";
            }
        }
    }

    # List out all the m4_include
    $m4 .= "$dnl_line

dnl List of configure.m4 files to include\n";
    foreach my $i (@includes) {
        $m4 .= "m4_include([$i])\n";
    }
}

##############################################################################

sub mpiext_process_extension {
    my ($topdir, $ext_prefix, $extdir) = @_;

    my $edir = "$topdir/$ext_prefix/$extdir";
    return
        if (! -d $edir);

    # Process this directory (pretty much the same treatment as for
    # MCA components, so it's in a sub).
    my $found_ext;

    $found_ext->{"name"} = $extdir;

    # Push the results onto the hash array
    push(@{$mpiext_found}, $found_ext);

    # Is there an autogen.subdirs in here?
    process_autogen_subdirs($edir);
}

##############################################################################

sub mpiext_run_global {
    my ($ext_prefix) = @_;

    my $topdir = Cwd::cwd();

    my $dir = "$topdir/$ext_prefix";
    opendir(DIR, $dir) ||
        my_die "Can't open $dir directory";
    foreach my $d (sort(readdir(DIR))) {
        # Skip any non-directory, "base", or any dir that begins with "."
        next
            if (! -d "$dir/$d" || $d eq "base" || substr($d, 0, 1) eq ".");

        # If this directory has a configure.m4, then it's an
        # extension.
        if (-f "$dir/$d/configure.m4") {
            verbose "=== Found $d MPI extension";

            # Check ignore status
            if (ignored("$dir/$d")) {
                verbose " (ignored)\n";
            } else {
                verbose "\n";
                mpiext_process_extension($topdir, $ext_prefix, $d);
            }
        }
    }
    closedir(DIR);
    debug_dump($mpiext_found);

    #-----------------------------------------------------------------------

    $m4 .= "\n$dnl_line
$dnl_line
$dnl_line

dnl Open MPI extensions information
$dnl_line\n\n";

    # Array for all the m4_includes that we'll need to pick up the
    # configure.m4's.
    my @includes;
    my $m4_config_ext_list;

    # Troll through each of the found exts
    foreach my $ext (@{$mpiext_found}) {
        my $e = $ext->{name};
        push(@includes, "$ext_prefix/$e/configure.m4");
        $m4_config_ext_list .= ", $e";
    }

    $m4_config_ext_list =~ s/^, //;

    # List the M4 and no configure exts
    $m4 .= "dnl List of all MPI extensions
m4_define([ompi_mpiext_list], [$m4_config_ext_list])\n";
    # List out all the m4_include
    $m4 .= "\ndnl List of configure.m4 files to include\n";
    foreach my $i (@includes) {
        $m4 .= "m4_include([$i])\n";
    }
}

##############################################################################
# Find and remove stale files

sub find_and_delete {
    foreach my $file (@_) {
        my $removed = 0;
        if (-f $file) {
            unlink($file);
            $removed = 1;
        }
        if (-f "config/$file") {
            unlink("config/$file");
            $removed = 1;
        }
        debug "    Removed stale copy of $file\n"
            if ($removed);
    }
}

##############################################################################
# Find a specific executable and ensure that it is a recent enough
# version.

sub find_and_check {
    my ($app, $app_name, $req_version) = @_;

    my @search_path = split(/;/, $app_name);
    my @min_version = split(/\./, $req_version);
    my @versions_found = ();

    foreach (@search_path) {
        verbose "   Searching for $_\n";
        my $version = `$_ --version`;
        if (!defined($version)) {
            verbose "  $_ not found\n";
            next;
        }

	# Matches a version string with 1 or more parts possibly prefixed with a letter (ex:
	# v2.2) or followed by a letter (ex: 2.2.6b). This regex assumes there is a space
	# before the version string and that the version is ok if there is no version.
	if (!($version =~ m/\s[vV]?(\d[\d\.]*\w?)/m)) {
	    verbose "  WARNING: $_ does not appear to support --version. Assuming it is ok\n";

	    return;
	}

	$version = $1;

        verbose "     Found $_ version $version; checking version...\n";
        push(@versions_found, $version);

        my @parts = split(/\./, $version);
        my $i = 0;
        # Check every component of the version number
        while ($i <= $#min_version) {
            verbose "       Found version component $parts[$i] -- need $min_version[$i]\n";

            # Check to see if there are any characters (!) in the
            # version number (e.g., Libtool's "2.2.6b" -- #%@#$%!!!).
            # Do separate comparisons between the number and any
            # trailing digits.  You can't just "lt" compare the whole
            # string because "10 lt 2b" will return true.  #@$@#$#@$
            # Libtool!!
            $parts[$i] =~ m/(\d+)([a-z]*)/i;
            my $pn = $1;
            my $pa = $2;
            $min_version[$i] =~ m/(\d+)([a-z]*)/i;
            my $mn = $1;
            my $ma = $2;

            # If the version is higher, we're done.
            if ($pn > $mn) {
                verbose "     ==> ACCEPTED\n";
                return;
            }
            # If the version is lower, we're done.
            elsif ($pn < $mn ||
                ($pn == $mn && $pa lt $ma)) {
                verbose "     ==> Too low!  Skipping this version\n";
                last;
            }

            # If the version was equal, keep checking.
            ++$i;
        }

        # If we found a good version, return.
        if ($i > $#min_version) {
            verbose "     ==> ACCEPTED\n";
            return;
        }
    }

    # if no acceptable version found, reject it
    print "
=================================================================
I could not find a recent enough copy of $app.
I need at least $req_version, but only found the following versions:\n\n";

    my $i = 0;
    foreach (@search_path) {
        print "    $_: $versions_found[$i]\n";
        $i++;
    }

    print "\nI am gonna abort.  :-(

Please make sure you are using at least the following versions of the
tools:

    GNU Autoconf: $ompi_autoconf_version
    GNU Automake: $ompi_automake_version
    GNU Libtool: $ompi_libtool_version
=================================================================\n";
    my_exit(1);
}

##############################################################################

sub safe_system {
    print "Running: " . join(/ /, @_) . "\n";
    my $ret = system(@_);
    $ret >>= 8;
    if (0 != $ret) {
        print "Command failed: @_\n";
        my_exit($ret);
    }
    $ret;
}

##############################################################################

sub patch_autotools_output {
    my ($topdir) = @_;

    # Set indentation string for verbose output depending on current directory.
    my $indent_str = "    ";
    if ($topdir eq ".") {
        $indent_str = "=== ";
    }

    # Patch ltmain.sh error for PGI version numbers.  Redirect stderr to
    # /dev/null because this patch is only necessary for some versions of
    # Libtool (e.g., 2.2.6b); it'll [rightfully] fail if you have a new
    # enough Libtool that doesn't need this patch.  But don't alarm the
    # user and make them think that autogen failed if this patch fails --
    # make the errors be silent.
    # Also patch ltmain.sh for NAG compiler
    if (-f "config/ltmain.sh") {
        verbose "$indent_str"."Patching PGI compiler version numbers in ltmain.sh\n";
        system("$patch_prog -N -p0 < $topdir/config/ltmain_pgi_tp.diff >/dev/null 2>&1");
        unlink("config/ltmain.sh.rej");

        verbose "$indent_str"."Patching \"-pthread\" option for NAG compiler in ltmain.sh\n";
        system("$patch_prog -N -p0 < $topdir/config/ltmain_nag_pthread.diff >/dev/null 2>&1");
        unlink("config/ltmain.sh.rej");
    }

    # If there's no configure script, there's nothing else to do.
    return
        if (! -f "configure");
    my @verbose_out;

    # Total ugh.  We have to patch the configure script itself.  See below
    # for explanations why.
    open(IN, "configure") || my_die "Can't open configure";
    my $c;
    $c .= $_
        while(<IN>);
    close(IN);
    my $c_orig = $c;

    # LT <=2.2.6b need to be patched for the PGI 10.0 fortran compiler
    # name (pgfortran).  The following comes from the upstream LT patches:
    # http://lists.gnu.org/archive/html/libtool-patches/2009-11/msg00012.html
    # http://lists.gnu.org/archive/html/bug-libtool/2009-11/msg00045.html
    # Note that that patch is part of Libtool (which is not in this OMPI
    # source tree); we can't fix it.  So all we can do is patch the
    # resulting configure script.  :-(
    push(@verbose_out, $indent_str . "Patching configure for Libtool PGI 10 fortran compiler name\n");
    $c =~ s/gfortran g95 xlf95 f95 fort ifort ifc efc pgf95 lf95 ftn/gfortran g95 xlf95 f95 fort ifort ifc efc pgfortran pgf95 lf95 ftn/g;
    $c =~ s/pgcc\* \| pgf77\* \| pgf90\* \| pgf95\*\)/pgcc* | pgf77* | pgf90* | pgf95* | pgfortran*)/g;
    $c =~ s/pgf77\* \| pgf90\* \| pgf95\*\)/pgf77* | pgf90* | pgf95* | pgfortran*)/g;

    # Similar issue as above -- the PGI 10 version number broke <=LT
    # 2.2.6b's version number checking regexps.  Again, we can't fix the
    # Libtool install; all we can do is patch the resulting configure
    # script.  :-( The following comes from the upstream patch:
    # http://lists.gnu.org/archive/html/libtool-patches/2009-11/msg00016.html
    push(@verbose_out, $indent_str . "Patching configure for Libtool PGI version number regexps\n");
    $c =~ s/\*pgCC\\ \[1-5\]\* \| \*pgcpp\\ \[1-5\]\*/*pgCC\\ [1-5]\.* | *pgcpp\\ [1-5]\.*/g;

    # Similar issue as above -- fix the case statements that handle the Sun
    # Fortran version strings.
    #
    # Note: we have to use octal escapes to match '*Sun\ F*) and the
    # four succeeding lines in the bourne shell switch statement.
    #   \ = 134
    #   ) = 051
    #   * = 052
    #
    # Below is essentially an upstream patch for Libtool which we want
    # made available to Open MPI users running older versions of Libtool

    foreach my $tag (("", "_FC")) {

        # We have to change the search pattern and substitution on each
        # iteration to take into account the tag changing
        my $search_string = '# icc used to be incompatible with GCC.\n\s+' .
                            '# ICC 10 doesn\047t accept -KPIC any more.\n.*\n\s+' .
	                    "lt_prog_compiler_wl${tag}=";
        my $replace_string = "# Flang compiler
      *flang)
	lt_prog_compiler_wl${tag}='-Wl,'
	lt_prog_compiler_pic${tag}='-fPIC -DPIC'
	lt_prog_compiler_static${tag}='-static'
        ;;
      # icc used to be incompatible with GCC.
      # ICC 10 doesn't accept -KPIC any more.
      icc* | ifort*)
	lt_prog_compiler_wl${tag}=";

        push(@verbose_out, $indent_str . "Patching configure for flang Fortran ($tag)\n");
        $c =~ s/$search_string/$replace_string/;
    }

    foreach my $tag (("", "_FC")) {

        # We have to change the search pattern and substitution on each
        # iteration to take into account the tag changing
        my $search_string = '\052Sun\134 F\052.*\n.*\n\s+' .
            "lt_prog_compiler_pic${tag}" . '.*\n.*\n.*\n.*\n';
        my $replace_string = "
        *Sun\\ Ceres\\ Fortran* | *Sun*Fortran*\\ [[1-7]].* | *Sun*Fortran*\\ 8.[[0-3]]*)
          # Sun Fortran 8.3 passes all unrecognized flags to the linker
          lt_prog_compiler_pic${tag}='-KPIC'
          lt_prog_compiler_static${tag}='-Bstatic'
          lt_prog_compiler_wl${tag}=''
          ;;
        *Sun\\ F* | *Sun*Fortran*)
          lt_prog_compiler_pic${tag}='-KPIC'
          lt_prog_compiler_static${tag}='-Bstatic'
          lt_prog_compiler_wl${tag}='-Qoption ld '
          ;;
";

        push(@verbose_out, $indent_str . "Patching configure for Sun Studio Fortran version strings ($tag)\n");
        $c =~ s/$search_string/$replace_string/;
    }

    foreach my $tag (("", "_FC")) {

        # We have to change the search pattern and substitution on each
        # iteration to take into account the tag changing
        my $search_string = 'lf95\052.*# Lahey Fortran 8.1\n\s+' .
            "whole_archive_flag_spec${tag}=" . '\n\s+' .
            "tmp_sharedflag='--shared' ;;" . '\n\s+' .
            'xl';
        my $replace_string = "lf95*)				# Lahey Fortran 8.1
	  whole_archive_flag_spec${tag}=
	  tmp_sharedflag='--shared' ;;
	nagfor*)			# NAGFOR 5.3
	  tmp_sharedflag='-Wl,-shared' ;;
	xl";

        push(@verbose_out, $indent_str . "Patching configure for NAG compiler #1 ($tag)\n");
        $c =~ s/$search_string/$replace_string/;

        # Newer versions of Libtool have the previous patch already. Therefore,
        # we add the support for convenience libraries separately
        my $search_string = "whole_archive_flag_spec${tag}=" . '\n\s+' .
            "tmp_sharedflag='--shared' ;;" . '\n\s+' .
            'nagfor\052.*# NAGFOR 5.3\n\s+' .
            "tmp_sharedflag='-Wl,-shared' ;;" . '\n\s+' .
            'xl';
        my $replace_string = "whole_archive_flag_spec${tag}=
	  tmp_sharedflag='--shared' ;;
        nagfor*)                        # NAGFOR 5.3
          whole_archive_flag_spec${tag}='\$wl--whole-archive`for conv in \$convenience\\\"\\\"; do test  -n \\\"\$conv\\\" && new_convenience=\\\"\$new_convenience,\$conv\\\"; done; func_echo_all \\\"\$new_convenience\\\"` \$wl--no-whole-archive'
          compiler_needs_object=yes
          tmp_sharedflag='-Wl,-shared' ;;
	xl";

        push(@verbose_out, $indent_str . "Patching configure for NAG compiler #2 ($tag)\n");
        $c =~ s/$search_string/$replace_string/;
    }

    # Oracle has apparently begun (as of 12.5-beta) removing the "Sun" branding.
    # So this patch (cumulative over the previous one) is required.
    push(@verbose_out, $indent_str . "Patching configure for Oracle Studio Fortran version strings\n");
    $c =~ s/\*Sun\*Fortran\*\)/*Sun*Fortran* | *Studio*Fortran*)/g;
    $c =~ s/\*Sun\\ F\*\)(.*\n\s+tmp_sharedflag=)/*Sun\\ F* | *Studio*Fortran*)$1/g;

    # See http://git.savannah.gnu.org/cgit/libtool.git/commit/?id=v2.2.6-201-g519bf91 for details
    # Note that this issue was fixed in LT 2.2.8, however most distros are still using 2.2.6b

    push(@verbose_out, $indent_str . "Patching configure for IBM xlf libtool bug\n");
    $c =~ s/(\$LD -shared \$libobjs \$deplibs \$)compiler_flags( -soname \$soname)/$1linker_flags$2/g;

    #Check if we are using a recent enough libtool that supports PowerPC little endian
    if(index($c, 'powerpc64le-*linux*)') == -1) {
        push(@verbose_out, $indent_str . "Patching configure for PowerPC little endian support\n");
        my $replace_string = "x86_64-*kfreebsd*-gnu|x86_64-*linux*|powerpc*-*linux*|";
        $c =~ s/x86_64-\*kfreebsd\*-gnu\|x86_64-\*linux\*\|ppc\*-\*linux\*\|powerpc\*-\*linux\*\|/$replace_string/g;
        $replace_string =
        "powerpc64le-*linux*)\n\t    LD=\"\${LD-ld} -m elf32lppclinux\"\n\t    ;;\n\t  powerpc64-*linux*)";
        $c =~ s/ppc64-\*linux\*\|powerpc64-\*linux\*\)/$replace_string/g;
        $replace_string =
        "powerpcle-*linux*)\n\t    LD=\"\${LD-ld} -m elf64lppc\"\n\t    ;;\n\t  powerpc-*linux*)";
        $c =~ s/ppc\*-\*linux\*\|powerpc\*-\*linux\*\)/$replace_string/g;
    }

    # Fix consequence of broken libtool.m4
    # see http://lists.gnu.org/archive/html/bug-libtool/2015-07/msg00002.html and
    # https://github.com/open-mpi/ompi/issues/751
    push(@verbose_out, $indent_str . "Patching configure for -L/-R libtool.m4 bug\n");
    # patch for libtool < 2.4.3
    $c =~ s/# Some compilers place space between "-\{L,R\}" and the path.\n       # Remove the space.\n       if test \$p = \"-L\" \|\|/# Some compilers place space between "-\{L,-l,R\}" and the path.\n       # Remove the spaces.\n       if test \$p = \"-L\" \|\|\n          test \$p = \"-l\" \|\|/g;
    # patch for libtool >= 2.4.3
    $c =~ s/# Some compilers place space between "-\{L,R\}" and the path.\n       # Remove the space.\n       if test x-L = \"\$p\" \|\|\n          test x-R = \"\$p\"\; then/# Some compilers place space between "-\{L,-l,R\}" and the path.\n       # Remove the spaces.\n       if test x-L = \"x\$p\" \|\|\n          test x-l = \"x\$p\" \|\|\n          test x-R = \"x\$p\"\; then/g;

    # Fix OS X Big Sur (11.0.x) support
    # From https://lists.gnu.org/archive/html/libtool-patches/2020-06/msg00001.html
    push(@verbose_out, $indent_str . "Patching configure for MacOS Big Sur libtool.m4 bug\n");
    # Some versions of Libtool use ${wl} consistently, but others did
    # not (e.g., they used $wl).  Make the regexp be able to handle
    # both.  Additionally, the case string searching for 10.[012]*
    # changed over time.  So make sure it can handle both of the case
    # strings that we're aware of.
    my $WL = '(\$\{wl\}|\$wl)';
    my $SOMETIMES = '(\[,.\])*';
    my $search_string = 'darwin\*\) # darwin 5.x on
      # if running on 10.5 or later, the deployment target defaults
      # to the OS version, if on x86, and 10.4, the deployment
      # target defaults to 10.4. Don\'t you love it\?
      case \$\{MACOSX_DEPLOYMENT_TARGET-10.0\},\$host in
	10.0,\*86\*-darwin8\*\|10.0,\*-darwin\[91\]\*\)
	  _lt_dar_allow_undefined=\'' . $WL . '-undefined ' . $WL . 'dynamic_lookup\' ;;
	10.\[012\]' . $SOMETIMES . '\*\)
	  _lt_dar_allow_undefined=\'' . $WL . '-flat_namespace ' . $WL . '-undefined ' . $WL . 'suppress\' ;;
	10.\*\)';
    my $replace_string = 'darwin*)
      # Open MPI patched for Darwin / MacOS Big Sur.  See
      # http://lists.gnu.org/archive/html/bug-libtool/2015-07/msg00001.html
      case ${MACOSX_DEPLOYMENT_TARGET},$host in
      10.[012],*|,*powerpc*)
	  _lt_dar_allow_undefined=\'${wl}-flat_namespace ${wl}-undefined ${wl}suppress\' ;;
      *)';
    $c =~ s/$search_string/$replace_string/g;

    # Fix ifort support on OSX
    # see https://ntq1982.github.io/files/20200621.html
    $search_string = 'case \$cc_basename in
      nagfor\*\)
        # NAG Fortran compiler
        lt_prog_compiler_wl_FC=\'-Wl,-Wl,,\'
        lt_prog_compiler_pic_FC=\'-PIC\'
        lt_prog_compiler_static_FC=\'-Bstatic\'
        ;;';
    $replace_string = "case \$cc_basename in
      icc* | ifort*)
        #Intel Fortran compiler
        lt_prog_compiler_wl_FC='-Wl,'
        lt_prog_compiler_pic_FC='-fno-common -PIC'
        lt_prog_compiler_static_FC=''
        ;;
      nagfor*)
        # NAG Fortran compiler
        lt_prog_compiler_wl_FC='-Wl,-Wl,,'
        lt_prog_compiler_pic_FC='-PIC'
        lt_prog_compiler_static_FC='-Bstatic'
        ;;";
    $c =~ s/$search_string/$replace_string/g;

    # Only write out verbose statements and a new configure if the
    # configure content actually changed
    return
        if ($c eq $c_orig);
    foreach my $str (@verbose_out) {
        verbose($str);
    }

    open(OUT, ">configure.patched") || my_die "Can't open configure.patched";
    print OUT $c;
    close(OUT);
    # Use cp so that we preserve permissions on configure
    safe_system("cp configure.patched configure");
    unlink("configure.patched");
}

sub export_version {
    my ($name,$version) = @_;
    $version =~ s/[^a-zA-Z0-9,.]//g;
    my @version_splits = split(/\./,$version);
    my $hex = sprintf("0x%04x%02x%02x", $version_splits[0], $version_splits[1], $version_splits[2]);
    $m4 .= "m4_define([OMPI_${name}_MIN_VERSION], [$version])\n";
    $m4 .= "m4_define([OMPI_${name}_NUMERIC_MIN_VERSION], [$hex])\n";
}

sub export_mpi_version {
    my ($name,$version) = @_;
    $version =~ s/[^a-zA-Z0-9,.]//g;
    $m4 .= "m4_define([$name], [$version])\n";
}

sub get_and_define_min_versions() {

    open(IN, "VERSION") || my_die "Can't open VERSION";
    while (<IN>) {
          my $line = $_;
          my @fields = split(/=/,$line);
          if ($fields[0] eq "automake_min_version") {
              if ($fields[1] ne "\n") {
                  $ompi_automake_version = $fields[1];
              }
          }
          elsif($fields[0] eq "autoconf_min_version") {
              if ($fields[1] ne "\n") {
                  $ompi_autoconf_version = $fields[1];
              }
          }
          elsif($fields[0] eq "libtool_min_version") {
              if ($fields[1] ne "\n") {
                  $ompi_libtool_version = $fields[1];
              }
          }
          elsif($fields[0] eq "pmix_min_version") {
              if ($fields[1] ne "\n") {
                  export_version("PMIX", $fields[1]);
              }
          }
          elsif($fields[0] eq "prte_min_version") {
              if ($fields[1] ne "\n") {
                  export_version("PRTE", $fields[1]);
              }
          }
          elsif($fields[0] eq "hwloc_min_version") {
              if ($fields[1] ne "\n") {
                  export_version("HWLOC", $fields[1]);
              }
          }
          elsif($fields[0] eq "event_min_version") {
              if ($fields[1] ne "\n") {
                  export_version("EVENT", $fields[1]);
              }
          }
          elsif($fields[0] eq "mpi_standard_version") {
              if ($fields[1] ne "\n") {
                  export_mpi_version("MPI_VERSION_NUM", $fields[1]);
              }
          }
          elsif($fields[0] eq "mpi_standard_subversion") {
              if ($fields[1] ne "\n") {
                  export_mpi_version("MPI_SUBVERSION_NUM", $fields[1]);
              }
          }
    }
    close(IN);
}

sub in_tarball {
    my $tarball = 0;
    open(IN, "VERSION") || my_die "Can't open VERSION";
    # If repo_rev is not an empty string, we are in a tarball
    while (<IN>) {
          my $line = $_;
          my @fields = split(/=/,$line);
          if ($fields[0] eq "repo_rev") {
              if ($fields[1] ne "\n") {
                  $tarball = 1;
                  last;
              }
          }
    }
    close(IN);
    return $tarball;
}

##############################################################################

sub replace_config_sub_guess {
    # This could be simpler if we could use some Perl modules for this
    # functionality (e.g., DateTime).  But I don't want to introduce
    # any CPAN dependencies here, so just do sometime simple, even if
    # it's a bit laborious. Use a few private helper functions for
    # this kind of functionality.

    sub _get_timestamp {
        my $filename = shift;

        my $ret;
        if (-x $filename) {
            my $out = `$filename --version`;
            $out =~ m/GNU config\.[a-z]+ \((.+)\)/;
            $ret = $1;
        }

        return $ret;
    }

    sub _split_timestamp {
        my $ts = shift;

        $ts =~ m/(\d+)-(\d+)-(\d+)/;
        return $1, $2, $3;
    }

    # Returns true if timestamp $a > timestamp $b.
    sub _timestamp_gt {
        my ($a, $b) = @_;

        my ($year_a, $month_a, $day_a) = _split_timestamp($a);
        my ($year_b, $month_b, $day_b) = _split_timestamp($b);

        # Don't try to be clever -- just do a simple set of explicit
        # comparisons.
        if ($year_a > $year_b) {
            return 1;
        } elsif ($year_a < $year_b) {
            return 0;
        } else {
            if ($month_a > $month_b) {
                return 1;
            } elsif ($month_a < $month_b) {
                return 0;
            } else {
                if ($day_a > $day_b) {
                    return 1;
                } else {
                    return 0;
                }
            }
        }
    }

    my ($topdir) = @_;

    # Find the stashed known-good files, and get their version
    # timestamps.
    my $cached_dir = "$topdir/config/from-savannah";
    my @files = qw/config.guess config.sub/;
    my %known_good_timestamps;
    foreach my $file (@files) {
        my $filename = "$cached_dir/upstream-$file";
        my_die("Cannot find $filename")
            if (! -f $filename);

        my $ts = _get_timestamp($filename);
        $known_good_timestamps{$file} = $ts;
    }

    # Find all config.guess/config.sub files in the tree.  If their
    # versions are older than the stashed known-good files, update
    # them from the stash.
    my @files;
    File::Find::find(sub {
        push(@files, $File::Find::name)
            if ($_ eq "config.guess" ||
                $_ eq "config.sub") }, $topdir);

    foreach my $file (@files) {
        # Skip anything in the 3rd-party tree
        next
            if ($file =~ /\/3rd-party\//);

        my $base = basename($file);
        my $ts = _get_timestamp($file);
        if (_timestamp_gt($known_good_timestamps{$base}, $ts)) {
            print("=== Replacing $file with newer version\n");
            safe_system("cp -f $cached_dir/upstream-$base $file");
        }
    }
}

##############################################################################
##############################################################################
## main - do the real work...
##############################################################################
##############################################################################

# Command line parameters

my $ok = Getopt::Long::GetOptions("no-3rdparty=s" => \$no_3rdparty_arg,
                                  "quiet|q" => \$quiet_arg,
                                  "debug|d" => \$debug_arg,
                                  "help|h" => \$help_arg,
                                  "platform=s" => \$platform_arg,
                                  "include=s" => \$include_arg,
                                  "exclude=s" => \$exclude_arg,
                                  "force|f" => \$force_arg,
                                  "jobs|j=i" => \$automake_jobs,
    );

if (!$ok || $help_arg) {
    print "Invalid command line argument.\n\n"
        if (!$ok);
    print "Options:
  --no-3rdparty <package>       Do not build the listed 3rd-party package (comma separtated list)
  --quiet | -q                  Do not display normal verbose output
  --debug | -d                  Output lots of debug information
  --help | -h                   This help list
  --platform | -p               Specify a platform file to be parsed for no_build
                                and only_build directives
  --include | -i                Comma-separated list of framework-component pairs
                                to be exclusively built - i.e., all other components
                                will be ignored and only those specified will be marked
                                to build
  --exclude | -e                Comma-separated list of framework or framework-component
                                to be excluded from the build
  --force | -f                  Run even if invoked from the source tree of an expanded
                                distribution tarball
  --jobs | -j #                 Value to set for AUTOMAKE_JOBS\n";
    my_exit($ok ? 0 : 1);
}

#---------------------------------------------------------------------------

# Check for project existence
my $project_name_long = "Open MPI";
my $project_name_short = "openmpi";

my_die "Invalid value for --jobs $automake_jobs. Must be greater than 0."
    if (defined $automake_jobs && $automake_jobs <= 0);

if (defined $automake_jobs) {
    $ENV{'AUTOMAKE_JOBS'} = $automake_jobs;
}

#---------------------------------------------------------------------------

$m4 = "dnl
dnl \$HEADER\$
dnl
$dnl_line
dnl This file is automatically created by autogen.pl; it should not
dnl be edited by hand!!
dnl
dnl Generated by $username at " . localtime($ENV{SOURCE_DATE_EPOCH} || time) . "
dnl on $full_hostname.
$dnl_line\n\n";

#---------------------------------------------------------------------------

# Verify that we're in the OMPI root directory by checking for a token file.

my_die "Not at the root directory of an OMPI source tree"
    if (! -f "config/opal_mca.m4");

my_die "autogen.pl has been invoked in the source tree of an Open MPI distribution tarball; aborting...
You likely do not need to invoke \"autogen.pl\" -- you can probably run \"configure\" directly.
If you really know what you are doing, and really need to run autogen.pl, use the \"--force\" flag."
    if (!$force_arg && in_tarball());

# Now that we've verified that we're in the top-level OMPI directory,
# set the sentinel file to remove if we abort.
$sentinel = Cwd::cwd() . "/configure";

#---------------------------------------------------------------------------

my $step = 1;
verbose "Open MPI autogen (buckle up!)

$step. Checking tool versions\n\n";

get_and_define_min_versions();

# Check the autotools revision levels
&find_and_check("autoconf", $ompi_autoconf_search, $ompi_autoconf_version);
&find_and_check("libtool", $ompi_libtoolize_search, $ompi_libtool_version);
&find_and_check("automake", $ompi_automake_search, $ompi_automake_version);

#---------------------------------------------------------------------------

++$step;
verbose "\n$step. Checking for git submodules\n\n";

my @enabled_3rdparty_packages = ();
my @disabled_3rdparty_packages = split(/,/, $no_3rdparty_arg);
# Alias: 'openpmix' -> 'pmix'
if (list_contains("openpmix", @disabled_3rdparty_packages)) {
    push(@disabled_3rdparty_packages, "pmix");
}

# Make sure we got a submodule-full clone.  If not, abort and let a
# human figure it out.
if (-f ".gitmodules") {
    open(IN, "git submodule status|")
        || die "Can't run \"git submodule status\"";
    while (<IN>) {
        $_ =~ m/^(.)[0-9a-f]{40}\s+(\S+)/;
        my $status = $1;
        my $path   = $2;

        print("=== Submodule: $path\n");
        if (index($path, "pmix") != -1 and list_contains("pmix", @disabled_3rdparty_packages)) {
          print("Disabled - skipping openpmix");
          next;
        }
        if (index($path, "prrte") != -1 and list_contains("prrte", @disabled_3rdparty_packages)) {
          print("Disabled - skipping prrte");
          next;
        }

        # Make sure the submodule is there
        if ($status eq "-") {
            print("    ==> ERROR: Missing

The submodule \"$path\" is missing.

Perhaps you forgot to \"git clone --recursive ...\", or you need to
\"git submodule update --init --recursive\"...?\n\n");
            exit(1);
        }

        # See if the commit in the submodule is not the same as the
        # commit that the git submodule thinks it should be.
        elsif ($status eq "+") {
                print("    ==> WARNING: Submodule hash is different than upstream.
         If this is not intentional, you may want to run:
         \"git submodule update --init --recursive\"\n");
        } else {
            print("    Local hash is what is expected by the submodule (good!)\n");
        }
    }
}

#---------------------------------------------------------------------------

# Save the platform file in the m4
$m4 .= "dnl Platform file\n";

# Process platform arg, if provided
if ($platform_arg) {
    $m4 .= "m4_define([autogen_platform_file], [$platform_arg])\n\n";
    open(IN, $platform_arg) || my_die "Can't open $platform_arg";
    # Read all lines from the file
    while (<IN>) {
        my $line = $_;
        my @fields = split(/=/,$line);
        if ($fields[0] eq "enable_mca_no_build") {
            if ($exclude_arg) {
                print "The specified platform file includes an
enable_mca_no_build line. However, your command line
also contains an exclude specification. Only one of
these directives can be given.\n";
                my_exit(1);
            }
            $exclude_arg = $fields[1];
        } elsif ($fields[0] eq "enable_mca_only_build") {
            if ($include_arg) {
                print "The specified platform file includes an
enable_mca_only_build line. However, your command line
also contains an include specification. Only one of
these directives can be given.\n";
                my_exit(1);
            }
            $include_arg = $fields[1];
        }
    }
    close(IN);
} else {
    # No platform file -- write an empty list
    $m4 .= "m4_define([autogen_platform_file], [])\n\n";
}

if ($exclude_arg) {
    debug "Using exclude list: $exclude_arg";
    my @list = split(/,/, $exclude_arg);
    foreach (@list) {
        my @pairs = split(/-/, $_);
        if (exists($pairs[1])) {
        # Remove any trailing newlines
            chomp($pairs[1]);
            debug "    Adding ".$pairs[0]."->".$pairs[1]." to exclude list\n";
            push(@{$exclude_list->{$pairs[0]}}, $pairs[1]);
        } else {
            debug "    Adding $pairs[0] to exclude list\n";
            push(@{$exclude_list->{$pairs[0]}}, "AGEN_EXCLUDE_ALL");
        }
    }
}
if ($include_arg) {
    debug "Using include list: $include_arg";
    my @list = split(/,/, $include_arg);
    foreach (@list) {
        my @pairs = split(/-/, $_);
        if (exists($pairs[1])) {
        # Remove any trailing newlines
            chomp($pairs[1]);
            debug "    Adding ".$pairs[0]."->".$pairs[1]." to include list\n";
            push(@{$include_list->{$pairs[0]}}, $pairs[1]);
        }
        # NOTE: it makes no sense to include all as that is the default
        # so ignore that scenario here, if given
    }
}

#---------------------------------------------------------------------------

++$step;
verbose "\n$step. Running template-generating scripts\n\n";

# These scripts generate fortran header files of different types, but
# guaranteed to have the same value (i.e., so humans don't have to
# maintain two sets of files, and potentially have values get out of
# sync).

my @scripts;
push(@scripts, "ompi/include/mpif-values.pl");

foreach my $s (@scripts) {
    verbose "=== $s\n";
    if (! -x $s) {
        print "Cannot find executable $s!\nAborting.\n";
        my_exit(1);
    }
    if (system($s) != 0) {
        print "Script failed: $s\n";
        my_exit(1);
    }
}

#---------------------------------------------------------------------------

# Find projects, frameworks, components
++$step;
verbose "\n$step. Searching for projects, MCA frameworks, and MCA components\n";

my $ret;

# Figure out if we're at the top level of the OMPI tree or not.
if (! (-f "VERSION" && -f "configure.ac" && -f $topdir_file)) {
    print("\n\nYou must run this script from the top-level directory of the Open MPI tree.\n\n");
    my_exit(1);
}

# Top-level projects to examine
my $projects;
push(@{$projects}, { name => "opal", dir => "opal", need_base => 1 });
push(@{$projects}, { name => "ompi", dir => "ompi", need_base => 1 });
push(@{$projects}, { name => "oshmem", dir => "oshmem", need_base => 1 });

$m4 .= "dnl Separate m4 define for each project\n";
foreach my $p (@$projects) {
    $m4 .= "m4_define([project_$p->{name}], [1])\n";
}

$m4 .= "\ndnl Project names
m4_define([project_name_long], [$project_name_long])
m4_define([project_name_short], [$project_name_short])\n";

# Setup MCA
mca_run_global($projects);

#---------------------------------------------------------------------------

# Handle 3rd-party packages
++$step;
verbose "\n$step. Setup for 3rd-party packages\n";

$m4 .= "\n$dnl_line
$dnl_line
$dnl_line

dnl 3rd-party package information\n";

# Extract the OMPI options to exclude them when processing PMIx and PRRTE
if ( ! (list_contains("pmix", @disabled_3rdparty_packages) &&
	list_contains("prrte", @disabled_3rdparty_packages))) {
    safe_system("./config/extract-3rd-party-configure.pl -p . -n \"OMPI\" -l > config/auto-generated-ompi-exclude.ini");
}

# these are fairly one-off, so we did not try to do anything
# generic. Sorry :).

verbose "=== Libevent\n";
if (list_contains("libevent", @disabled_3rdparty_packages)) {
    verbose "--- Libevent disabled\n";
} else {
    my $libevent_directory = "libevent-" . $libevent_version;
    my $libevent_tarball = $libevent_directory . ".tar.gz";
    if (! -f "3rd-party/" . $libevent_tarball) {
        my_die("Could not find libevent tarball\n");
    }
    $m4 .= "m4_define([package_libevent], [1])\n";
    $m4 .= "m4_define([libevent_tarball], [" . $libevent_tarball . "])\n";
    $m4 .= "m4_define([libevent_directory], [" . $libevent_directory . "])\n";
    verbose "--- Libevent enabled (" . $libevent_version . ")\n";
}

verbose "=== hwloc\n";
if (list_contains("hwloc", @disabled_3rdparty_packages)) {
    verbose "--- hwloc disabled\n";
} else {
    my $hwloc_directory = "hwloc-" . $hwloc_version;
    my $hwloc_tarball = $hwloc_directory . ".tar.gz";
    if (! -f "3rd-party/" . $hwloc_tarball) {
        my_die("Could not find hwloc tarball\n");
    }
    $m4 .= "m4_define([package_hwloc], [1])\n";
    $m4 .= "m4_define([hwloc_tarball], [" . $hwloc_tarball . "])\n";
    $m4 .= "m4_define([hwloc_directory], [" . $hwloc_directory . "])\n";
    verbose "--- hwloc enabled (" . $hwloc_version . ")\n";
}

verbose "=== PMIx\n";
if (list_contains("pmix", @disabled_3rdparty_packages)) {
    verbose "--- PMIx disabled\n";
} else {
    # sanity check pmix files exist
    if (! -f "3rd-party/openpmix/configure.ac") {
        my_die("Could not find pmix files\n");
    }
    push(@subdirs, "3rd-party/openpmix/");
    $m4 .= "m4_define([package_pmix], [1])\n";

    # Grab the unique configure options from each of the 3rd party packages
    safe_system("./config/extract-3rd-party-configure.pl -p \"3rd-party/openpmix/\" -n \"PMIx\" -e config/auto-generated-ompi-exclude.ini > config/auto-extracted-pmix-configure-args.m4");
    # Add the additional configure options from PMIx
    safe_system("./config/extract-3rd-party-configure.pl -p \"3rd-party/openpmix/\" -n \"PMIx\" -l >> config/auto-generated-ompi-exclude.ini");

    verbose "--- PMIx enabled\n";
}

verbose "=== PRRTE\n";
if (list_contains("prrte", @disabled_3rdparty_packages)) {
    verbose "--- PRRTE disabled\n";
} else {
    # sanity check prrte files exist
    if (! -f "3rd-party/prrte/configure.ac") {
        my_die("Could not find pmix files\n");
    }

    verbose "Patching prrte.spec file\n";
    system("$patch_prog -N -p0 < ./config/prrte.spec.diff > /dev/null 2>&1");

    push(@subdirs, "3rd-party/prrte/");
    $m4 .= "m4_define([package_prrte], [1])\n";

    # Grab the unique configure options from each of the 3rd party packages
    safe_system("./config/extract-3rd-party-configure.pl -p \"3rd-party/prrte/\" -n \"PRRTE\" -e config/auto-generated-ompi-exclude.ini > config/auto-extracted-prrte-configure-args.m4");

    verbose "--- PRRTE enabled\n";
}

$m4 .= "\n";

process_autogen_subdirs("3rd-party");

#---------------------------------------------------------------------------

# Find MPI extensions
++$step;
verbose "\n$step. Searching for Open MPI extensions\n\n";
mpiext_run_global("ompi/mpiext");

#---------------------------------------------------------------------------

# Process all subdirs that we found in previous steps
++$step;
verbose "\n$step. Processing autogen.subdirs directories\n";

if ($#subdirs >= 0) {
    foreach my $d (@subdirs) {
        process_subdir($d);
    }
} else {
    print "<none found>\n";
}

#---------------------------------------------------------------------------

# If we got here, all was good.  Run the auto tools.
++$step;
verbose "\n$step. Running autotools on top-level tree\n\n";

# Remove old versions of the files (this is probably overkill, but...)
verbose "==> Remove stale files\n";
find_and_delete(qw/config.guess config.sub depcomp compile install-sh ltconfig
    ltmain.sh missing mkinstalldirs libtool/);

# Remove the old m4 file and write the new one
verbose "==> Writing m4 file with autogen.pl results\n";
unlink($m4_output_file);
open(M4, ">$m4_output_file") ||
    my_die "Can't open $m4_output_file";
print M4 $m4;
close(M4);

# Generate the version checking script with autom4te
verbose "==> Generating opal_get_version.sh\n";
chdir("config");
safe_system("autom4te --language=m4sh opal_get_version.m4sh -o opal_get_version.sh");

# Run autoreconf
verbose "==> Running autoreconf\n";
chdir("..");
my $cmd = "autoreconf -ivf --warnings=all,no-obsolete,no-override -I config -I config/oac";
foreach my $project (@{$projects}) {
    $cmd .= " -I $project->{dir}/config"
        if (-d "$project->{dir}/config");
}
safe_system($cmd);

patch_autotools_output(".");

# Per https://github.com/open-mpi/ompi/issues/8410, replace config.sub
# and config.guess with known-good versions if the Autoconf-installed
# versions are older.
replace_config_sub_guess(".");

#---------------------------------------------------------------------------

verbose "
================================================
Open MPI autogen: completed successfully.  w00t!
================================================\n\n";

# Done!
exit(0);
