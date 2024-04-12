#!/usr/bin/env perl
#
# Copyright (c) 2009-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2015      IBM Corporation.  All rights reserved.
#
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
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
my $topdir_file = "include/pmix.h";
my $dnl_line = "dnl ---------------------------------------------------------------------------";
# The text file we'll write at the end that will contain
# all the mca component directory paths
my $mca_library_paths_file = "config/mca_library_paths.txt";

# Data structures to fill up with all the stuff we find
my $mca_found;
my @subdirs;

# Command line parameters
my $quiet_arg = 0;
my $debug_arg = 0;
my $help_arg = 0;
my $include_arg = 0;
my $exclude_arg = 0;
my $force_arg = 0;

# Include/exclude lists
my $include_list;
my $exclude_list;

# Minimum versions
my $pmix_automake_version = "1.13.4";
my $pmix_autoconf_version = "2.69";
my $pmix_libtool_version = "2.4.2";

# Search paths
my $pmix_autoconf_search = "autoconf";
my $pmix_automake_search = "automake";
my $pmix_libtoolize_search = "libtoolize;glibtoolize";

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

$username = getpwuid($>);
$full_hostname = `hostname`;
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

sub mca_process_component {
    my ($framework, $component) = @_;

    my $cdir = "src/mca/$framework/$component";

    return
        if (! -d $cdir);

    # Process this directory
    my $found_component;

    $found_component = {
        name => $component,
        framework_name => $framework,
        abs_dir => $cdir,
    };

    # Does this directory have a configure.m4 file?
    if (-f "$cdir/configure.m4") {
        $found_component->{"configure.m4"} = 1;
        verbose "    Found configure.m4 file\n";
    }

    # Push the results onto the $mca_found hash array
    push(@{$mca_found->{$framework}->{"components"}},
         $found_component);

    # save the directory for later to create the paths
    # to all the component libraries
    push(@subdirs, $cdir);
}

##############################################################################

sub ignored {
    my ($dir) = @_;

    # If this directory does not have .pmix_ignore, or if it has a
    # .pmix_unignore that has my username in it, then add it to the
    # list of components.
    my $ignored = 0;

    if (-f "$dir/.pmix_ignore") {
        $ignored = 1;
    }
    if (-f "$dir/.pmix_unignore") {
        open(UNIGNORE, "$dir/.pmix_unignore") ||
            my_die "Can't open $dir/.pmix_unignore file";
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
    my ($framework) = @_;

    # Does this framework have a configure.m4 file?
    my $dir = "src/mca/$framework";
    if (-f "$dir/configure.m4") {
        $mca_found->{$framework}->{"configure.m4"} = 1;
        verbose "    Found framework configure.m4 file\n";
    }

    # Did we exclude all components for this framework?
    if (exists($exclude_list->{$framework}) &&
        $exclude_list->{$framework}[0] eq "AGEN_EXCLUDE_ALL") {
        verbose "    => Excluded\n";
    } else {
        # Look for component directories in this framework
        if (-d $dir) {
            $mca_found->{$framework}->{found} = 1;
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

                verbose "--- Found pmix / $framework / $d component: src/mca/$framework/$d\n";

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
                    verbose "    => Ignored (found .pmix_ignore file)\n";
                } else {
                    mca_process_component($framework, $d);
                }
            }
        }
        closedir(DIR);
    }
}

##############################################################################

sub mca_generate_framework_header(\$\@) {
    my (@frameworks) = @_;
    my $framework_array_output="";
    my $framework_decl_output="";
    my $framework_name_output="";

    foreach my $framework (@frameworks) {
        # There is no common framework object
        if ($framework ne "common" and $framework ne "src") {
            my $framework_name = "pmix_${framework}_base_framework";
            $framework_array_output .= "    &$framework_name,\n";
            $framework_decl_output .= "extern pmix_mca_base_framework_t $framework_name;\n";
            $framework_name_output .= "    \"${framework}\",\n";
        }
    }
    $framework_name_output .= "    \"mca\",\n";

    my $ifdef_string = uc "pmix_FRAMEWORKS_H";
    open(FRAMEWORKS_OUT, ">src/include/pmix_frameworks.h");
    printf FRAMEWORKS_OUT "%s", "/*
 * This file is autogenerated by autogen.pl. Do not edit this file by hand.
 */
#ifndef $ifdef_string
#define $ifdef_string

#include \"src/mca/base/pmix_mca_base_framework.h\"

$framework_decl_output

PMIX_EXPORT extern pmix_mca_base_framework_t *pmix_frameworks[];

PMIX_EXPORT extern char *pmix_framework_names[];

#endif /* $ifdef_string */\n\n";
    close(FRAMEWORKS_OUT);

    open(FRAMEWORKS_OUT, ">src/include/pmix_frameworks.c");
    printf FRAMEWORKS_OUT "%s", "/*
 * This file is autogenerated by autogen.pl. Do not edit this file by hand.
 */

#include \"src/include/pmix_config.h\"
#include \"src/include/pmix_frameworks.h\"

pmix_mca_base_framework_t *pmix_frameworks[] = {
$framework_array_output    NULL
};

char *pmix_framework_names[] = {
$framework_name_output    NULL
};

";
    close(FRAMEWORKS_OUT);
}

##############################################################################

sub mca_process_project {

    # Look for framework directories
    my $dir = "src/mca";
    opendir(DIR, $dir) ||
        my_die "Can't open $dir directory";
    my @my_dirs = readdir(DIR);
    @my_dirs = sort(@my_dirs);

    foreach my $d (@my_dirs) {
        # Skip any non-directory, "base", or any dir that begins with "."
        next
            if (! -d "$dir/$d" || $d eq "base" || substr($d, 0, 1) eq ".");

        # If this directory has a $dir.h file and a base/
        # subdirectory, or its name is "common", then it's a
        # framework.
        if ("common" eq $d ||
            (-f "$dir/$d/$d.h" && -d "$dir/$d/base")) {
            verbose "\n=== Found pmix framework: src/mca/$d\n";
            mca_process_framework($d);
        }
    }
    closedir(DIR);
}

##############################################################################

sub mca_run_global {

    # Go find a list of frameworks, and for each of
    # those, go find a list of components.
    mca_process_project();

    # Debugging output
    debug_dump($mca_found);

    $m4 .= "\n$dnl_line
$dnl_line
$dnl_line

dnl MCA information\n";

    # Array for all the m4_includes that we'll need to pick up the
    # configure.m4's.
    my @includes;

    # Write the list of frameworks
    my $frameworks_comma;

    # Print out project-level info
    my @mykeys = keys(%{$mca_found});
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
        push(@includes, "src/mca/$f/configure.m4")
            if (exists($mca_found->{$f}->{"configure.m4"}));

        # This framework does have a Makefile.am (or at least,
        # it should!)
        my_die "Missing src/mca/$f/Makefile.am"
            if (! -f "src/mca/$f/Makefile.am");
    }
    $frameworks_comma =~ s/^, //;

    &mca_generate_framework_header("src", @mykeys);

    $m4 .= "$dnl_line

dnl Frameworks in the pmix project and their corresponding directories
m4_define([mca_pmix_framework_list], [$frameworks_comma])

";

    # Print out framework-level info
    foreach my $f (@mykeys) {
        my $components;
        my $m4_config_component_list;
        my $no_config_component_list;

        # Troll through each of the found components
        foreach my $comp (@{$mca_found->{$f}->{components}}) {
            my $c = $comp->{name};
            $components .= "$c ";

            # Does this component have a configure.m4 file?
            if (exists($comp->{"configure.m4"})) {
                push(@includes, "src/mca/$f/$c/configure.m4");
                $m4_config_component_list .= ", $c";
            } else {
                $no_config_component_list .= ", $c";
            }
        }
        $m4_config_component_list =~ s/^, //;
        $no_config_component_list =~ s/^, //;

        $m4 .= "dnl Components in the pmix / $f framework
m4_define([mca_pmix_${f}_m4_config_component_list], [$m4_config_component_list])
m4_define([mca_pmix_${f}_no_config_component_list], [$no_config_component_list])

";
    }

    # List out all the m4_include
    $m4 .= "$dnl_line

dnl List of configure.m4 files to include\n";
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

    GNU Autoconf: $pmix_autoconf_version
    GNU Automake: $pmix_automake_version
    GNU Libtool: $pmix_libtool_version
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

    # The PGI 10 version number broke <=LT
    # 2.2.6b's version number checking regexps.  We can't fix the
    # Libtool install; all we can do is patch the resulting configure
    # script.  :-( The following comes from the upstream patch:
    # http://lists.gnu.org/archive/html/libtool-patches/2009-11/msg00016.html
    push(@verbose_out, $indent_str . "Patching configure for Libtool PGI version number regexps\n");
    $c =~ s/\*pgCC\\ \[1-5\]\* \| \*pgcpp\\ \[1-5\]\*/*pgCC\\ [1-5]\.* | *pgcpp\\ [1-5]\.*/g;

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
      # PMIx patched for Darwin / MacOS Big Sur.  See
      # http://lists.gnu.org/archive/html/bug-libtool/2015-07/msg00001.html
      case ${MACOSX_DEPLOYMENT_TARGET},$host in
      10.[012],*|,*powerpc*)
      _lt_dar_allow_undefined=\'${wl}-flat_namespace ${wl}-undefined ${wl}suppress\' ;;
      *)';
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
    $m4 .= "m4_define([PMIX_${name}_MIN_VERSION], [$version])\n";
    $m4 .= "m4_define([PMIX_${name}_NUMERIC_MIN_VERSION], [$hex])\n";
}

sub get_and_define_min_versions() {

    open(IN, "VERSION") || my_die "Can't open VERSION";
    while (<IN>) {
          my $line = $_;
          my @fields = split(/=/,$line);
          if ($fields[0] eq "automake_min_version") {
              if ($fields[1] ne "\n") {
                  $pmix_automake_version = $fields[1];
              }
          }
          elsif($fields[0] eq "autoconf_min_version") {
              if ($fields[1] ne "\n") {
                  $pmix_autoconf_version = $fields[1];
              }
          }
          elsif($fields[0] eq "libtool_min_version") {
              if ($fields[1] ne "\n") {
                  $pmix_libtool_version = $fields[1];
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
          elsif($fields[0] eq "flex_min_version") {
              if ($fields[1] ne "\n") {
                  export_version("FLEX", $fields[1]);
              }
          }
    }
    close(IN);
}


##############################################################################

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

my $ok = Getopt::Long::GetOptions("quiet|q" => \$quiet_arg,
                                  "debug|d" => \$debug_arg,
                                  "help|h" => \$help_arg,
                                  "include=s" => \$include_arg,
                                  "exclude=s" => \$exclude_arg,
                                  "force|f" => \$force_arg,
    );

if (!$ok || $help_arg) {
    print "Invalid command line argument.\n\n"
        if (!$ok);
    print "Options:
  --quiet | -q                  Do not display normal verbose output
  --debug | -d                  Output lots of debug information
  --help | -h                   This help list
  --include | -i                Comma-separated list of framework-component pairs
                                to be exclusively built - i.e., all other components
                                will be ignored and only those specified will be marked
                                to build
  --exclude | -e                Comma-separated list of framework or framework-component
                                to be excluded from the build
  --force | -f                  Run even if invoked from the source tree of an expanded
                                distribution tarball\n";
    my_exit($ok ? 0 : 1);
}

#---------------------------------------------------------------------------

# Check for project existence
my $project_name_long = "PMIx";
my $project_name_short = "PMIx";

#---------------------------------------------------------------------------

$full_hostname = `hostname`;
chomp($full_hostname);

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

# Verify that we're in the PMIx root directory by checking for a token file.

my_die "Not at the root directory of an PMIx source tree"
    if (! -f "config/pmix_mca.m4");

$force_arg = 1;

my_die "autogen.pl has been invoked in the source tree of a PMIx distribution tarball; aborting...
You likely do not need to invoke \"autogen.pl\" -- you can probably run \"configure\" directly.
If you really know what you are doing, and really need to run autogen.pl, use the \"--force\" flag."
    if (!$force_arg && in_tarball());

# Now that we've verified that we're in the top-level OMPI directory,
# set the sentinel file to remove if we abort.
$sentinel = Cwd::cwd() . "/configure";

#---------------------------------------------------------------------------

my $step = 1;
verbose "PMIx autogen (buckle up!)

$step. Checking tool versions\n\n";

get_and_define_min_versions();

# Check the autotools revision levels
&find_and_check("autoconf", $pmix_autoconf_search, $pmix_autoconf_version);
&find_and_check("libtool", $pmix_libtoolize_search, $pmix_libtool_version);
&find_and_check("automake", $pmix_automake_search, $pmix_automake_version);

#---------------------------------------------------------------------------

++$step;
verbose "\n$step. Checking for git submodules\n\n";

# Make sure we got a submodule-full clone.  If not, abort and let a
# human figure it out.
if (-f ".gitmodules") {
    print("   Doing some sanity checks for required submodule(s)...\n");

    # Do a quick sanity check to ensure that required
    # submodule(s) are at least present (e.g., they won't be present
    # if you downloaded a GitHub.com-created tarball).
    open(IN, ".gitmodules") ||
        die "Can't open .gitmodules";
    while (<IN>) {
        # Find "path = " lines
        if (!($_ =~ m/^\s+path = (.+)$/)) {
            next;
        }
        my $path = $1;

        # Check that the path exists and is non-empty.
        my $happy = 1;
        my $havefiles = 1;
        if (! -d $path) {
            $happy = 0;
        } else {
            opendir(DIR, $path) ||
                my_die "Can't open $path directory";
            my @files = readdir(DIR);
            closedir(DIR);

            $havefiles = 0
                if ($#files < 4);
        }

        if (!$happy) {
            print("    ==> ERROR: Missing submodule directory

The submodule path \"$path\" is missing.\n\n");
            exit(1);
        }

        if (!$havefiles) {
            print("    ==> ERROR: Missing submodule files

The submodule \"$path\" files are missing.\n\n");
            exit(1);
        }

    }
    if (-d ".git") {
        open(IN, "git submodule status|")
            || die "Can't run \"git submodule status\"";
        while (<IN>) {
            $_ =~ m/^(.)[0-9a-f]{40}\s+(\S+)/;
            my $status = $1;
            my $path   = $2;

            print("=== Submodule: $path\n");

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
}

#---------------------------------------------------------------------------

# No platform file -- write an empty list
$m4 .= "m4_define([autogen_platform_file], [])\n\n";

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

# Find frameworks, components
++$step;
verbose "\n$step. Searching for MCA frameworks and components\n";

my $ret;

# Figure out if we're at the top level of the PMIx tree or not.
if (! (-f "VERSION" && -f "configure.ac" && -f $topdir_file)) {
    print("\n\nYou must run this script from the top-level directory of the PMIx tree.\n\n");
    my_exit(1);
}

$m4 .= "\ndnl Project names
m4_define([project_name_long], [$project_name_long])
m4_define([project_name_short], [$project_name_short])\n";

# Setup MCA
mca_run_global();

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

# Remove the old library path file and write the new one
verbose "==> Writing txt file with all the mca component paths\n";
unlink($mca_library_paths_file);
open(M4, ">$mca_library_paths_file") ||
    my_die "Cannot open $mca_library_paths_file";
my $paths = join(":", @subdirs);
print M4 $paths;
close(M4);

# Run autoreconf
verbose "==> Running autoreconf\n";
my $cmd = "autoreconf -ivf --warnings=all,no-obsolete,no-override -I config -I config/oac";
safe_system($cmd);

patch_autotools_output(".");

# Per https://github.com/open-mpi/ompi/issues/8410, replace config.sub
# and config.guess with known-good versions if the Autoconf-installed
# versions are older.
replace_config_sub_guess(".");

#---------------------------------------------------------------------------

verbose "
================================================
PMIx autogen: completed successfully.  w00t!
================================================\n\n";

# Done!
exit(0);
