#!/usr/bin/env perl
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
# Primitive script to give approximate code counts in the Open MPI tree
#

use strict;
use File::Find;

use Cwd;

# Setup some directories

my $verbose = 0;
my @skip_dirs;

my @code_dirs = ( "src", "include", "test" );
my @doc_dirs = ( "doxygen" );

my @meta_dirs = ( ".svn", ".deps", ".libs", "libltdl" );
my @skip_files = ( "Makefile.in", "Makefile", ".ompi_built" , "config.cache",
                   "libtool", "depcomp", "aclocal.m4", "install-sh",
                   "missing", "mkinstalldirs", "compile", "config.sub",
                   "config.guess", "config.log", "config.status",
                   "TAGS", ".", "configure", "ltmain.sh",
                   "ChangeLog");
my @skip_patterns = ( ".o\$", ".lo\$", ".out\$", "autom4te", ".in\$",
                      ".bak\$", "~\$", ".gz\$", "^stamp-", "^.#", "^#.+#\$",
	              "dynamic-mca" );


# Primitive check to find the top OMPI dir

my @tlds = @code_dirs;
for (my $i = 0; $i <= $#doc_dirs; ++$i) {
    $tlds[$#tlds + 1] = $doc_dirs[$i];
}
my $good = 0;
do {
    $good = 1;
    for (my $i = 0; $i <= $#tlds; ++$i) {
        if (! -d $tlds[$i]) {
            $good = 0;
            last;
        }
    }

    if (!$good) {
        chdir("..");
        my $dir = getcwd();
        if ($dir eq "/") {
            print("Unable to find Open MPI top dir; aborting\n");
            exit(1);
        }
    }
} while ($good != 1);

# Found top dir

print("Found top Open MPI directory: " . getcwd() . "\n");

# Now count up code

my %files_found;
my %dirs_found;

sub wanted {
    # Setup

    my $file = $_;
    my @dirnames = split('/', $File::Find::dir);
    my $dir = $dirnames[$#dirnames];

    # Is the file a dir?

    my $is_dir = (-d $file);

    # Do we want this dir?

    for (my $i = 0; $i <= $#skip_dirs; ++$i) {
        if ($skip_dirs[$i] eq $dir || 
            ($is_dir && $skip_dirs[$i] eq $file)) {
            print("Skipping dir: $File::Find::dir / $file\n")
                if ($verbose);
            $File::Find::prune = 1;
            return 0;
        }
    }

    # Do we want this file?

    for (my $i = 0; $i <= $#skip_files; ++$i) {
        if ($skip_files[$i] eq $file) {
            return 0;
        }
    }
    for (my $i = 0; $i <= $#skip_patterns; ++$i) {
        $file;
        if (/$skip_patterns[$i]/) {
            print("Skipping file pattern: $File::Find::dir/$file\n")
                if ($verbose);
            $File::Find::prune = 1
                if ($is_dir);
            return 0;
        }
    }
    if ($is_dir) {
        print ("Found dir: $File::Find::dir/$file\n")
            if ($verbose);
    } else {
        $files_found{$File::Find::name} = 1;
        print ("Found file: $File::Find::name\n")
            if ($verbose);
    }
    $dirs_found{$File::Find::dir} = 1;
    1;
}

# Look for code

%files_found = ();
%dirs_found = ();
@skip_dirs = @meta_dirs;
for (my $i = 0; $i <= $#doc_dirs; ++$i) {
    $skip_dirs[$#skip_dirs + 1] = $doc_dirs[$i];
}
print("Searching for code files...\n");
find(\&wanted, ".");

my @files = keys(%files_found);
my @dirs = keys(%dirs_found);
print ("Found files $#files\n");
print ("Found dirs $#dirs\n");

# Total files

%files_found = ();
%dirs_found = ();
@skip_dirs = @meta_dirs;
print("Searching for all files (including docs)...\n");
find(\&wanted, ".");

my @files = keys(%files_found);
my @dirs = keys(%dirs_found);
print ("Found files $#files\n");
print ("Found dirs $#dirs\n");
