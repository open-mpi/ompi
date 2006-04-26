#%Module -*- tcl -*-
#
# Copyright (c) 2006 The Trustees of Indiana University and Indiana
#                    University Research and Technology
#                    Corporation.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$

# This modulefile is a dispatcher for other LAM modulefiles.  It looks
# around the environment of the machine and determines which LAM
# modulefile to load (i.e., which LAM installation to use).

proc ModulesHelp { } {
  puts stderr "\tThis module acts as a dispatcher to automatically"
  puts stderr "\tload the 'right' LAM/MPI installation into your"
  puts stderr "\tenvironment based on what LAM/MPI installations are"
  puts stderr "\tavailable and the environment available on this machine."
}

module-whatis "Automatically select an appropriate LAM/MPI modulefile to load."

# Don't let any other MPI module be loaded while this one is loaded

conflict mpi

# Directory where we'll find the LAM modulefiles.

set modulefiledir /opt/modules/modulefiles

# See if we have BLCR and/or GM

set have_blcr [file exists /usr/lib/libcr.so]
set have_gm [file exists /usr/lib/libgm.so]

# Get the version number of the LAM represented by this modulefile

set version [lindex [split [module-info name] "-"] 1]

# If we find a matching modulefile, [un]load it.  Otherwise, fail
# silently because we don't want to interject stuff in stderr for fear
# of breaking system-level scripts (e.g., cexec).  The "break"
# statement in the final else clause will ensure that this module is
# actually not loaded.  So fail silently, but under protest.  ;-)

set dir "$modulefiledir/lam"
if { $have_blcr == 1 && $have_gm == 1 &&
     [file exists $dir/lam-with-blcr-and-gm-oscar-$version] } {
  module load $dir/lam-with-blcr-and-gm-oscar-$version
} elseif { $have_blcr == 0 && $have_gm == 1 &&
     [file exists $dir/lam-with-gm-oscar-$version] } {
  module load $dir/lam-with-gm-oscar-$version
} elseif { $have_blcr == 1 && $have_gm == 0 &&
     [file exists $dir/lam-with-blcr-oscar-$version] } {
  module load $dir/lam-with-blcr-oscar-$version
} elseif { [file exists $dir/lam-oscar-$version] } {
  module load $dir/lam-oscar-$version
} else {
  break
}

