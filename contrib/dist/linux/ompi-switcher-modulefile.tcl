#%Module -*- tcl -*-
#
# Copyright (c) 2006 The Trustees of Indiana University and Indiana
#                    University Research and Technology
#                    Corporation.  All rights reserved.
# Copyright (c) 2006 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$

# This modulefile is a dispatcher for other Open MPI modulefiles.  It
# looks around the environment of the machine and determines which
# Open MPI modulefile to load (i.e., which Open MPI installation to
# use).

proc ModulesHelp { } {
  puts stderr "\tThis module acts as a dispatcher to automatically"
  puts stderr "\tload the 'right' Open MPI installation into your"
  puts stderr "\tenvironment based on what Open MPI installations are"
  puts stderr "\tavailable and the environment available on this machine."
}

module-whatis "Automatically select an appropriate Open MPI modulefile to load."

# Don't let any other MPI module be loaded while this one is loaded

conflict mpi

# Directory where we'll find the Open MPI modulefiles.

set modulefiledir /opt/modules/modulefiles

# See if we have BLCR and/or GM

set have_blcr [file exists /usr/lib/libcr.so]
set have_gm [file exists /usr/lib/libgm.so]

# Get the version number of the Open MPI represented by this modulefile

set version [lindex [split [module-info name] "-"] 1]

# If we find a matching modulefile, [un]load it.  Otherwise, fail
# silently because we don't want to interject stuff in stderr for fear
# of breaking system-level scripts (e.g., cexec).  The "break"
# statement in the final else clause will ensure that this module is
# actually not loaded.  So fail silently, but under protest.  ;-)

set dir "$modulefiledir/openmpi"
if { $have_blcr == 1 && $have_gm == 1 &&
     [file exists $dir/openmpi-with-blcr-and-gm-$version] } {
  module load $dir/openmpi-with-blcr-and-gm-$version
} elseif { $have_blcr == 0 && $have_gm == 1 &&
     [file exists $dir/openmpi-with-gm-$version] } {
  module load $dir/openmpi-with-gm-$version
} elseif { $have_blcr == 1 && $have_gm == 0 &&
     [file exists $dir/openmpi-with-blcr-$version] } {
  module load $dir/openmpi-with-blcr-$version
} elseif { [file exists $dir/openmpi-$version] } {
  module load $dir/openmpi-$version
} else {
  break
}

