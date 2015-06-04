Applied the following patches from the upstream hwloc 1.9 branch after
the v1.9.1 release:

All relevant commits up to open-mpi/hwloc@4e23b12 (i.e., the HEAD as
of 27 March 2015).  "Relevant" commits are defined as those that
included files that are embedded in the Open MPI tree (e.g., updates
to files in docs/, utils/, etc. aren't relevant because they are not
embedded in the Open MPI tree).  To be specific, the following commits
have been cherry-picked over to Open MPI:

* open-mpi/hwloc@7c03216 v1.9.1 released, doing 1.9.2rc1 now
* open-mpi/hwloc@b35ced8 misc.h: Fix hwloc_strncasecmp() build under strict flags on BSD
* open-mpi/hwloc@d8c3f3d misc.h: Fix hwloc_strncasecmp() with some icc
* open-mpi/hwloc@f705a23 Use gcc's __asm__ version of the asm extension, which can be used in all standards
* open-mpi/hwloc@307726a configure: fix the check for X11/Xutil.h
* open-mpi/hwloc@ec58c05 errors: improve the advice to send hwloc-gather-topology files in the OS error message
* open-mpi/hwloc@35c743d NEWS update
* open-mpi/hwloc@868170e API: clearly state that os_index isn't unique while logical_index is
* open-mpi/hwloc@851532d x86 and OSF: Don't forget to set NUMA node nodeset
* open-mpi/hwloc@790aa2e cpuid-x86: Fix duplicate asm labels in case of heavy inlining on x86-32
* open-mpi/hwloc@dd09aa5 debug: fix an overzealous assertion about the parent cpuset vs its children
* open-mpi/hwloc@769b9b5 core: fix the merging of identical objects in presence of Misc objects
* open-mpi/hwloc@71da0f1 core: reorder children in merge_useless_child() as well
* open-mpi/hwloc@c9cef07 hpux: improve hwloc_hpux_find_ldom() looking for NUMA node
* open-mpi/hwloc@cdffea6 x86: use ulong for cache sizes, uint won't be enough in the near future
* open-mpi/hwloc@55b0676 x86: use Group instead of Misc for unknown x2apic levels
* open-mpi/hwloc@7764ce5 synthetic: Misc levels are not allowed in the synthetic description
* open-mpi/hwloc@5b2dce1 error: point to the FAQ when displaying the big OS error message
* open-mpi/hwloc@c7bd9e6 pci: fix SR-IOV VF vendor/device names
* open-mpi/hwloc@a0f72ef distances: when we fail to insert an intermediate group, don't try to group further above
* open-mpi/hwloc@e419811 AIX: Fix PU os_index
* open-mpi/hwloc@08ab793 groups: add complete sets when inserting distance/pci groups
* open-mpi/hwloc@c66e714 core: only update root->complete sets if insert succeeds
* open-mpi/hwloc@01da9b9 bitmap: fix a corner case in hwloc_bitmap_isincluded() with infinite sets
* open-mpi/hwloc@e7b192b pci: fix bridge depth
