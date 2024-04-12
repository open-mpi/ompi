PRRTE v2.x series
=================

This file contains all the NEWS updates for all the PRRTE v2.x
series, in reverse chronological order.

2.0.2 -- 11 Feb 2022
--------------------
.. important:: This release contains a workaround that resolves the prior
               conflict with HWLOC versions 2.5.0 through 2.7.0 (inclusive).
               Those versions of HWLOC are now supported.

- PR #1197: Cleanup a bit in prep for PMIx v5 release
- PR #1195: Remove HWLOC version block
- PR #1190: Add missing examples to tarball
- PR #1189: Ensure an error gets reported when rankfile fails


2.0.1 -- 1 Feb 2022
---------------------
.. important:: As of v2.0.1, PRRTE no longer has a dependency on "pandoc"
               for building and installing man pages.

.. important:: PRRTE has identified a bug in HWLOC versions 2.5.0 thru
               2.7.0 (inclusive) that causes PMIx to segfault during certain
               operations. We have worked with the HWLOC developers to fix
               that problem, and the fix will be included beginning in HWLOC
               version 2.7.1. Accordingly, PRRTE now checks for the problem
               HWLOC versions and will error out of configure if they are
               encountered.

- PR #1185 and 1186: Update HWLOC version detection
    - Reject versions 2.5.0-2.7.0, inclusive
- PR #1183: Always build the PBS scheduler support
- PR #1182: Cleanup handling of allocated node names
- PR #1169: Updates for rc5
   - Enable support for PMIX_IOF_OUTPUT_RAW attribute
   - Update NEWS
- PR #1167: Updates
   - Be more flexible in library handling
   - Update libevent/hwloc handling to match PMIx
   - Remove event header defines
   - Minor cleanups and ensure no local IOF copy
     when persistent
   - change the pcc wrapper compiler to a symlink
     to pmixcc
   - Cleanup code a bit
   - Select all adapters with IPv4 addresses within
     specified subnet ranges
   - validate input for mpiexec --with-ft options
- PR #1158: Updates
   - Fix uninitialized variable
   - Update NEWS
- PR #1157: Fix brackets around code clause that caused
     erroneous "unable to find tune file" error
- PR #1156: Handle default param case for HWT CPUs
- PR #1155: Cleanup qualifier detection for mapping policy
- PR #1154: Increase speed of rank computation
   - Remove spurious break in loop
- PR #1153: Ensure the direct-multi test ranks-by slot
- PR #1151: Revamp map-by NUMA support
- PR #1150: Tranlate legacy OMPI params
- PR #1147: Updates
   - Minor infrastructure and example cleanup
   - Allow external caller to set prte_tool_basename
   - Restore support for map/rank/bind by NUMA domains
   - Standardize on use of HWLOC_OBJ_NUMANODE
   - Activate the colocation code and debug it
   - Update man pages
- PR #1145: delete use of PMIX_CHECK_BROKEN_QSORT refs
- PR #1137: Protect against PMIx v4.1.1-only definitions
- PR #1135: Final update v2.0.1 for rc3
   - Show MCA base params in prte_info
   - Silence variety of warnings
   - Start enabling picky compilers when requested
   - Remove unused variables in BMG component
   - Increase verbosity of detected error
   - Remove broken qsort support
   - Add a legacy example
   - Don't specify parent ID from tool
- PR #1104: Update v2.0.1 for rc3
   - Silence Coverity warnings
   - Update portable platform file
   - Only order DVM termination once
   - Abort configure if gcc is <= 4.8.1
   - Fix segmentation fault caused by wrong size for vpids buffer
- PR #1097: Update v2.0.1 for rc2
   - Update NEWS and VERSION
   - Circulate and report local completion status for collectives
   - Don't require pandoc to make a tarball
   - Do not register xml topos
   - Remove the Markdown source files for PRRTE man pages
- PR #1089: Updates
   - Cleanup timeout support to differentiate spawn vs execution time limits
- PR #1086: Multiple commits
   - Use the info list array feature to construct registration data
   - Fix path check for pandoc
- PR #1083: Multiple commits
   - Add MCA param to control device distance computation
   - Correctly report --output-directory and --output-filename as deprecated
   - Check for libevent minimum 2.0.21
   - Properly handle generic "orte" MCA params
   - Cleanups associated with IOF operations
   - Avoid use of MCA params for singleton and report-uri
   - autogen.pl: ignore all excluded components
   - Don't allow PRRTE IOF to block during delivery
   - Update ignores to include Coverity working directory
   - Ensure we hold the IOF data until the PMIx library is done with it
   - Add an "unlock" call to balance the code
   - Use correct ninfo when passing into APIs
   - Don't locally output stdout/err if prterun has a parent


2.0.0 -- 29 Jul 2021
--------------------
.. important:: This is the initial production release of the PRRTE
               software system. It contains full support of the
               PMIx v4.1 Standard plus extensions as provided by
               OpenPMIx v4.1. It therefore requires support from
               that release or above.

Delineating all the changes from the original v1.0.0 tarball would be
overwhelming. The original tarball was provided solely as a means for
packagers to prepare for the eventual production version and was not
intended for general use. As expected, the code base has been extensively
revised/rewritten since that time.

Further changes shall be tracked here in follow-on releases.
