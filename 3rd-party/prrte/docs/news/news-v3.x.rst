PRRTE v3.x series
=================

This file contains all the NEWS updates for the PRRTE v3.x
series, in reverse chronological order.

3.0.5 -- 21 Mar 2024
--------------------
- PR #1952: Multiple commits
   - Update docs
   - Update PMIx-related configure code
   - update the hwloc utility code
   - Add missing doc
- PR #1949: Update VERSION and NEWS for release
- PR #1946: Correct deprecation translation for use-hwthread-cpus
- PR #1936: Add missing show_help text
- PR #1933: Check for appfile and ingest it if found
- PR #1931: Multiple commits
   - Fix testing of suicide for daemons
   - Fix daemon suicide and preserve output files
   - Remove unused MCA param
   - Only trigger job failed to start once
   - Add "close stale issues" actions
   - oac: strengthen Sphinx check
   - Revamp the session directory system
   - guard against possible segfault in prted


3.0.4 -- 4 Feb 2024
-------------------
- PR #1918: Multiple commits
   - Support query for number of available slots
   - Add an option '--allow-run-as-root' into prted
   - Update the Python regex for doc build
- PR #1910: Fix the map-by pe-list option
- PR #1908: Multiple commits
   - Use pmix_path_nfs to detect shared file systems
   - Remove debug
   - Remove extra character at end of error messages
   - psets: fix some problems with PMIX_QUERY_PSET_MEMBERSHIP query.


3.0.3 -- 19 Dec 2023
--------------------
- PR #1893: Check for stdatomic.h
- PR #1891: add nodes to job data for ALL jobs included in the launch message
- PR #1888: Multiple commits
   - Minor tweaks of cmd line processing
   - Preserve the clichk test code for future use
- PR #1884: Multiple commits
   - Fix copy/paste error
   - Remove daemonize option from prterun
   - Fix the ras simulator module
   - Preserve backward compatibility
   - Check the PMIx version for min required
   - Correct min version for pmix_getline util
- PR #1877: Multiple commits
   - Remove the "refresh" key before requesting dmodx from PMIx server
   - Shift a code block to enable MCA param directive for ppr mapping
- PR #1870: Multiple commits
   - Begin to restore the detailed help topics
   - docs: add prrte-rst-content to conf.py's exclude_patterns
   - src/docs: update README.txt
   - Consolidate non-component show_help files
   - Add missing file
- PR #1865: Fix the seq mapper
- PR #1860: Multiple commits
   - prte.c: a prefix of "/" is ok
   - Correctly translate the plm_rsh MCA params
   - Fix add-hostfile and add-host operations
   - Add print functions for node, job, and proc flags
   - Add missing "acquire_object"
   - Fix handling of "--" in cmd line
   - Use global PRRTE event base for sigchld callback
   - Remove the event base param to prte_wait_cb
   - Fix OMPI cmd line processing
- PR #1846: Multiple commits
   - Restore the prun help strings
   - Restore prte_info show help topics
   - Recover the pterm show-help strings
- PR #1843: Avoid adding NULL prefix value to app attributes
- PR #1841: Multiple commits
   - Error out of attempts for 32-bit builds
   - Convert OMPI MCA params from legacy orte
   - Fix display of DVM allocation


3.0.2 -- 23 Oct 2023
--------------------
- PR #1832: Update NEWS for release
- PR #1831: Silence error log
- PR #1829: Multiple commits
   - docs/show-help-files: Re-enable Sphinx warning checks
   - Correctly forward stdin to remote procs
- PR #1826: Multiple commits
   - docs: document minimum PMIx version needed
   - prte_setup_pmix.m4: use PMIx min version from VERSION
   - Make checking min versions consistent
   - Update CPPFLAGS for HWLOC config tests
- PR #1822: Fix rpmbuild error
- PR #1821: Roll VERSION to v3.0.2


3.0.1 -- 27 Sep 2023
--------------------
.. important:: This release contains a major revision of the PRRTE
               documentation infrastructure. Accordingly, the various
               documents may contain some number of errors and should
               be regarded as a work-in-progress. We apologize in
               advance for any inconvenience this may create, and will
               continue to improve and correct the documents in future
               releases.

- PR #1818: Update VERSION and rename news file
- PR #1816: Correct confusion over help file names
- PR #1814: Update NEWS infrastructure
- PR #1810: Blacklist the HWLOC GL component to avoid deadlock
- PR #1807: Multiple commits
   - src/docs/show-help-files/Makefile.am: fix syntax
   - docs: Fix up several minor bugs that showed up via CI
   - Prototype Github Actions for PRRTE CI
- PR #1800: Keep trying to find the solution to the docs problem
- PR #1798: Keep trying to fix this Python stuff
- PR #1796: Don't ignore failure to create directory
- PR #1793: build-dummy-ini-files: Be safe for Python 2
- PR #1791: Update to track master branch
- PR #1787: Add support for dynamic definition of process sets
- PR #1784: Multiple commits
   - docs: fix HTML word wapping in table cells
   - Support fix for OMPI Github issue #11532
   - Fix binding to multiple pe's
   - Fix typos in the --enable-dlopen support
   - Fix typo of variable name (PRRTE->PRTE)
   - Remove Doxygen
- PR #1766: Multiple commits
   - Update ignores
   - mca/rmaps: impute process count when np is not set
   - Complete cleanup of map/bind default settings
- PR #1762: v3.0: .github/workflows: update actions versions
- PR #1752: [v3.0] rmaps/base: remove call to hwloc_bitmap_andnot() in bind_generic()
- PR #1747: Respect "--" marker
- PR #1741: [v3.0] RTD: Add .readthedocs.yaml file
- PR #1737: Multiple commits
   - Add debug output in fence upcall
   - Silence TSan data race warning.
- PR #1734: v3.0: backport fixes for ompi v5.0 submodule update
- PR #1727: Protect against bad ppr directives
- PR #1724: Fix one more place
- PR #1721: Just disable the xml map output for ancient hwloc
- PR #1718: Protect against ancient hwloc versions
- PR #1714: Multiple commits
   - Parsable output in an XML format
   - Support odd topologies and relax pe-binding rules
   - Cleanup some formatting and unnecessary debug
- PR #1705: Allow specifying ppr as the default mapping policy
- PR #1702: Multiple commits
   - Recoverable jobs may report more than one proc error over the lifetime
   - Fix double-counting of failed procs in recoverable jobs
- PR #1700: Update headers for rc2
- PR #1696: Always set waitpid-fired flag when proc exits
- PR #1694: Fix passing of envars for spawn
- PR #1692: Multiple commits
   - Plug small memory leaks.
   - Plug memory leaks.
   - Adhere to project's variable declaration convention.
   - Cleanup initial implementation of allocate and session ctrl
   - Protect one more place for Session_control
   - Fix the routing for non-tree-spawn launch
   - Cleanup prte_info output
   - Ensure we exit cleanly when a daemon fails to start
   - Store prted URI under correct name
   - Remove duplicate computation of routing tree
   - Minor cleanup of verbose output
   - Fix typo
   - Send direct to HNP if we don't know a route
   - Cleanup some mangled formatting
   - Cleanup some debug output
   - Update the pmix server integration
- PR #1686: Update config/oac submodule
- PR #1682: Multiple commits
   - Remove wrapper definitions
   - Extend example to check for sessionID and jobID
- PR #1678: Multiple commits
   - add --debug-daemon to prted options
   - Add the rml and routed types to prte_info array
- PR #1674: Ensure cmd line errors provide error messages
- PR #1672: Multiple commits
   - Correct default binding for map-by node/slot
   - Provide better error message for cross-package binding
   - Add missing command line directives
   - Fix minor issues found in tests.
   - Silence compiler warning in test/connect.
   - Silence compiler warnings in examples.
   - Use exit macros in example dmodex.
   - Remove antiquated code that can cause problems
   - Update sphinx requirements
   - Remove the PMIX_SIZE_ESTIMATE support
- PR #1660: Correct function name when using older hwloc versions
- PR #1658: Multiple commits
   - Properly handle overload-allowed
   - Define MIN for environments that lack it
   - Fix a typo in the README, fixes #1645
   - docs: fix Sphinx parsing error
- PR #1644: Multiple commits
   - Add a spawn_multiple test
   - Provide an estimate of the size of registration data
   - Provide size estimates for collective operations that return data
   - Add an option to display the available CPUs
   - Fix generate_dist type
   - Protect against missing attribute definition
   - Set distances array type
   - Add "parseable" display qualifier
   - Generate distances by default
   - Show help message when mapping too low
   - Fix round-robin by obj with multiple cpus/rank
   - Fix map-by slot and node for multiple cpus/rank
- PR #1633: v3.0.x: First cut of Sphinx/ReadTheDocs docs
- PR #1626: Remove unused variable
- PR #1622: Multiple commits
   - Update version to 3.0.1
   - Extend query support to include allocation info
   - Remove debug
   - Include topology in allocated node info
   - Minor touchups of the "fault" example
   - Designate the framework project as PRTE
   - Begin adding support for new server module function pointer
   - Silence warning - add return values
   - Update to compatibility with PMIx master ranch
   - Protect against earlier PMIx versions
   - Add an IOF stress test
   - Accept local group op upcalls
   - Check for and track scheduler attachment
   - Check for map/bind conflicting directive, improve unbound report
   - Don't error out on binding if not required
- PR #1600: Multiple commits
   - Minor change to check_os_flavors
   - Don't emit an error output if not needed
- PR #1597: Fix oac_check_package.m4
- PR #1595: build: fix bashisms in configure


3.0.0 -- 13 Nov 2022
--------------------
.. important:: This release represents a major update to the PRRTE
               code base from prior releases. Many of the changes will be
               transparent to the user. However, there are a few key areas that
               may (likely) impact users. These include:

                 - changes to the cmd line interpreter. PRRTE has provided an
                   abstraction layer that allows programming library developers
                   and environment vendors to customize the cmd line of PRRTE
                   binaries (e.g., prterun and prun). This is governed by the
                   detection of a "personality" that is to be used for defining
                   the cmd line options. The personality can be set either directly
                   on the cmd line itself (via the "--personality foo" option) or
                   is detected by PRRTE thru the environment.
                 - a more extensive "--help" support system.
                 - reduction in the "--rank-by" options.
                 - addition of new "--output" options
                 - removal of the "mindist" mapper due to lack of a maintainer

               There are quite a few more changes than listed here, but they mostly
               pertain to code internals. However, these dictate that PRRTE v3.0.0
               require use of PMIx v4.2 or above to compile. Note that this doesn't
               impact the PMIx level associated with any applications executed by
               PRRTE - the only requirement is that applications link against a
               PMIx version that meets the PMIx cross-version requirements (i.e., a
               minimum of PMIx v2.5).

Detailed changes include:

- PR #1412: Restrict PRRTE to PMIx v4.2+
- PR #1410: Remove schizo/hydra component
- PR #1409: Add new output options to help string
- PR #1407: Support a broader range of IO output formats
- PR #1404: Fix a hang in daemon callback
- PR #1402: Cleanup race condition in daemon reports
- PR #1399: Multiple commits
   - Allow mapping in overload scenario if bind not specified
   - Ensure rankfile and seq mappers computer local and app ranks
- PR #1394: Add forwarding of stdin to indirect example
- PR #1391: Change the default mapping for --bind-to none option to BYSLOT.

Branch from `master@f3d4089 <https://github.com/openpmix/prrte/commit/f3d4089>`_
