PMIx v4.x series
================

This file contains all the NEWS updates for the PMIx v4.x
series, in reverse chronological order.

4.2.6 -- 9 Sep 2023
----------------------
.. warning:: CVE-2023-41915

    A security issue was reported by François Diakhate (CEA)
    which is addressed in the PMIx v4.2.6 and v5.0.1 releases.
    (Older PMIx versions may be vulnerable, but are no longer
    supported.)

    A filesystem race condition could permit a malicious user
    to obtain ownership of an arbitrary file on the filesystem
    when parts of the PMIx library are called by a process
    running as uid 0. This may happen under the default
    configuration of certain workload managers, including Slurm.

Detailed changes include:
 - PR #3150 Do not follow links when doing `chown`
 - PR #3147 Multiple commits
    - Retrieve pset names upon PMIx_Get request
    - Add a new "pctrl" tool for requesting job control ops
 - PR #3143 Multiple commits
    - Properly support the "log" example
    - Enable building of tarball
    - show_help: strip leading/trailing blank lines
    - docs: fix some leftover "Open MPI" references
    - docs: fix HTML word wapping in table cells
    - Improve error handling in setup_topology
    - Minor cleanups for disable-dlopen
    - Fix Python bindings
 - PR #3132 Multiple commits
    - Switch to using event lib for connections
    - Roll to v4.2.6

4.2.5 -- 6 Aug 2023
----------------------
 - PR #3121 Multiple commits
    - Allow to override build date with SOURCE_DATE_EPOCH
    - Adjust NEWS and VERSION for release
 - PR #3120 Multiple commits
    - Repair the pattrs utility
    - docs: speed up Sphinx processing
    - Fix bitrot in pquery tool
    - Fix bitrot in plookup
    - Construct the array of event strings
    - Add new API and cleanup pevent tool
    - Minor formatting cleanups
    - Expose pmix_getline utility
 - PR #3102 Multiple commits
    - Protect against HWLOC v3 and above
    - pmix_hwloc.c: use NUMANODE by default
    - pmix_hwloc.h: PACKAGE/NUMANODE replaced SOCKET/NODE in 1.11, not 1.10


4.2.4 -- 17 Jun 2023
----------------------
 - PR #3093 Require cherry picks and update github actions
 - PR #3086 Multiple commits
    - Handle the case of unbound proc. Also ensure topology is available
    - Remove the group from client tracking when destruct is done
    - Return the correct status from PMIx_Init
    - Update asm config, shmem ref counting, atomic ops.
    - Remove environ from public header
    - Update MCA param file handling
    - Cleanup changes and update to rc2
 - PR #3081 Remove unimplemented component
 - PR #3078 Eliminate double-free
 - PR #3066 RTD: Add .readthedocs.yaml file
 - PR #3064 Add missing frees
 - PR #3060 Multiple commits
    - Provide tool for checking compatibility between two PMIx libraries
    - Ensure tools always send their event registration to their servers
    - Include the OAC subdir in the autoconf make directories
 - PR #3058 setserver fixes for v4.2 branch
 - PR #3053 Multiple commits
    - Attempt to resolve gcc13 complaint
    - Add/update documentation
    - PMIx: Fix compile error in macro
    - Ensure tools send requests to their server
    - Sort the procs for a fence operation
    - Silence UBSan warning
    - Make pmix_test MSan clean
    - Silence TSan data race warnings
    - Avoid python setup.py installdir error
    - Remove duplicate atomic definitions
 - PR #3029: Fix a few corner cases in the cmd line parser
 - PR #3009: Fix fetch of globally unique keys
 - PR #3008: Multiple commits
    - Plug memory leaks
    - Convert majority of PMIX_*_FREE() macros
    - Improve PMIX_*_RELEASE() consistency
    - Silence couple of nit warnings
    - Complete PMIX_*_FREE() conversion
    - Fix some Python bindings errors
    - Correct cbdata type in pmix_show_help callback function
    - Cleanup code style, fix bit checks, and fix backward compatibility
 - PR #3002: Update OAC submodule pointer
 - PR #2992: Multiple commits
    - Update rpm spec file
    - Silence debug output
    - Avoid large stack allocations in pmix_bfrops_base_tma_setenv()
 - PR #2977: Fix handling of cmd line arguments
 - PR #2972: Handle the case where prterun is given no options
 - PR #2966: Update sphinx requirements
 - PR #2964: Trim v5 crossover from the Python bindings
 - PR #2963: Multiple commits
    - Remove the PMIX_SIZE_ESTIMATE attribute
    - Remove GDS "set_size" entry points


4.2.3 -- 7 Feb 2023
-------------------
 - PR #2959 Move release of pmix_client_globals.myserver to later
 - PR #2937 Multiple commits
    - Update exceptions doc
    - Disable the "sentinel" attribute in Solaris
    - Handle some Solaris errors/warnings
    - Hide unused params
    - Turn off the "format" attribute on Solaris
 - PR #2927 Add option to abort on component find failure
 - PR #2922 Fix memory leak in pmix_hash_fetch
 - PR #2920 ptl/base: retry recv() when it encounter EAGAIN or EWOULDBLOCK
 - PR #2913 Multiple commits
    - Fix some backport issues
    - Fix component name declarations
    - Silence unnecessary repository item warnings
    - Silence unnecessary warnings
    - Correct libpmix_mca_common_dstore versioning
    - Fix the static build
 - PR #2911 Multiple commits
    - Avoid double-caching of events
    - Add const qualifier to nspace strings
    - Provide support for estimating the size of value objects
    - Enable size estimates for modex-like operations
    - Fix segfault in fence operation with groups
    - Add an attribute to request display of available cpus
    - Initialize client topology
    - Add attribute to stipulate parseable output for display info
    - Add malloc return check in bfrops for byte object unpack
    - Add a draft security policy
    - Update docs/security.rst
    - Provide pointer to online security policy in README.md
    - Missing cleanups
 - PR #2902 Multiple commits
    - First cut of Sphinx / ReadTheDocs docs
    - Update Github actions to use submodules
    - build: Update to use OAC_C_COMPILER_VENDOR
    - Update OAC submodule pointer
    - First cut of Sphinx / ReadTheDocs docs
    - mlnx Github action: install Sphinx
    - docs: put restrictions on Sphinx versions
 - PR #2895 python: updates for Python bindings
 - PR #2885 Protect against NULL topology when destructing pmix_topology_t
 - PR #2882 Multiple commits
    - Minor compatibility touchup
    - Cleanup
    - Fix singleton support
    - Re-implement the timeout support for purely local grp ops
    - Fix local group operations
    - Correctly identify Cython as a required package
    - Move SIGCHLD capture to aux event base
    - Silence a warning during Python binding install
    - Fix a typo in the macro backers and add a "stop" pt in deprecated.h
    - Silence a few Coverity complaints
    - Add an attribute for passing an auxiliary event base
    - Complete the macro-to-function conversion
    - Continue macro converstion to functions
    - Checkpoint work on macro conversion to functions
    - Extend handling of bool MCA params
    - Fix a couple of bugs in the modex/get path
    - Convert macros to functions
    - Always allow the GDS to use the namespace list
    - Ensure the ptl connection handler includes the provided info
    - Change function signature of mark_modex_complete().
    - Add some scheduler integration support
    - Add hooks for GDS components to handshake modex complete
    - Include deprecated definitions in Python bindings
    - Add attribute to report index of topology in storage array
    - Add attribute to query allocation info
    - Add an API to pretty-print pmix_app_t structures
    - Try both peer and server storage for connect info
    - Remove debug print statements
    - Allow gds function fallback to hash take II.
    - Add support for scheduler connections
    - Cleanup a couple of warnings in Python bindings
    - Add an API and attribute
    - Avoid infinite loop in fabric registration
    - Roll version to 4.2.3
 - PR #2829: Multiple commits
    - Minor correction to check_os_flavors
    - Allow Python tool to set server module functions
    - Some repairs to the Python bindings
    - Fix the Python tests
 - PR #2828: Remove chatty error log output
 - PR #2823: Some cleanup of the Python bindings build system
 - PR #2821: Correct return codes for two APIs
 - PR #2817: Modify the pmix_output system
 - PR #2813: Fix bashism in oac_check_package.m4
 - PR #2811: Multiple commits
    - build: fix bashisms in configure
    - build: fix -Wstrict-prototypes
 - PR #2808: pmix_list: fix a bug in pmix_list_insert()
 - PR #2806: Multiple commits
    - Have python bindings properly setup the env
    -  The PMIx_IOF_Push() function can take a NULL option for its
       buffer object. Update Python bindings so it can use this.
 - PR #2803: oneapi (and probably llvm): patch to allow
             pmix tests to compile using icx, icpx, etc.
 - PR #2801: intel oneapi: fix a munge code error


4.2.2 -- 25 Oct 2022
--------------------
.. important:: This is the minimum version required to support PRRTE v3.0.

- PR #2799: Multiple commits
- Add const qualifier to pset_name
- Fix one place that complained about lost qualifier
- PR #2797 Silence complaint about enum vs int
- PR #2793 Update NEWS
- PR #2792 Multiple commits
- Handle app-info in the gds/hash component
- Handle session-info in the gds/hash component
- PR #2790 Update NEWS
- PR #2789 Multiple commits
- Cleanup some store/retrieve issues
- Stop-in-init applies to all procs in a job
- PR #2787 Update EXCEPTIONS
- PR #2783 Multiple commits
- Add some debug macros for tracking key values
- Provide a little more useful error output
- PR #2777 Multiple commits
- llvm/oneapi: fixes to bring pmix up to iso c99
- pnet/nvd: Fix macro escaping issue
- Enhance the performance of the var_scope_push/pop script
- PMIX_OBJ_STATIC_INIT: fixed initialization
- PR #2775 Plug amemory leaks
- PR #2772 Update headers for release
- PR #2771 Plug a memory leak
- PR #2770 Multiple commits
- Fix the "check_cli_option" code
- Provide more detailed process failure codes and fix
  CLI parsing
- pmix_reinit: a fix to allow PMIx to be reinitable
- Update specfile BuildRequires
- Additional BuildRequires in spec
- PR #2766 Roll to v4.2.2


4.2.1 -- 13 Sept 2022
---------------------
 - PR #2754 Multiple commits
    - Export the output_stream_t class declaration
    - Update NEWS for release
 - PR #2752 Catch missing library renames
 - PR #2751 Multiple commits
    - Remove stale m4 and unimplemented function declaration
    - Mark that proc arrays being passed have been sorted
    - Add improved debug and correct param passing to pmix_init_util
 - PR #2747 Final prep for release
 - PR #2746 Ensure tools relay events to their server
 - PR #2744 Multiple commits
    - Clean up leftover .gitignore entry
    - Fix a number of Coverity issues
    - Add a couple of macro definitions
 - PR #2739 Multiple commits
    - Consistently use PMIx_Error_string in client example
    - Convert the MCA parameter for "show_load_errors"
 - PR #2734 Add some detail to warning output by flex detector
 - PR #2731 Do not set the buffer type in construct
 - PR #2728 Prep for v4.2.1rc1
    - Add some attributes to support job launch
    - Update EXCEPTIONS, NEWS, VERSION for v4.2.1rc1
 - PR #2725 Multiple updates
    - Make the session info array support conform to the standard
    - Remove stale common/sse code and cleanup pnet/sshot configure
    - Resolve confused use of PMIX_UNIV_SIZE for PMIX_JOB_SIZE in
      test code
    - Minor cleanups
 - PR #2718 Release GIL before registering event handler in Python
      bindings
 - PR #2716 Multiple commits
    - Add support for HPE Slingshot fabric
    - Add runtime options attribute
 - PR #2713 Multiple commits
    - iof: Fix merging of stderr to stdout.
    - Fix bad dereferences when passed a NULL parameter to PMIx_Init
    - Add new attribute definitions to support display options
 - PR #2706 Remove man pages
 - PR #2703 Fix flex detection
 - PR #2700 Multiple commits
    - Fix the buildrpm script
    - Enable show_help output on tools
    - Bump VERSION to v4.2.1


4.2.0 -- 20 Aug 2022
--------------------
.. important:: This release includes a number of new features that
               may be of use to library and application developers. These include:

                 * support for qualified values - i.e., the ability to reuse an
                   attribute, assigning it different values with each value
                   contingent upon one or more qualifiers. Thus, requests to
                   return the value can specify the corresponding qualifiers
                   to identify the specific version of the value being requested.
                 * provide additional information to be included in group construct
                   operations. The result of the operation shall include exchange
                   of such information with all participants, with the information
                   "qualified" by the assigned group context ID.
                 * new output formats that allow prepending output streams with
                   the hostname and pid of the source process
                 * improved support for tools that allow connection to multiple
                   simultaneous servers and better handshakes for establishing
                   connections
                 * fixes for access to session/node/app-realm information
                 * broader support for pretty-print of PMIx structures such
                   as pmix_info_t and pmix_value_t
                 * compliance with the new PMIx ABI definitions. This includes
                   converting some macros to functions, with macros retained
                   for backward compatibility
                 * capture and forwarding of default MCA parameter file values,
                   both from the system and user level

Detailed changes:
 - PR #2697 Multiple commits
    - Add example to simulate OMPI group usage
    - Cleanup singleton IOF lists
 - PR #2695 Output IO as singleton, support background commands
 - PR #2692,2690 Silence gcc12 warnings
 - PR #2689 Need to replace the entire proc in fence with group member
 - PR #2687 Update NEWS/VERSION for rc2
 - PR #2686 Fix/implement the group invite support
 - PR #2682 Do not error out if lib is symlinked to lib64
 - PR #2681 Separate out pinstalldirs for inclusion by PRRTE
 - PR #2679 Fix the tm configure logic
 - PR #2675 Minor cleanup of timestamp output
 - PR #2673 Update NEWS, remove unready components, fix missing var
 - PR #2672 Fix make_tarball and remove unused variable
 - PR #2670 Support broader range of output formats
 - PR #2668 Multiple commits
    - Sort proc arrays to remove order sensitivity
    - Fix multi-node group info distribution
 - PR #2666 Coordinate psec modules across pfexec child
 - PR #2665 Complete implementation of group info exchange
 - PR #2659 Multiple commits
    - Fix IOF of stdin
    - Protect "create" macros from zero entries
    - Return the correct code for register fabric
    - Forward stdin to apps started using pfexec
 - PR #2651 Multiple commits
    - Enable picky compiler options by default in Git repo builds
    - Remove bad destruct call
    - Fix PMIX_INFO_PROCESSED macros
    - Update show-help system
    - Fix show_help output to include tools in distribution
    - Fix dmodex operations
    - Properly cast the pmix_list_item_t struct
    - Fix potential use after free in tests
    - Add "const" qualifiers to some string print APIs
    - Cleanup some debug output
    - construct_dictionary.py: make .format() safe for Python 2
    - src/include/Makefile.am: avoid potential file corruption
    - Stop multiple invocations of debugger-release
    - Update the dmodex example
 - PR #2629 Multiple commits
    - Setup PMIX_STD_ABI_VERSION in the VERSION file
    - Define the PMIX_QUERY_ABI_VERSION attribute
    - Backend query support for PMIX_QUERY_ABI_VERSION and local keys
    - Add examples for using PMIx_Query_info with PMIX_QUERY_ABI_VERSION
    - Add PMIx Standard version info to pmix_info
    - Fix pcompress/zlib implementation
    - Return "succeeded" status when outputting help/version info
 - PR #2623 Fix greek versioning
 - PR #2614 Fix retrieval of node/app/session-realm info
 - PR #2613 Some minor cleanups for picky compilers
 - PR #2612 Some initial valgrind cleanup
 - PR #2610 Multiple commits
    - Remove unnecessary function call in pmix_gds_hash_fetch()
    - pmix_fd: cap the max FD to try to close
    - Support colocation of processes
    - Optimize the file descriptor cleanup on OSX
    - Require flex only when keyval_lex.c is not provided
    - Fix hwloc verbose output
 - PR #2601 Initialize pmix_info_t flags when loading
 - PR #2594 Backport the utility and class exposure to support PRRTE
 - PR #2588 configure.ac: update directory space check
 - PR #2585 configury: do look for sed
 - PR #2576 Refactor show_help() to use the PMIx_Log() api
 - PR #2567 Make pmix_common.h stand alone
 - PR #2564 Error out if no atomic support is available
 - PR #2543 Properly deal with delayed local get requests
 - PR #2540 Ensure we get correct return status
 - PR #2538 Multiple commits
    - Fix warning - compare of different signs
    - Fix dmodex operation on local host
 - PR #2535 Update the configure logic to track master
 - PR #2534 Initialize size for getsockopt() and revert bad free
 - PR #2533 Example fixes
 - PR #2532 Protect critical zone in pmix_obj_update()
 - PR #2518 Prohibit Python bindings with non-shared lib builds
 - PR #2517 Fix Coverity warnings
 - PR #2516 Properly handle queries of tools
 - PR #2507 Properly handle tools that have tools connected to them
 - PR #2506 Add print APIs and update pquery to use them
 - PR #2505 Update configure flags
 - PR #2504 Don't search home component path if not present
 - PR #2502 Add missing function and improve error message
 - PR #2460 Multiple commits
    - Remove unneeded atomics code
    - Begin stripping configure of unnecessary checks
    - Initialize the mutex when constructing an object
    - Sync the library to the Standard
    - convert pmix_value_xfer to PMIx_Value_xfer
    - pmix_iof.c: malloc buffer before memcpy()
    - Clean up unused return value warnings
    - Remove unnecessary sys/sysctl.h includes
    - Include typedef for GCC builtin atomics


4.1.2 -- 11 Feb 2022
--------------------
.. important:: This release contains a workaround that resolves the prior
               conflict with HWLOC versions 2.5.0 through 2.7.0 (inclusive).
               Those versions of HWLOC are now supported.

- PR #2453: Avoid string literals in environ
  - Be defensive against string literals in env
  - Remove block of hwloc 2.5 - 2.7
  - Adjust Mellanox CI Dockerfile so it can build


4.1.1 -- 1 Feb 2022
-------------------
.. important:: As of v4.1.1, PMIx includes an EXCEPTIONS file that lists
               all deviations from the PMIx Standard. This primarily includes
               extensions that have not yet been adopted by the Standard.

.. important:: As of v4.1.1, PMIx no longer has a dependency on "pandoc"
               for building and installing man pages.

.. warning:: PMIx has identified a bug in HWLOC versions 2.5.0 thru
             2.7.0 (inclusive) that causes PMIx to segfault during certain
             operations. We have worked with the HWLOC developers to fix
             that problem, and the fix will be included beginning in HWLOC
             version 2.7.1. Accordingly, PMIx now checks for the problem
             HWLOC versions and will error out of configure if they are
             encountered.

- PR #2445 and 2447: Update HWLOC version detection
- Reject versions 2.5.0-2.7.0, inclusive
- PR #2428: Update for rc6
- Enable buffered IOF output
- Cleanups and docs for rc6
- PR #2426: Updates from master
- Updates to cleanup conflicts and touchups
- Silence Coverity warnings
- Be more flexible in library handling
- Finish cleaning up nocopy behavior
- test_v2: use static declaration for client parser
- Respect the nocopy qualifier
- Add static library note to README
- PMIX_HAVE_LIBEV and PMIX_HAVE_LIBEVENT flags must always
    be defined
- Fix two bugs in PMIX_FLAGS_APPEND_MOVE
- Fix a problem using PMIX_RANK
- Final minor diddles of configure summary categories
- Add configure support for pgpu/pnet components
- libevent: prefer compiler tests over linking tests
- Cleanup libevent/libev selection logic
- Remove pkg-config dependency list
- Add wrapper compiler mca link argument passing
- Reintroduce PMIX_DYN_LIB_SUFFIX define
- config: remove string checks in hwloc/libevent
- Fix devel-check of test_v2
- Silence Coverity warning and cleanup code
- Fix a number of warnings and cleanup a few things
- Select all adapters with IPv4 addresses with specified
  subnet ranges
- Fix environmental variable name in help-pmix-runtime
- Remove curl/jansson default search assumption
- Remove cobuild remnants from configure
- V2 suite test case for multiple inits and finalizes
- PR #2410: Mark dependencies private in pkg-config file
- PR #2396: Fix the network support components
- PR #2394: Update for landing zone 1
- Update NEWS/Version
- PR #2393:
- Correct copy/paste error - use correct procID
- Add a little debug info to a verbose output
- PR #2389: delete use of PMIX_CHECK_BROKEN_QSORT refs
- PR #2384: Final update for v4.1.1 rc5
- Ensure a param is always initialized
- Provide static initializers for all structures
- Stop in init if rndz URI given
- Update EXCEPTIONS/NEWS files
- PR #2380: Update 4.1.1 rc5
- Improve handling of compiler version string
- Fix corner case on iof flags
- Squash unused variable warnings
- Remove duplicate defines of client build dependencies
- Ensure we terminate the input channel when done
- Ensure pmix library gets a chance to cleanly terminate children
- Avoid ABI break in mid-series
- Define a static initializer for data_buffer_t
- Fix typos - replace OMPI with PMIx
- Silence Coverity concerns
- Make the backward-compatible ABI functions visible
- Avoid warning on void function return
- Allow operation if ONLY a loopback device is present
- PR #2332: Update 4.1.1 rc4
- Modify configure logic
- Add a missing helpfile (util) and few fixups
- Fix --output to ignore err on existing dirs
- Squash unused param warnings
- PR #2317: Update 4.1.1 rc3
- Add test_v2 to autoconf/automake processing
- Silence Coverity warnings
- Fix resource leak
- Change construct.py to mark PMIx functions with nogil
- Enable ultra-picky compiler options
- Ensure picky flags not set until after AC is done
- Add missing simptest.h file to tarball
- Expand the tm/pbs config to check for lib64
- Update portable platform file
- Abort configure if gcc is <= v4.8.1
- Enable support for address sanitizers, but only on request
- Fix issues raised by picky compiler checks
- Don't check for Python 3.4+ if not building the Python bindings
- Update VERSION and NEWS
- PR #2299: Update 4.1.1 rc2
- Update VERSION and NEWS
- Add some spawn-specific timeout attributes
- Resolve race condition in lost connection
- Provide "partial_success" error when collectives not complete
- Only conditionally decode the nspace return value when we
  are sure the spawn was successful
- Correctly copy stone age hwloc topologies
- Remove man page Markdown source and build dependency on pandoc
- PR #2277: Update v4.1.1 release candidate
- Add missing m4 file
- Add a Standard extension value to the compliance version
- Properly read/output stdout/err from a fork/exec'd child
- Default to using our local_output flag
- Cleanup compiler warnings for ancient hwloc versions
- Prefix the output files with "pmix"
- Ensure tools wait until all active events are processed
- autogen.pl: ignore all excluded components
- Don't treat inability to open shmem file as fatal
- Avoid use of MCA params for singleton and report-uri
- Ensure the server waits for all IOF and message events to complete
- Restore the thread join in progress thread "stop"
- Mark the read event as no longer active
- Avoid blocking in the stdin read handler
- Some cleanup of IOF output
- Add missing .m4 files to extra_dist
- Check for libevent minimum 2.0.21
- Add Intel GPU component
- Correct vendor IDs and generalize check_vendor
- Add missing storage-related datatype support
- Add missing storage constants
- Improve pnet component selection
- Cleanup the device distance computation
- PR #2257: Check for libevent minimum 2.0.21
- PR #2253: Fix up string creation functions, take the GIL in the callback code,
  and system malloc instead of the Python malloc for datastructures
  going to PMIx
- PR #2250: Update attribute support tables


4.1.0 -- 29 July 2021
---------------------
.. important:: This release implements the complete PMIx v4.1 Standard
               and therefore includes a number of new APIs and features. These
               are fully documented in the official document. It also includes
               some extensions that have not yet been included in that document.

Beyond the v4.1 modifications and additions to APIs, datatypes, attributes,
and macros, changes to the library include:

 - PR #2251: More updates from master
     - Replaced PMIx_Notify_Event with cbfunc call in errhandler to match
     - Update attribute support tables
 - PR #2248: Continue updates to support MPICH integration
     - Extend IOF outputting format to cover Hydra options.
 - PR #2246: Cleanup some IOF attributes
 - PR #2235: Cherry-pick updates from master branch
     - Default tools to outputting their IOF
     - Initalize val before get in case get isn't successful
     - Some cleanups of the event notification and keepalive support
     - Remove stale travis.yml file
     - Update simptest to truly support PMIx_Abort
     - Some cleanups for client finalize and IOF output
     - Do not forward cached IOF to self
     - Update how C to Python bytes/strings are handled for get and byte objects
     - Add test_v2 directory
     - Add configure logic for RM and sse support
     - Add pstat framework
     - Remove duplicate PMIx_Data_load and PMIx_Data_unload definitions
     - Add manpage files for tools
     - Add sse common component
     - Add the prm components
     - Add the storage framework
     - General update of code base to track master branch
 - PR #2224: common/dstore: Fix inconsistent Makefile.am
 - PR #2216: Cherry-pick updates from master branch
     - Add missing osname endpt elements to bfrops
     - Optimize check for nodes
     - Transfer stdout/err formatting to PMIx
     - Ensure tool output of IOF
 - PR #2208: Cherry-pick updates from master branch
     - Enable re-init of clients
     - Add attribute to indicate copy/nocopy of output directed to files
 - PR #2204: Add a few job error constants
 - PR #2201: Cherry-pick updates from master branch
     - Minor updates based on Standards review
     - Correct references to help-ptl-tool.txt
     - Protect register_nspace against new entries
     - Add oversubscribed attribute
 - PR #2195: configury: Use AC_CHECK_ALIGNOF and fix cross-compiling
 - PR #2190:Cherry-pick updates from master branch
     - Remove duplicative pmdl/ompi directories
     - Add missing m4 file
     - Update hwloc support to handle revised version string
     - Register ompi5 and ompi4 as aliases for ompi plugin
     - Correct the PMIx_Get signature
     - Silence some gcc warnings
     - Silence some gcc11 warnings
     - Protect against bad nspace input
     - Cleanup few lingering gcc11 warnings
     - Protect against duplicate envar harvesting
 - PR #2177: Cleanup shadow variables in dstore base and components
 - PR #2156: Ensure we pass the desired scope on a PMIx_Get call
 - PR #2170: Remove non-required items
 - PR #2168: Cherry-pick updates from master branch
     - Add missing datatype support in darray macros
     - Update comments on debug attributes
     - Remove stale envar settings in hwloc support
     - Silence warning of unused var
     - Minor addition to debug output
 - PR #2158: atomics: Fix broken make dist
 - PR #2154: Cherry-pick updates from master branch
     - Spawn needs to do a little checking of the app
     - build: Allow autogen.pl to be run from a tarball
     - Enable singletons to connect to system servers
     - Fix compile error in ptl_base_connect.c
     - Update ptl_base_connect.c
     - build: Change default build mode for components
     - build: Remove options around mca direct calling
     - Setup clients to output forwarded IO
     - Set the default for enable-mca-dso
     - Tool finalize crash due to ref count issue
     - Reject ambiguous connection options
     - ptl: prevent free of uninited suri variable
     - Clang-format the code
     - psec: include missing psec.h
     - ptl: help on too-many-conns: mention conn handle files
     - Some mods/cleanup of debugger definitions and handling
     - build: Explicitly list libpmix dependencies
     - build: Fix compiler attribute detection
     - build: Fix compiler family detection
     - atomics: Only support C11 and GCC builtin atomics
     - Clean out unneeded test directory
 - PR #2112: Add a bfrops 4.0.1 component
 - PR #2111: Cherry-pick updates from master branch
     - Add missing items
     - Add missing PMIx_Data... functions
     - Fix signature of new compression APIs
     - Add compress/decompress APIs
     - Update clang format
     - Slight touchups on event notification and name print
 - PR #2108: Cherry-pick updates from master branch
     - leak: Free items in nslist for fence tracker
     - Update src/common/pmix_iof.c
     - Properly handle stdin forwarding
     - Eliminate shadow variables
     - leak: Fix case where buffer was unloaded, losing the pointer
     - Leak: Always free ns->jobbkt in pmix_nspace_caddy_t destructor.
     - Add clang format support
 - PR #2105: Add zlib warning and compression checks
 - PR #2100: Cherry-pick updates from master branch
     - Correct listener - this is a PMIx v4 (not 4.1) server
     - Purge shadowing as reported by gcc.
     - Add -Wshadow to --picky-compiler
     - Avoid zero byte malloc in argv_join_range
     - Minor fix to libevent configury
     - Minor configure cleanups
     - Use LT_PATH_NM instead of AC_PATH_PROG to find nm
     - Update to Autoconf 2.7x
     - Enable singleton "comm_spawn" operations
     - Link against libz when testing for TM
     - Initialize myproc structure before calling PMIx_tool_set_server
     - Py: Open files as UTF-8 while processing
     - Fix Python binding build
     - Correctly pass the right object to dereg cbfunc
     - Extend check for empty buffer
     - Correct miscast of object type
     - Validate CFLAGS individually
     - Use the provided caddy instead of creating a new one
     - Silence -qinline xlc compiler warning
     - Fix case where var->mbv_enumerator can be released when static
     - Cleanup several places based on testing with PRRTE
     - Ensure proper handling of IOF pull requests
     - Protect against empty message
     - Remove PMIX_BUFFER datatype and extend macro definition
     - Fix stale definition for PMIX_HAVE_ATOMIC_LLSC_PTR
     - Add datatype support for new types
     - configury: fix _PMIX_CHECK_PACKAGE_LIB() macro
     - Missed a spot - check for HWLOC_VERSION defined
     - Protect HWLOC_VERSION
     - Fix configury where most compilers will get mislabeled as 'gnu'
     - Ensure the topology support matches the Standard
     - Fix check for IBM xl compilers for v13.1 and later
     - Cleanup the IOF register/dereg response code path
     - Fix clang compliler regression
     - Have developer builds use -O3 by default
     - Silence PGI atomics warnings, while not breaking clang
     - Update the new hybrid test a bit
     - Fix build failure on Apple silicon
     - Correctly handle precedence for first/last overall events
     - Fix various PGI warnings
     - Fix bugs in OFI configure and HWLOC component
     - Add the local reference ID to iof/pull request
     - Add a couple of useful macros
     - Correct name of PSM2 envar
     - configury: fix --with-ofi=DIR handling
     - Add the psm2 auth_key to the job info and silence warning
     - Update the pnet/opa component to current architecture
     - Ensure the data array always gets initialized in PMIX_INFO_LIST_CONVERT
     - Cleanup indirect debugger launch against mpirun
 - PR #2014: Silence a couple of Coverity warnings
 - PR #2013: Add the PMIx Standard version level to the version string
 - PR #2012: Fix Python binding build for VPATH
 - PR #2007: Disable IPv6 by default
 - PR #2000: Set hostname in global to NULL on finalize
 - PR #1998: Specify Python 3 for the configure check


4.0.0 -- 30 Dec 2020
--------------------
.. important:: This release implements the complete PMIx v4.0 Standard
               and therefore includes a number of new APIs and features. These
               are fully documented in the official document - the details of
               the revisions included in v4.0 are summarized here:
               https://pmix.github.io/uploads/2020/12/pmix-standard-4.0.pdf#page=549
               Note that this version of OpenPMIx includes a first-cut at the
               Python bindings described in Appendix A of the v4.0 Standard.

Beyond the v4.0 modifications and additions to APIs, datatypes, attributes,
and macros, changes to the library include:

 - Removal of the usock messaging component - only TCP is now supported
 - Removal of the PMI-1 and PMI-2 backward compatibility libraries into
   a new separate repository
 - Packaging changes to push the headers into ``*-devel`` packages
 - libtool patch for Mac BigSur OS
 - Fixed dependency issue with HWLOC to protect against stone-age versions
 - Changed man page format to Markdown, requires pandoc to generate from
   Git repository (but not from tarball)
 - Enable local fork/exec by tools when not connected to a server - this
   is done transparently
 - Support reproducible builds
 - Multiple bug fixes and memory leak repairs
 - Add support for network interface and GPU device distances
 - Allow retrieval of the caller's own rank and process ID via PMIx_Get
 - Provide full delineation of client, server, and tool attribute support
 - Add support for libev in lieu of libevent
 - Detect/avoid conflict with LSF version of "libevent"
 - Auto-detect and forward envars from various identified programming models
   (e.g., OpenMPI, OpenSHMEM) and fabrics
 - Change the default component build behavior to prefer building components
   as part of libpmix.so instead of individual DSOs.
