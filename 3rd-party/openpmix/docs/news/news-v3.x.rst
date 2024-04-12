PMIx v3.x series
================

This file contains all the NEWS updates for the PMIx v3.x
series, in reverse chronological order.

3.2.2 -- 7 Dec 2020
-------------------
 - PR #1930: Remove man page setup as there are no manpages in v3.2
 - PR #1933: Remove stale config command
 - PR #1940: Fix dependency issue with hwloc
 - PR #1941: .spec: add ``*-devel`` packages


3.2.1 -- 12 Nov 2020
--------------------
 - PR #1890:
   - Fix Issue #1889: Fix symlinks in unit tests to include new timeout
   - Fix Issue #1891: Remove pnet/opa component that should not be in v3.2
 - PR #1904: Add more metadata to string generated from preg/compress
 - PR #1919: Fix memory leak in PMIx_Get/fastpath


3.2.0 -- 22 Oct 2020
--------------------
 - PR #1402/#1403/#1421/#1423: Modex size reductions
 - PR #1752: Convert man pages to Markdown (pandoc)
 - PR #1766: Move from -levent to -levent_core for linking Libevent
 - PR #1832: Sync 3.2 branch with master
   - New attributes:

     - PMIX_HOSTNAME_ALIASES
     - PMIX_HOSTNAME_KEEP_FQDN
     - PMIX_GET_REFRESH_CACHE
     - PMIX_REQUIRED_KEY

   - Removed PMIX_VALUE_COMPRESSED_STRING_UNPACK macro from pmix_common.h
   - New frameworks
     - PR #1139: pcompress - Compression methods
     - PR #1423: psquash - Flexible integer packing
   - New components
     - PR #1139: preg/compress - regular expression methods
   - PR #1422 Modified string representations produced by PMIx_generate_regex
     and PMIx_generate_ppn.
   - Issue #1586: Fixed dmodex support and PMIx_Get behavior
   - PR #1748: Removed --with-pmix-symbol-rename configure option
 - PR #1848: Expose PMIX_REGEX constant per v3.2 standard
 - PR #1885: Fix immediate flag behavior at the server


3.1.5 -- 14 Feb 2020
--------------------
.. important:: The signature of the PMIx_Allocation_request has changed
               in accordance with an Errata update of the PMIx v3 Standard

- PR #1413/#1465: Remove unnecessary error log
- PR #1433: Return the correct status from PMIx_Publish
- PR #1445: Sync. with master to fix 'get' of data for unknown namespace
  Includes the following PRs from master
  - PR #1382: dstore: fixed truncate key-names while restoring
  - PR #1405: Fix xnspace dmodex and add verbose debug
  - PR #1406: Resolve request for job-level data
  - PR #1407/#1409/#1411: Fix dmodex across nspaces
  - PR #1434/#1436: Cleanup handling of info arrays
  - PR #1435: Cleanup example and remove debug
  - PR #1437: Update blocking Allocation_request signature
  - PR #1440: Fix 'get' of data for unknown namespace
  - PR #1442: Fix fences with namespaces where no local processes are running
- PR #1472: Initialize nlocal and local in the tracker
- PR #1487: Sync. with master to fix info array and cross-version issues
- PR #1493/#1497/#1501/#1505/#1589: Info array and Cross-version fixes
- PR #1511/#1517/#1520/#1523/#1534/#1565: Fix pmix tests
- PR #1530: Improve handling of servers piror to v3.1.5
- PR #1531: Update transfer from hash to dstore
- PR #1538: Fix singleton initialization
- PR #1547: Add missing PMIx_IOF_deregister function
- PR #1554/#1591: Fix memory leak on namespace deregister
- PR #1561: Configury fix for reproducible builds
- PR #1579: Protect pthread setpshared calls
- PR #1587: Fix to gds/dstore configure logic
- PR #1610: Adjust hotel timeout to be in whole seconds
- PR #1613: dstore: Fix cache size calculation
- PR #1622: Fix multiple occurrences of unaligned access in pmix tests
- PR #1620: Re-address the collective tracker problem


3.1.4 -- 9 Aug 2019
-------------------
- PR #1342: Fix if_linux_ipv6_open interface filter
- PR #1344: Remove unnecessary libtool init for c++
- PR #1346: Fix incorrect pointer casts/deref
- PR #1347/#1348: Fix use of gethostname
- PR #1353/#1357: util/environ: use setenv() if available
- PR #1354: Plug a misc memory leak in the pmix_query_caddy_t destructor
- PR #1356: Fix another pointer cast/deref in test suite
- PR #1358: Implement support for class-based info arrays
- PR #1359: Plug misc minor memory leaks
- PR #1368: Backport support for libev
- PR #1369: Fix legacy support for PMI-1
- PR #1370: Cleanup handling of data requests for different nspaces
- PR #1193: Resolve get of proc-specific job-level info from another nspace
- PR #1376: Fix problems in the Log code path, updates to simple test suite
- PR #1377: Skip fastpath/dstore for NULL keys
- PR #1379: Change IF_NAMESIZE to PMIX_IF_NAMESIZE and set to safe size
- PR #1385: Check for EINVAL return from posix_fallocate
- PR #1389: Plug misc memory leaks in configure


3.1.3 -- 2 July 2019
--------------------
- PR #1096: Restore PMIX_NUM_SLOTS for backward compatibility
- PR #1106: Automatically generate PMIX_NUMERIC_VERSION
- PR #1143: Fix tool connection handshake for tools that are registered
  clients
- PR #1163: Fix a compiler warning in atomics on POWER arch
- PR #1162: Fix race condition when clients fail while in a PMIx
  collective operation
- PR #1166: Fix a regression in spinlock atomics
- PR #1159: Fix missing pointer update when shared memory segment
  was re-attached
- PR #1180: Remove dependency on C++ compiler for thread detection
- PR #1180: Add detection for Flex when building in non-tarball situations
- PR #1165: Add dependency on libevent-devel to rpm spec file
- PR #1188: Link libpmix.so to MCA component libraries
- PR #1194: Ensure any cached notifications arrive after registration completes
- PR #1205: Add "make check" support
- PR #1209: Update configure logic for clock_gettime
- PR #1213/#1217/#1221: Add configure option "--enable-nonglobal-dlopen"
  If the MCA component libraries should link back to libpmix.so
- PR #1231: SPEC: Allow splitting PMIx in pmix and pmix-libpmi packages
- PR #1222: Fix case of multiple launcher calls in job script
- PR #1237: Avoid double-free of collective tracker
- PR #1237: Ensure all participants are notified of fence complete
- PR #1237: Ensure all participants are notified of connect and disconnect complete
- PR #1250: Fix PMIx_server_finalize hang (rare)
- PR #1271: PTL/usock doesn't support tools
- PR #1280: Fix the PTL connection establishment protocol
- PR #1280: Fix tool connection in psec/handshake mode
- PR #1289: Avoid output_verbose overhead when it won't print
- PR #1296: Allow setup_fork to proceed even if gdds and pnet don't contribute
- PR #1296: Allow servers to pass NULL module
- PR #1297: Provide internal blocking ability to the register/deregister fns
- PR #1298: Add dummy handshake component to psec framework for testing
- PR #1303: Allow jobs to not specify proc-level info
- PR #1304: Provide proc data in cases where host does not
- PR #1305: Add some more values that can be computed
- PR #1308: Add missing tool rendezvous file
- PR #1309: Fix potential integer overflow in regex
- PR #1311: Work around memory bug in older gcc compilers
- PR #1321: Provide memory op hooks in user-facing macros
- PR #1329: Add -fPIC to static builds
- PR #1340: Do not use '==' in m4 test statements


3.1.2 -- 24 Jan 2019
--------------------
 - Fix a bug in macro identifying system events
 - Restore some non-standard macros to the pmix_extend.h
   header - these are considered "deprecated" and will be
   removed from public-facing headers in future releases


3.1.1 -- 18 Jan 2019
--------------------
- Fix a bug in registration of default event handlers
  that somehow slipped thru testing


3.1.0 -- 17 Jan 2019
--------------------
.. important:: THIS RELEASE MARKS THE STARTING POINT FOR FULL COMPLIANCE
               WITH THE PMIX v3 STANDARD. ALL API BEHAVIORS AND ATTRIBUTE
               DEFINITIONS MEET THE v3 STANDARD SPECIFICATIONS.

- Add a new, faster dstore GDS component 'ds21'
- Performance optimizations for the dstore GDS components.
- Plug miscellaneous memory leaks
- Silence an unnecessary warning message when checking connection
  to a non-supporting server
- Ensure lost-connection events get delivered to default event
  handlers
- Correctly handle cache refresh for queries
- Protect against race conditions between host and internal library
  when dealing with async requests
- Cleanup tool operations and add support for connections to
  remote servers. Initial support for debugger direct/indirect
  launch verified with PRRTE. Cleanup setting of tmpdir options.
  Drop rendezvous files when acting as a launcher
- Automatically store the server URI for easy access by client
- Provide MCA parameter to control TCP connect retry/timeout
- Update event notification system to properly evict oldest events
  when more space is needed
- Fix a number of error paths
- Update IOF cache code to properly drop oldest message. Provide
  MCA parameter for setting cache size.
- Handle setsockopt(SO_RCVTIMEO) not being supported
- Ensure that epilogs get run even when connections unexpectedly
  terminate. Properly split epilog strings to process multiple
  paths
- Pass the tool's command line to the server so it can be returned
  in queries
- Add support for C11 atomics
- Support collection and forwarding of fabric-specific envars
- Improve handling of hwloc configure option
- Fix PMIx_server_generate_regex to preserve node ordering
- Fix a bug when registering default event handlers


3.0.2 -- 18 Sept 2018
---------------------
- Ensure we cleanup any active sensors when a peer departs. Allow the
  heartbeat monitor to "reset" if a process stops beating and subsequently
  returns
- Fix a few bugs in the event notification system and provide some
  missing implementation (support for specifying target procs to
  receive the event).
- Add PMIX_PROC_TERMINATED constant
- Properly deal with EOPNOTSUPP from getsockopt() on ARM


3.0.1 -- 23 Aug 2018
--------------------
.. warning:: DEPRECATION WARNING The pmix_info_array_t struct was
             initially marked for deprecation in the v2.x series.
             We failed to provide clear warning at that time. This
             therefore serves as warning of intended removal of
             pmix_info_array_t in the future v4 release series.

- Fixed memory corruption bug in event notification
  system due to uninitialized variable
- Add numeric version field to pmix_version.h
- Transfer all cached data to client dstore upon first connect
- Implement missing job control and sensor APIs


3.0.0 -- 6 July 2018
--------------------
.. important:: This release implements the complete PMIX v3.0 Standard
               and therefore includes a number of new APIs and features. These
               can be tracked by their RFC's on the community website:
               https://pmix.org/pmix-standard.

- Added blocking forms of several existing APIs:
    - PMIx_Log
    - PMIx_Allocation_request
    - PMIx_Job_control
    - PMIx_Process_monitor
- Added support for getting/validating security credentials
    - PMIx_Get_credential, PMIx_Validate_credential
- Extended support for debuggers/tools
    - Added IO forwarding support allowing tools to request
      forwarding of output from specific application procs,
      and to forward their input to specified target procs
    - Extended tool attributes to support synchronization
      during startup of applications. This includes the
      ability to modify an application's environment
      (including support for LD_PRELOAD) and define an
      alternate fork/exec agent
    - Added ability for a tool to switch server connections
      so it can first connect to a system-level server to
      launch a starter program, and then reconnect to that
      starter for debugging purposes
- Extended network support to collect network inventory by
     either rolling it up from individual nodes or by direct
     query of fabric managers. Added an API by which the
     host can inject any rolled up inventory into the local
     PMIx server. Applications and/or the host RM can access
     the inventory via the PMIx_Query function.
- Added the ability for applications and/or tools to register
     files and directories for cleanup upon their termination
- Added support for inter-library coordination within a process
- Extended PMIx_Log support by adding plugin support for new
     channels, including local/remote syslog and email. Added
     attributes to query available channels and to tag and
     format output.
- Fix several memory and file descriptor leaks
