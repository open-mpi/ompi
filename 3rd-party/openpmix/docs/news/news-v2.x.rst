PMIx v2.x series
================

This file contains all the NEWS updates for all the PMIx v2.x
series, in reverse chronological order.

2.2.4 -- 07 Jun 2020
--------------------
- PR #1466: Silence spurious error log
- PR #1489: Allow specification of hostname to use by client
- PR #1499/#1512/#1518/#1525/#1528/#1533/#1553: Fix internal tests
- PR #1678: Update "check_vendor" for PGI problem
- PR #1734: Fix resource leaks in ptl/usock component
- PR #1777: Fix crash of dstor locks destructor in ds12
- PR #1788: Remove unnecessary error logs


2.2.3 -- 15 Aug 2019
--------------------
- PR #1162: Fix race condition when clients fail while in a PMIx
  collective operation
- PR #1163: Fix a compiler warning in atomics on POWER arch
- PR #1165: Add BuildRequires: libevent-devel to spec file
- PR #1180: Remove dependency on C++ compiler for thread detection
- PR #1180: Add detection for Flex when building in non-tarball situations
- PR #1182: configury: use PMIX_ENABLE_DLOPEN_SUPPORT instead of enable_dlopen
- PR #1188: Link libpmix.so to MCA component libraries
- PR #1190: Ensure any cached notifications arrive after reg completes
- PR #1194: Ensure any cached notifications arrive after registration completes
- PR #1209: Update configure logic for clock_gettime
- PR #1213/#1217/#1221: Add configure option "--enable-nonglobal-dlopen"
  If the MCA component libraries should link back to libpmix.so
- PR #1231: SPEC: Allow splitting PMIx in pmix and pmix-libpmi packages
- PR #1253: Preserve user-provided CFLAGS to protect autotools
- PR #1267: Correct dmodex example
- PR #1275: IPv6 IF Read: Buffer Overflow
- PR #1295: Fix comment: IPv6 IF_NAMESIZE
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
- PR #1329: Add -fPIC to static builds
- PR #1334: Cache only -W CFLAG entries to fix 32-bit builds
- PR #1341: Do not use '==' in m4 test statements
- PR #1342: Fix if_linux_ipv6_open interface filter
- PR #1344: Remove unnecessary libtool init for c++
- PR #1346: Fix incorrect pointer casts/deref
- PR #1347/#1348: Fix use of gethostname
- PR #1353/#1357: util/environ: use setenv() if available
- PR #1354: Plug a misc memory leak in the pmix_query_caddy_t destructor
- PR #1356: Fix another pointer cast/deref in test suite
- PR #1358: Implement support for class-based info arrays
- PR #1359: Plug misc minor memory leaks
- PR #1369: Fix legacy support for PMI-1
- PR #1370: Cleanup handling of data requests for different nspaces
- PR #1193: Resolve get of proc-specific job-level info from another nspace
- PR #1377: Skip fastpath/dstore for NULL keys
- PR #1379: Change IF_NAMESIZE to PMIX_IF_NAMESIZE and set to safe size
- PR #1385: Check for EINVAL return from posix_fallocate
- PR #1389: Plug misc memory leaks in configure


2.2.2 -- 24 Jan 2019
--------------------
 - Fix a bug in macro identifying system events


2.2.1 -- 18 Jan 2019
--------------------
 - Fix a bug in registration of default event handlers
   that somehow slipped thru testing


2.2.0 -- 17 Jan 2019
--------------------
.. important:: THIS RELEASE MARKS THE STARTING POINT FOR FULL COMPLIANCE
               WITH THE PMIX v2.2 STANDARD. ALL API BEHAVIORS AND ATTRIBUTE
               DEFINITIONS MEET THE v2.2 STANDARD SPECIFICATIONS.

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
  remote servers.
- Automatically store the server URI for easy access by client
- Provide MCA parameter to control TCP connect retry/timeout
- Update event notification system to properly evict oldest events
  when more space is needed
- Fix a number of error paths
- Handle setsockopt(SO_RCVTIMEO) not being supported
- Pass the tool's command line to the server so it can be returned
  in queries
- Add support for C11 atomics
- Fix a bug when registering default event handlers


2.1.4 -- 18 Sep 2018
--------------------
- Updated configury to silence warnings on older compilers
- Implement job control and sensor APIs
- Update sensor support
- Fix a few bugs in the event notification system and provide some
  missing implementation (support for specifying target procs to
  receive the event).
- Add PMIX_PROC_TERMINATED constant
- Properly deal with EOPNOTSUPP from getsockopt() on ARM


2.1.3 -- 23 Aug 2018
--------------------
- Fixed memory corruption bug in event notification
  system due to uninitialized variable
- Add numeric version definition
- Transfer all cached data to client dstore upon first connect


2.1.2 -- 6 July 2018
--------------------
- Added PMIX_VERSION_RELEASE string to pmix_version.h
- Added PMIX_SPAWNED and PMIX_PARENT_ID keys to all procs
  started via PMIx_Spawn
- Fixed faulty compares in PMI/PMI2 tests
- Fixed bug in direct modex for data on remote node
- Correctly transfer all cached job info to the client's
  shared memory region upon first connection
- Fix potential deadlock in PMIx_server_init in an error case
- Fix uninitialized variable
- Fix several memory and file descriptor leaks


2.1.1 -- 23 Feb 2018
--------------------
- Fix direct modex when receiving new nspace
- Resolve direct modex of job-level info
- Fix a bug in attribute configuration checks
- Fix a couple of bugs in unpacking of direct modex job-level data
- Correcly handle application setup data during "instant on" launch
- add a PMIX_BYTE_OBJECT_LOAD convenience macro
- Fix two early "free" bugs
- Add an example PMI-1 client program


2.1.0 -- 1 Feb 2018
-------------------
.. important:: This release contains the first implementation of cross-version
               support. Servers using v2.1.0 are capable of supporting clients using
               PMIx versions v1.2 and above. Clients using v2.1.0 are able to interact
               with servers based on v1.2 and above.

- Added cross-version communication support
- Enable reporting of contact URI to stdout, stderr, or file (PR #538)
- Enable support for remote tool connections (PR #540, #542)
- Cleanup libevent configure logi to support default install paths (PR #541)
- Debounce "unreachable" notifications for tools when they disconnect (PR #544)
- Enable the regex generator to support node names that include multiple
  sets of numbers


2.0.3 -- 1 Feb 2018
-------------------
- Fix event notification so all sides of multi-library get notified
  of other library's existence
- Update syslog protection to support Mac High Sierra OS
- Remove usock component - unable to support v1.x clients due
  to datatype differences
- Cleanup security handshake
- Cleanup separation of PMI-1/2 libraries and PMIx symbols
- Protect against overly-large messages
- Update data buffer APIs to support cross-version operations
- Protect receive callbacks from NULL and/or empty buffers as this
  can occur when the peer on a connection disappears.
- Fix tool connection search so it properly descends into the directory
  tree while searching for the server's contact file.
- Fix store_local so it doesn't reject a new nspace as that can happen
  when working with tools
- Ensure we always complete PMIx_Finalize - don't return if something
  goes wrong in the middle of the procedure
- Fix several tool connection issues


2.0.2 -- 19 Oct 2017
--------------------
- Update RPM spec file (rpmbuild -ta, and --rebuild fixes) (PR #523)
- Support singletons in PMI-1/PMI-2 (PR #537)
- Provide missing implementation support for arrays of pmix_value_t's (PR #531)
- Remove unsupported assembly code for MIPS and ARM processors
  prior to v6 (PR #547)
- Fix path separator for PMIx configuration files (PR #547)
- Add configure option to enable/disable the default value for the
  show-load-errors MCA param (PR #547)


2.0.1 -- 24 Aug. 2017
---------------------
- Protect PMIX_INFO_FREE macro from NULL data arrays
- Added attributes to support HWLOC shared memory regions
- Fixed several syntax errors in configure code
- Fixed several visibility errors
- Correctly return status from PMIx_Fence operation
- Restore tool connection support and implement search
  operations to discover rendezvous files


2.0.0 -- 22 Jun 2017
--------------------
.. important:: This release implements the complete PMIX v2.0 Standard
               and therefore includes a number of new APIs and features. These
               can be tracked by their RFC's in the RFC repository at:
               https://github.com/pmix/RFCs. A formal standards document will
               be included in a later v2.x release. Some of the changes are
               identified below.

- Added the Modular Component Architecture (MCA) plugin manager and
  converted a number of operations to plugins, thereby allowing easy
  customization and extension (including proprietary offerings)
- Added support for TCP sockets instead of Unix domain sockets for
  client-server communications
- Added support for on-the-fly Allocation requests, including requests
  for additional resources, extension of time for currently allocated
  resources, and return of identified allocated resources to the scheduler
  (RFC 0005 - https://github.com/pmix/RFCs/blob/master/RFC0005.md)
- Tightened rules on the processing of PMIx_Get requests, including
  reservation of the "pmix" prefix for attribute keys and specifying
  behaviors associated with the PMIX_RANK_WILDCARD value
  (RFC 0009 - https://github.com/pmix/RFCs/blob/master/RFC0009.md)
- Extended support for tool interactions with a PMIx server aimed at
  meeting the needs of debuggers and other tools. Includes support
  for rendezvousing with a system-level PMIx server for interacting
  with the system management stack (SMS) outside of an allocated
  session, and adds two new APIs:
- PMIx_Query: request general information such as the process
  table for a specified job, and available SMS capabilities
- PMIx_Log: log messages (e.g., application progress) to a
  system-hosted persistent store
  (RFC 0010 - https://github.com/pmix/RFCs/blob/master/RFC0010.md)
- Added support for fabric/network interactions associated with
  "instant on" application startup
  (RFC 0012 - https://github.com/pmix/RFCs/blob/master/RFC0012.md)
- Added an attribute to support getting the time remaining in an
  allocation via the PMIx_Query interface
  (RFC 0013 - https://github.com/pmix/RFCs/blob/master/RFC0013.md)
- Added interfaces to support job control and monitoring requests,
  including heartbeat and file monitors to detect stalled applications.
  Job control interface supports standard signal-related operations
  (pause, kill, resume, etc.) as well as checkpoint/restart requests.
  The interface can also be used by an application to indicate it is
  willing to be pre-empted, with the host RM providing an event
  notification when the preemption is desired.
  (RFC 0015 - https://github.com/pmix/RFCs/blob/master/RFC0015.md)
- Extended the event notification system to support notifications
  across threads in the same process, and the ability to direct
  ordering of notifications when registering event handlers.
  (RFC 0018 - https://github.com/pmix/RFCs/blob/master/RFC0018.md)
- Expose the buffer manipulation functions via a new set of APIs
  to support heterogeneous data transfers within the host RM
  environment
  (RFC 0020 - https://github.com/pmix/RFCs/blob/master/RFC0020.md)
- Fix a number of race condition issues that arose at scale
- Enable PMIx servers to generate notifications to the host RM
  and to themselves
