PMIx v1.x series
====================

This file contains all the NEWS updates for all the PMIx v1.x
series, in reverse chronological order.

1.2.5 -- 6 Feb 2018
----------------------
- Fix cross-version issue when v1.2 client interacts with v2.1 server (PR #564)
- Update client connection for cross-version support (PR #591)
- Fix write memory barrier ASM for PowerPC (PR #606)
- Add protection from overly-large messages


1.2.4 -- 13 Oct. 2017
----------------------
- Silence some unnecessary warning messages (PR #487)
- Coverity fix - TOCTOU (PR #465)
- automake 1.13 configure fix (PR #486)
- Update RPM spec file (rpmbuild -ta, and --rebuild fixes) (PR #523)
- Support singletons in PMI-1/PMI-2 (PR #537)


1.2.3 -- 24 Aug. 2017
----------------------
- Resolve visibility issues for public APIs (PR #451)
- Atomics update - remove custom ASM atomics (PR #458)
- Fix job-fence test (PR #423)
- Replace stale PMIX_DECLSPEC with PMIX_EXPORT (PR #448)
- Memory barrier fixes for thread shifting (PR #387)
- Fix race condition in dmodex (PR #346)
- Allow disable backward compatability for PMI-1/2 (PR #350)
- Fix segv in PMIx_server_deregister_nspace (PR #343)
- Fix possible hang in PMIx_Abort (PR #339)


1.2.2 -- 21 March 2017
----------------------
- Compiler fix for Sun/Oracle CC (PR #322)
- Fix missing include (PR #326)
- Improve error checking around posix_fallocate (PR #329)
- Fix possible memory corruption (PR #331)


1.2.1 -- 21 Feb. 2017
----------------------
- dstore: Fix data corruption bug in key overwrite cases
- dstore: Performance and scalability fixes
- sm: Use posix_fallocate() before mmap
- pmi1/pmi2: Restore support
- dstore: Fix extension slot size allocation (Issue #280)


1.2.0 -- 14 Dec. 2016
----------------------
- Add shared memory data storage (dstore) option. Default: enabled
  Configure option: --disable-dstore
- PMIx_Commit performance improvements
- Disable errhandler support
- Keep job info in the shared memory dstore
- PMIx_Get performance and memory improvements

1.1.5
-----
- Add pmix_version.h to support direct detection of PMIx library version
- Fix support for Solaris 10 by using abstract version of strnlen
- Fix native security module for Solaris by using getpeerucred in
  that environment
- Ensure man pages don't get installed in embedded builds
- Pass temporary directory locations in info keys instead of
  the environment

1.1.4
-----
- Properly increment the reference count for PMIx_Init
- Fix examples so all run properly
- Fix/complete PMI2 backward compatibility support to handle
  keys that are not associated with a specific rank
- Do a better job of hiding non-API symbols
- Correct handling of semi-colon terminations on macros.
  Thanks to Ashley Pittman for the patch
- Add more man pages
- Improve error checking and messages for connection
  attempts from client to server
- If the tmpdir name is too long, provide an appropriate
  help message to the user (particularly relevant on
  Mac OSX). Thanks to Rainer Keller for the patch.
- Fix some C++ compatibility issues
- Fix/complete PMI-1 backward compatibility support
- Do not install internal headers unless specifically
  requested to do so
- Add support for multiple calls to Put/Commit


1.1.3
-----
- Update the symbol hiding file to cover all symbols
- Fix examples and test directory Makefile.am's so
  the Makefiles are automatically built and the
  code compiled, but not installed
- Do not install the pmix library in embedded use-cases


1.1.2
-----
- Provide a check for hwloc support - if not found, then
  don't pass any topology info down to the client as it
  won't know how to unpack it anyway.
- Fix a few places where thread safety wasn't provided
- Fix several issues identified by Paul Hargrove:
    * PMIx_Init(NULL) is supported
    * Incomplete PMIx_constants man page had some lingering cruft
    * Missing prototype for pmix_value_load
- Fix race condition in PMIx_Get/PMIx_Get_nb
- Fix double-free error in pmix_server_commit.
- Fix PMIX_LOAD_BUFFER to be safe.


1.1.1
-----
- Fix an issue where the example and test programs
  were incorrectly being installed. Thanks to Orion
  Poplawski for reporting it


1.1.0
-----
- major update of APIs to reflect comments received from 1.0.0
  non-production release
- fixed thread-safety issues
- fixed a range of pack/unpack issues
- added unit tests for all APIs


1.0.0
------
Initial public release of draft APIs for comment - not production
intended
