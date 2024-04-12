.. _platform-notes-section-label:

Platform Notes
==============

* Systems that have been tested are:

  * Linux (various flavors/distros), 64 bit (x86, ppc, aarch64),
    with gcc (>=4.8.x+), clang (>=3.6.0), Intel,
    and Portland (be sure to also see :ref:`the Compiler Notes
    section <compiler-notes-section-label>`)
  * macOS (10.14-10.15, 11.x, 12.x), 64 bit (x86_64 and M1) with XCode
    compilers

* Other systems have been lightly (but not fully) tested:

  * Cygwin 64 bit with gcc
  * ARMv6, ARMv7, ARMv9
  * Other 64 bit platforms.
  * OpenBSD.  Requires configure option``--disable-dlopen`` with this release.
  * Problems have been reported when building on FreeBSD 11.1
    using the clang-4.0 system compiler. A workaround is to build
    PMIx using the GNU compiler.

.. note:: 32-bit environments are no longer supported.
