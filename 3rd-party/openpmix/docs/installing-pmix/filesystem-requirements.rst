Filesystem requirements
=======================

.. _install-filesystem-timestamp-warning-label:

.. warning:: If you are building PMIx on a network filesystem, the
   machine you on which you are building *must* be time-synchronized
   with the file server.

Specifically: PMIx's build system *requires* accurate filesystem
timestamps.  If your ``make`` output shows that it ran GNU Automake,
Autoconf, and/or Libtool, or includes warning about timestamps in the
future, perhaps looking something like this::

   Warning: File `Makefile.am' has modification time 3.6e+04 s in the future

**Know that this is not normal**, and you likely have an invalid
build.

In this case, you should remove the PMIx source directory and
start over (e.g., by re-extracting the PMIx tarball): either
switch to build on a local filesystem, or ensure that the time on your
build machine is synchronized with the time on your file server before
building again.
