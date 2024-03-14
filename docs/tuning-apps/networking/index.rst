Networking support
==================

Open MPI supports a variety of different networking transports for
off-node communication.  Not all transports are supported or available
on every platform.  Many require specialized hardware, operating
system drivers, and/or userspace network transport libraries.

When Open MPI is being configured, it will search for a variety of
network transport libraries (and corresponding development header
files).  By default, if ``configure`` can find a network transport
library and its development header files, it will include support for
that library.  If ``configure`` does not find a library or its header
files, it will simply skip that library (and Open MPI will simply not
build support for that library).

.. note:: The sections listed below are by no means comprehensive.

.. toctree::
   :maxdepth: 1

   ofi
   tcp
   shared-memory
   ib-and-roce
   iwarp
   cuda
   rocm
