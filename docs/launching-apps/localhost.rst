Launching only on the local node
================================

It is common to develop MPI applications on a single workstation or
laptop, and then move to a larger parallel / HPC environment once the
MPI application is ready.

Open MPI supports running multi-process MPI jobs on a single machine.
In such cases, you can simply avoid listing a hostfile or remote
hosts, and simply list a number of MPI processes to launch.  For
example:

.. code-block:: sh

   shell$ mpirun -n 6 mpi-hello-world
   Hello world, I am 0 of 6 (running on my-laptop))
   Hello world, I am 1 of 6 (running on my-laptop)
   ...
   Hello world, I am 5 of 6 (running on my-laptop)

If you do not specify the ``-n`` option, ``mpirun`` will default to
launching as many MPI processes as there are processor cores (not
hyperthreads) on the machine.

MPI communication
-----------------

When running on a single machine, Open MPI will most likely use the
``ob1`` PML and the following BTLs for MPI communication between
peers:

* ``self``: used for sending and receiving loopback MPI messages
  |mdash| where the source and destination MPI process are the same.
* ``sm``: used for sending and receiving MPI messages where the source
  and destination MPI processes can share memory (e.g., via SYSV or
  POSIX shared memory mechanisms).

Shared memory MPI communication
-------------------------------

.. error:: TODO This should really be moved to the networking section.

The ``sm`` BTL supports two modes of shared memory communication:

#. **Two-copy:** Otherwise known as "copy-in / copy-out", this mode is
   where the sender copies data into shared memory and the receiver
   copies the data out.

   This mechanism is always available.

#. **Single copy:** In this mode, the sender or receiver makes a
   single copy of the message data from the source buffer in one
   process to the destination buffer in another process.  Open MPI
   supports three flavors of shared memory single-copy transfers:

   * `Linux KNEM <https://knem.gitlabpages.inria.fr/>`_.  This is a
     standalone Linux kernel module, made specifically for HPC and MPI
     libraries to enable high-performance single-copy message
     transfers.

     Open MPI must be able to find the KNEM header files in order to
     build support for KNEM.

   * `Linux XPMEM <https://github.com/hjelmn/xpmem>`_.  Similar to
     KNEM, this is a standalone Linux kernel module, made specifically
     for HPC and MPI libraries to enable high-performance single-copy
     message transfers.  It is derived from the Cray XPMEM system.

     Open MPI must be able to find the XPMEM header files in order to
     build support for XPMEM.

   * Linux Cross-Memory Attach (CMA).  This mechanism is built-in to
     modern versions of the Linux kernel.  Although more performance
     than the two-copy shared memory transfer mechanism, CMA is the
     lowest performance of the single-copy mechanisms.  However, CMA
     is likely the most widely available because it is enabled by
     default in several modern Linux distributions.

     Open MPI must be built on a Linux system with a recent enough
     Glibc and kernel version in order to build support for Linux CMA.

Which mechanism is used at run time depends both on how Open MPI was
built and how your system is configured.  You can check to see which
single-copy mechanisms Open MPI was built with via two mechanisms:

#. At the end of running ``configure``, Open MPI emits a list of
   transports for which it found relevant header files and libraries
   such that it will be able to build support for them.  You might see
   lines like this, for example:

   .. code-block:: text

      Shared memory/copy in+copy out: yes
      Shared memory/Linux CMA: yes
      Shared memory/Linux KNEM: no
      Shared memory/XPMEM: no

   The above output indicates that Open MPI will be built with 2-copy
   (as mentioned above, 2-copy is *always* available) and with Linux
   CMA support.  KNEM and XPMEM support will *not* be built.

#. After Open MPI is installed, the ``ompi_info`` command can show
   which ``smsc`` (shared memory single copy) components are
   available:

   .. code-block:: text

      shell$ ompi_info | grep smsc
                MCA smsc: cma (MCA v2.1.0, API v1.0.0, Component v5.1.0)

   This Open MPI installation only supports the Linux CMA single-copy
   mechanism.

.. note:: As implied by the SMSC component names, none of them are
   supported on macOS.  macOS users will use the two-copy mechanism.
