Shared Memory
=============


The sm BTL
----------

The ``sm`` BTL is a low-latency, high-bandwidth mechanism for
transferring data between two processes via shared memory.  This BTL
can only be used between processes executing on the same node.

.. note:: Between Open MPI version 1.8.0 and 4.1.x, the shared memory
          BTL was named ``vader``.  As of Open MPI version 5.0.0, the
          BTL has been renamed ``sm``.

.. warning:: In Open MPI version 5.0.x, the name ``vader`` is simply
             an alias for the ``sm`` BTL.  Similarly, all
             ``vader_``-prefixed MCA parameters are automatically
             aliased to their corresponding ``sm_``-prefixed MCA
             parameter.

             This alias mechanism is a legacy transition device, and
             will likely disappear in a future release of Open MPI.

/////////////////////////////////////////////////////////////////////////

Specifying the Use of sm for MPI Messages
-----------------------------------------

Typically, it is unnecessary to do so;  OMPI will use the best BTL available
for each communication.

Nevertheless, you may use the MCA parameter ``btl``.  You should also
specify the ``self`` BTL for communications between a process and
itself.  Furthermore, if not all processes in your job will run on the
same, single node, then you also need to specify a BTL for internode
communications.  For example:

.. code-block:: sh

   shell$ mpirun --mca btl self,sm,tcp -n 16 ./a.out

/////////////////////////////////////////////////////////////////////////

Tuning Parameters to Improve Performance
----------------------------------------

Mostly, the default values of the MCA parameters have already
been chosen to give good performance.  To improve performance further
is a little bit of an art.  Sometimes, it's a matter of trading off
performance for memory.

* ``btl_sm_eager_limit``: If message data plus header information fits
  within this limit, the message is sent "eagerly" |mdash| that is, a
  sender attempts to write its entire message to shared buffers
  without waiting for a receiver to be ready.  Above this size, a
  sender will only write the first part of a message, then wait for
  the receiver to acknowledge its readiness before continuing.  Eager
  sends *can* improve performance by decoupling senders from
  receivers.

* ``btl_sm_max_send_size``: Large messages are sent in fragments of
  this size.  Larger segments *can* lead to greater efficiencies,
  though they could perhaps also inhibit pipelining between sender and
  receiver.

* ``btl_sm_free_list_num``: This is the initial number of fragments on
  each (eager and max) free list.  The free lists can grow in response
  to resource congestion, but you can increase this parameter to
  pre-reserve space for more fragments.

* ``btl_sm_backing_directory``: Directory to place backing files for
  shared memory communication. This directory should be on a local
  filesystem such as ``/tmp`` or ``/dev/shm`` (default: (linux) ``/dev/shm``,
  (others) session directory)

/////////////////////////////////////////////////////////////////////////

Shared Memory Mechanisms
------------------------

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

/////////////////////////////////////////////////////////////////////////

Shared Memory Mapping on the Filesystem
---------------------------------------

The default location of the file is in the ``/dev/shm`` directory. If ``/dev/shm``
does not exist on the system, the default location will be the OMPI session
directory. The path is typically something like:
``/dev/shm/sm_segment.nodename.user_id.job_id.my_node_rank``.
For example, the full path could be: ``/dev/shm/sm_segment.x.1000.23c70000.0``.

You can use the MCA parameter ``btl_sm_backing_directory`` to place the
directory in a non-default location.

.. note:: The session directory can be customized via
          PRRTE using ``--prtemca prte_tmpdir_base /path/to/somewhere``.

.. note:: Even when using single-copy methods like CMA, a shared memory file is still
          created for managing connection metadata.
