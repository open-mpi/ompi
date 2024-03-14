.. _label-frameworks:

Internal frameworks
===================

The Modular Component Architecture (MCA) is the backbone of Open MPI
-- most services and functionality are implemented through MCA
components.

MPI layer frameworks
--------------------

Here is a list of all the component frameworks in the MPI layer of
Open MPI:

* ``bml``: BTL management layer
* ``coll``: MPI collective algorithms
* ``fbtl``: point to point file byte transfer layer: abstraction for
  individual read: collective read and write operations for MPI I/O
* ``fcoll``: collective file system functions for MPI I/O
* ``fs``: file system functions for MPI I/O
* ``hook``: Generic hooks into Open MPI
* ``io``: MPI I/O
* ``mtl``: Matching transport layer, used for MPI point-to-point
  messages on some types of networks
* ``op``: Back end computations for intrinsic MPI_Op operators
* ``osc``: MPI one-sided communications
* ``pml``: MPI point-to-point management layer
* ``sharedfp``: shared file pointer operations for MPI I/O
* ``topo``: MPI topology routines
* ``vprotocol``: Protocols for the "v" PML

OpenSHMEM component frameworks
------------------------------

* ``atomic``: OpenSHMEM atomic operations
* ``memheap``: OpenSHMEM memory allocators that support the
  PGAS memory model
* ``scoll``: OpenSHMEM collective operations
* ``spml``: OpenSHMEM "pml-like" layer: supports one-sided,
  point-to-point operations
* ``sshmem``: OpenSHMEM shared memory backing facility

Miscellaneous frameworks
------------------------

* ``allocator``: Memory allocator
* ``backtrace``: Debugging call stack backtrace support
* ``btl``: Point-to-point Byte Transfer Layer
* ``dl``: Dynamic loading library interface
* ``hwloc``: Hardware locality (hwloc) versioning support
* ``if``: OS IP interface support
* ``installdirs``: Installation directory relocation services
* ``memchecker``: Run-time memory checking
* ``memcpy``: Memory copy support
* ``memory``: Memory management hooks
* ``mpool``: Memory pooling
* ``patcher``: Symbol patcher hooks
* ``pmix``: Process management interface (exascale)
* ``rcache``: Memory registration cache
* ``reachable``: Network reachability determination
* ``shmem``: Shared memory support (NOT related to OpenSHMEM)
* ``smsc``: Shared memory single-copy support
* ``threads``: OS and userspace thread support
* ``timer``: High-resolution timers

Framework notes
---------------

Each framework typically has one or more components that are used at
run-time.  For example, the ``btl`` framework is used by the MPI layer
to send bytes across different types underlying networks.  The ``tcp``
``btl``, for example, sends messages across TCP-based networks; the
``ucx`` ``pml`` sends messages across InfiniBand-based networks.

MCA parameter notes
-------------------

Each component typically has some tunable parameters that can be
changed at run-time.  Use the :ref:`ompi_info(1) <man1-ompi_info>`
command to check a component to see what its tunable parameters are.
For example:

.. code-block:: sh

   shell$ ompi_info --param btl tcp

shows some of the parameters (and default values) for the ``tcp`` ``btl``
component (use ``--all`` or ``--level 9`` to show *all* the parameters).

Note that ``ompi_info`` (without ``--all`` or a specified level) only
shows a small number a component's MCA parameters by default.  Each
MCA parameter has a "level" value from 1 to 9, corresponding to the
MPI-3 MPI_T tool interface levels.  :ref:`See the LEVELS section in
the ompi_info(1) man page <man1-ompi_info-levels>` for an explanation
of the levels and how they correspond to Open MPI's code.

Here's rules of thumb to keep in mind when using Open MPI's levels:

* Levels 1-3:

  * These levels should contain only a few MCA parameters.
  * Generally, only put MCA parameters in these levels that matter to
    users who just need to *run* Open MPI applications (and don't
    know/care anything about MPI).  Examples (these are not
    comprehensive):

    * Selection of which network interfaces to use.
    * Selection of which MCA components to use.
    * Selective disabling of warning messages (e.g., show warning
      message XYZ unless a specific MCA parameter is set, which
      disables showing that warning message).
    * Enabling additional stderr logging verbosity.  This allows a
      user to run with this logging enabled, and then use that output
      to get technical assistance.

* Levels 4-6:

  * These levels should contain any other MCA parameters that are
    useful to expose to end users.
  * There is an expectation that "power users" will utilize these MCA
    parameters |mdash| e.g., those who are trying to tune the system
    and extract more performance.
  * Here's some examples of MCA parameters suitable for these levels
    (these are not comprehensive):

    * When you could have hard-coded a constant size of a resource
      (e.g., a resource pool size or buffer length), make it an MCA
      parameter instead.
    * When there are multiple different algorithms available for a
      particular operation, code them all up and provide an MCA
      parameter to let the user select between them.

* Levels 7-9:

  * Put any other MCA parameters here.
  * It's ok for these MCA parameters to be esoteric and only relevant
    to deep magic / the internals of Open MPI.
  * There is little expectation of users using these MCA parameters.

See :ref:`this section <label-running-setting-mca-param-values>` for
details on how to set MCA parameters at run time.
