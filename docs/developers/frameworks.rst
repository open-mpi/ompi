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

Each component typically has some tunable parameters that can be
changed at run-time.  Use the ``ompi_info`` command to check a component
to see what its tunable parameters are.  For example:

.. code-block:: sh

   shell$ ompi_info --param btl tcp

shows some of the parameters (and default values) for the ``tcp`` ``btl``
component (use ``--level`` to show *all* the parameters; see below).

Note that ``ompi_info`` only shows a small number a component's MCA
parameters by default.  Each MCA parameter has a "level" value from 1
to 9, corresponding to the MPI-3 MPI_T tool interface levels.  In Open
MPI, we have interpreted these nine levels as three groups of three:

#. End user / basic
#. End user / detailed
#. End user / all
#. Application tuner / basic
#. Application tuner / detailed
#. Application tuner / all
#. MPI/OpenSHMEM developer / basic
#. MPI/OpenSHMEM developer / detailed
#. MPI/OpenSHMEM developer / all

Here's how the three sub-groups are defined:

#. End user: Generally, these are parameters that are required for
   correctness, meaning that someone may need to set these just to
   get their MPI/OpenSHMEM application to run correctly.
#. Application tuner: Generally, these are parameters that can be
   used to tweak MPI application performance.
#. MPI/OpenSHMEM developer: Parameters that either don't fit in the
   other two, or are specifically intended for debugging /
   development of Open MPI itself.

Each sub-group is broken down into three classifications:

#. Basic: For parameters that everyone in this category will want to
   see.
#. Detailed: Parameters that are useful, but you probably won't need
   to change them often.
#. All: All other parameters -- probably including some fairly
   esoteric parameters.

To see *all* available parameters for a given component, specify that
ompi_info should use level 9:

.. code-block:: sh

   shell$ ompi_info --param btl tcp --level 9

.. error:: TODO The following content seems redundant with the FAQ.
   Additionally, information about how to set MCA params should be
   prominently documented somewhere that is easy for users to find --
   not buried here in the developer's section.

These values can be overridden at run-time in several ways.  At
run-time, the following locations are examined (in order) for new
values of parameters:

#. ``PREFIX/etc/openmpi-mca-params.conf``:
   This file is intended to set any system-wide default MCA parameter
   values -- it will apply, by default, to all users who use this Open
   MPI installation.  The default file that is installed contains many
   comments explaining its format.

#. ``$HOME/.openmpi/mca-params.conf``:
   If this file exists, it should be in the same format as
   ``PREFIX/etc/openmpi-mca-params.conf``.  It is intended to provide
   per-user default parameter values.

#. environment variables of the form ``OMPI_MCA_<name>`` set equal to a
   ``VALUE``:

   Where ``<name>`` is the name of the parameter.  For example, set the
   variable named ``OMPI_MCA_btl_tcp_frag_size`` to the value 65536
   (Bourne-style shells):

   .. code-block:: sh

      shell$ OMPI_MCA_btl_tcp_frag_size=65536
      shell$ export OMPI_MCA_btl_tcp_frag_size

   .. error:: TODO Do we need content here about PMIx and PRTE env vars?

#. the ``mpirun``/``oshrun`` command line: ``--mca NAME VALUE``

   Where ``<name>`` is the name of the parameter.  For example:

   .. code-block:: sh

      shell$ mpirun --mca btl_tcp_frag_size 65536 -n 2 hello_world_mpi

   .. error:: TODO Do we need content here about PMIx and PRTE MCA vars
              and corresponding command line switches?

These locations are checked in order.  For example, a parameter value
passed on the ``mpirun`` command line will override an environment
variable; an environment variable will override the system-wide
defaults.

Each component typically activates itself when relevant.  For example,
the usNIC component will detect that usNIC devices are present and
will automatically be used for MPI communications.  The Slurm
component will automatically detect when running inside a Slurm job
and activate itself.  And so on.

Components can be manually activated or deactivated if necessary, of
course.  The most common components that are manually activated,
deactivated, or tuned are the ``btl`` components -- components that are
used for MPI point-to-point communications on many types common
networks.

For example, to *only* activate the ``tcp`` and ``self`` (process loopback)
components are used for MPI communications, specify them in a
comma-delimited list to the ``btl`` MCA parameter:

.. code-block:: sh

   shell$ mpirun --mca btl tcp,self hello_world_mpi

To add shared memory support, add ``sm`` into the command-delimited list
(list order does not matter):

.. code-block:: sh

   shell$ mpirun --mca btl tcp,sm,self hello_world_mpi

.. note:: There used to be a ``vader`` ``btl`` component for shared
          memory support; it was renamed to ``sm`` in Open MPI v5.0.0,
          but the alias ``vader`` still works as well.

To specifically deactivate a specific component, the comma-delimited
list can be prepended with a ``^`` to negate it:

.. code-block:: sh

   shell$ mpirun --mca btl ^tcp hello_mpi_world

The above command will use any other ``btl`` component other than the
``tcp`` component.
