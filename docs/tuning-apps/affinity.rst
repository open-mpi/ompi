Processor and memory affinity
=============================

.. _tuning-using-paffinity-label:

Processor affinity
------------------

Open MPI supports processor affinity on a variety of systems through
process binding, in which each MPI process, along with its threads, is
"bound" to a specific subset of processing resources (cores, packages,
etc.).  That is, the operating system will constrain that process to
run on only that subset.

.. note:: The operating system may allow other processes to run on the
          same resources.

Affinity can improve performance by inhibiting excessive process
movement |mdash| for example, away from "hot" caches or NUMA memory.
Judicious bindings can improve performance by reducing resource
contention (by spreading processes apart from one another) or
improving interprocess communications (by placing processes close to
one another).  Binding can also improve performance reproducibility by
eliminating variable process placement.

.. warning:: Processor affinity probably should *not* be used when a
             node is over-subscribed (i.e., more processes are
             launched than there are processors).

             This can lead to a serious degradation in performance
             (even more than simply oversubscribing the node).  Open
             MPI will usually detect this situation and automatically
             disable the use of processor affinity (and display
             run-time warnings to this effect).

Memory affinity
---------------

Memory affinity is critically important on modern servers
because most architectures exhibit Non-Uniform Memory Access (NUMA)
architectures.  In a NUMA architecture, memory is physically
distributed throughout the machine even though it is virtually treated
as a single address space.  That is, memory may be physically local to
one or more processors |mdash| and therefore remote to other processors.

Simply put: some memory will be faster to access (for a given process)
than others.

Open MPI supports general and specific memory affinity, meaning that
it generally tries to allocate all memory local to the processor that
asked for it.  When shared memory is used for communication, Open MPI
uses memory affinity to make certain pages local to specific
processes in order to minimize memory network/bus traffic.

Open MPI supports memory affinity on a variety of systems, and is
controlled through the `Hardware Locality (hwloc)
<https://www.open-mpi.org/projects/hwloc/>`_ library.

Note that memory affinity support is enabled
*only when processor affinity is enabled.* Specifically: using memory
affinity does not make sense if processor affinity is not enabled
because processes may allocate local memory and then move to a
different processor, potentially remote from the memory that it just
allocated.
