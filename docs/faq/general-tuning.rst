General Tuning
==============

.. TODO How can I create a TOC just for this page here at the top?

/////////////////////////////////////////////////////////////////////////

.. _faq-general-tuning-install-components:

How do I install my own components into an Open MPI installation?
-----------------------------------------------------------------

By default, Open MPI looks in two places for components at
run-time (in order):

#. ``$prefix/lib/openmpi/``: This is the system-provided components
   directory, part of the installation tree of Open MPI itself.
#. ``$HOME/.openmpi/components/``: This is where users can drop their
   own components that will automatically be "seen" by Open MPI at
   run-time.  This is ideal for developmental, private, or otherwise
   unstable components.

Note that the directories and search ordering used for finding
components in Open MPI is, itself, an MCA parameter.  Setting the
``mca_component_path`` changes this value (a colon-delimited list of
directories).

Note also that components are only used on nodes where they are
"visible". Hence, if your ``$prefix/lib/openmpi/`` is a directory on a
local disk that is not shared via a network filesystem to other nodes
where you run MPI jobs, then components that are installed to that
directory will *only* be used by MPI jobs running on the local node.

More specifically: components have the same visibility as normal
files.  If you need a component to be available to all nodes where you
run MPI jobs, then you need to ensure that it is visible on all nodes
(typically either by installing it on all nodes for non-networked
filesystem installs, or by installing them in a directory that is
visible to all nodes via a networked filesystem).  Open MPI does not
automatically send components to remote nodes when MPI jobs are run.

/////////////////////////////////////////////////////////////////////////

.. _faq-tuning-using-paffinity-label:

What is processor affinity?  Does Open MPI support it?
------------------------------------------------------

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

/////////////////////////////////////////////////////////////////////////

What is memory affinity?  Does Open MPI support it?
---------------------------------------------------

Memory affinity is increasingly relevant on modern servers
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

Open MPI supports memory affinity on a variety of systems.

In recent versions of Open MPI, memory affinity is controlled through
the `Hardware Locality (hwloc)
<https://www.open-mpi.org/projects/hwloc/>`_ library.

Note that memory affinity support is enabled
*only when processor affinity is enabled.* Specifically: using memory
affinity does not make sense if processor affinity is not enabled
because processes may allocate local memory and then move to a
different processor, potentially remote from the memory that it just
allocated.

/////////////////////////////////////////////////////////////////////////

How do I tell Open MPI to use processor and/or memory affinity?
---------------------------------------------------------------

Open MPI will, by default, enable processor and memory affinity when
not running in an oversubscribed environment (i.e., when the number of
MPI processes are less than or equal two the number of processors
available).

The ``mpirun(1)`` man page for each version of Open MPI contains a lot of
information about the use of processor and memory affinity.  You
should consult the ``mpirun(1)`` page for your version of Open MPI for
detailed information about processor/memory affinity.

.. error:: TODO Link to mpirun(1) ...?

/////////////////////////////////////////////////////////////////////////

Does Open MPI support calling fork(), system(), or popen() in MPI processes?
----------------------------------------------------------------------------

It depends on a lot of factors, including (but not limited to):

* The operating system
* The underlying compute hardware
* The network stack
* Interactions with other middleware in the MPI process

In some cases, Open MPI will determine that it is not safe to
``fork()``.  In these cases, Open MPI will register a
``pthread_atfork()`` callback to print a warning when the process
forks.

This warning is helpful for legacy MPI applications where the current
maintainers are unaware that ``system()`` or ``popen()`` is being invoked from
an obscure subroutine nestled deep in millions of lines of Fortran code
(we've seen this kind of scenario many times).

However, this atfork handler can be dangerous because there is no way
to *unregister* an atfork handler.  Hence, packages that
dynamically open Open MPI's libraries (e.g., Python bindings for Open
MPI) may fail if they finalize and unload libmpi, but later call
fork.  The atfork system will try to invoke Open MPI's atfork handler;
nothing good can come of that.

For such scenarios, or if you simply want to disable printing the
warning, Open MPI can be set to never register the atfork handler with
the ``mpi_warn_on_fork`` MCA parameter.  For example:

.. code-block:: sh

   shell$ mpirun --mca mpi_warn_on_fork 0 ...

Of course, systems that ``dlopen("libmpi.so", ...)`` may not use Open
MPI's ``mpirun``, and therefore may need to use (JMS: this ref no
longer exists -- it moved to running-apps/tuning.rst) a different
mechanism to set MCA parameters
<faq-general-tuning-setting-mca-params>`.

/////////////////////////////////////////////////////////////////////////

I want to run some performance benchmarks with Open MPI.  How do I do that?
---------------------------------------------------------------------------

Running benchmarks is an extremely difficult task to do correctly.
There are many, many factors to take into account; it is *not* as
simple as just compiling and running a stock benchmark application.
This documentation is by no means a definitive guide, but it does try
to offer some suggestions for generating accurate, meaningful
benchmarks.

#. Decide *exactly* what you are benchmarking and setup your system
   accordingly.  For example, if you are trying to benchmark maximum
   performance, then many of the suggestions listed below are
   extremely relevant (be the only user on the systems and network in
   question, be the only software running, use processor affinity,
   etc.).  If you're trying to benchmark average performance, some of
   the suggestions below may be less relevant.  Regardless, it is
   critical to *know* exactly what you're trying to benchmark, and
   *know* (not guess) both your system and the benchmark application
   itself well enough to understand what the results mean.

   To be specific, many benchmark applications are not well understood
   for exactly what they are testing.  There have been many cases
   where users run a given benchmark application and wrongfully
   conclude that their system's performance is bad |mdash| solely on
   the basis of a single benchmark that they did not understand.  Read
   the documentation of the benchmark carefully, and possibly even
   look into the code itself to see exactly what it is testing.

   Case in point: not all ping-pong benchmarks are created equal.
   Most users assume that a ping-pong benchmark is a ping-pong
   benchmark is a ping-pong benchmark.  But this is not true; the
   common ping-pong benchmarks tend to test subtly different things
   (e.g., NetPIPE, TCP bench, IMB, OSU, etc.).  *Make sure you
   understand what your benchmark is actually testing.*

#. Make sure that you are the *only* user on the systems where you are
   running the benchmark to eliminate contention from other
   processes.

#. Make sure that you are the *only* user on the entire network /
   interconnect to eliminate network traffic contention from other
   processes.  This is usually somewhat difficult to do, especially in
   larger, shared systems.  But your most accurate, repeatable results
   will be achieved when you are the only user on the entire network.

#. Disable all services and daemons that are not being used.  Even
   "harmless" daemons consume system resources (such as RAM) and cause
   "jitter" by occasionally waking up, consuming CPU cycles, reading
   or writing to disk, etc.  The optimum benchmark system has an
   absolute minimum number of system services running.

#. Ensure that processor and memory affinity are properly utilized to
   disallow the operating system from swapping MPI processes between
   processors (and causing unnecessary cache thrashing, for example).

   .. warning:: On NUMA architectures, having the processes getting
                bumped from one socket to another is more expensive in
                terms of cache locality (with all of the cache
                coherency overhead that comes with the lack of it)
                than in terms of memory transfer routing (see below).

#. Be sure to understand your system's architecture, particularly with
   respect to the memory, disk, and network characteristics, and test
   accordingly.  For example, on NUMA architectures, memory accesses
   may be routed through a memory interconnect; remote device and/or
   memory accesses will be noticeably slower than local device and/or
   memory accesses.

#. Compile your benchmark with the appropriate compiler optimization
   flags.  With some MPI implementations, the compiler wrappers (like
   ``mpicc``, ``mpifort``, etc.) add optimization flags
   automatically.  Open MPI does not.  Add ``-O`` or other flags
   explicitly.

#. Make sure your benchmark runs for a sufficient amount of time.
   Short-running benchmarks are generally less accurate because they
   take fewer samples; longer-running jobs tend to take more samples.

#. If your benchmark is trying to benchmark extremely short events
   (such as the time required for a single ping-pong of messages):

   * Perform some "warmup" events first.  Many MPI implementations
     (including Open MPI) |mdash| and other subsystems upon which the
     MPI uses |mdash| may use "lazy" semantics to setup and maintain
     streams of communications.  Hence, the first event (or first few
     events) may well take significantly longer than subsequent
     events.

   * Use a high-resolution timer if possible |mdash|
     ``gettimeofday()`` only returns millisecond precision (sometimes
     on the order of several microseconds).

   * Run the event many, many times (hundreds or thousands, depending
     on the event and the time it takes).  Not only does this provide
     more samples, it may also be necessary, especially when the
     precision of the timer you're using may be several orders of
     magnitude less precise than the event you're trying to
     benchmark.

#. Decide whether you are reporting minimum, average, or maximum
   numbers, and have good reasons why.

#. Accurately label and report all results.  Reproducibility is a
   major goal of benchmarking; benchmark results are effectively
   useless if they are not precisely labeled as to exactly what they
   are reporting.  Keep a log and detailed notes about the ''exact''
   system configuration that you are benchmarking.  Note, for example,
   all hardware and software characteristics (to include hardware,
   firmware, and software versions as appropriate).

/////////////////////////////////////////////////////////////////////////

I am getting a MPI_WIN_FREE error from IMB-EXT |mdash| what do I do?
--------------------------------------------------------------------

When you run IMB-EXT with Open MPI, you'll see a
message like this:

.. code-block::

   [node01.example.com:2228] *** An error occurred in MPI_Win_free
   [node01.example.com:2228] *** on win
   [node01.example.com:2228] *** MPI_ERR_RMA_SYNC: error while executing rma sync
   [node01.example.com:2228] *** MPI_ERRORS_ARE_FATAL (your MPI job will now abort)

This is due to a bug in the Intel MPI Benchmarks, known to be in at
least versions v3.1 and v3.2.  Intel was notified of this bug in May
of 2009.  If you have a version after then, the bug should be fixed.
If not, here is the fix that you can apply to the IMB-EXT source code
yourself.

Here is a small patch that fixes the bug in IMB v3.2:

.. code-block:: diff

   diff -u imb-3.2-orig/src/IMB_window.c imb-3.2-fixed/src/IMB_window.c
   --- imb-3.2-orig/src/IMB_window.c     2008-10-21 04:17:31.000000000 -0400
   +++ imb-3.2-fixed/src/IMB_window.c      2009-07-20 09:02:45.000000000 -0400
   @@ -140,6 +140,9 @@
                             c_info->rank, 0, 1, c_info->r_data_type,
                             c_info->WIN);
              MPI_ERRHAND(ierr);
              }
   +          /* Added a call to MPI_WIN_FENCE, per MPI-2.1 11.2.1 */
   +          ierr = MPI_Win_fence(0, c_info->WIN);
   +          MPI_ERRHAND(ierr);
              ierr = MPI_Win_free(&c_info->WIN);
              MPI_ERRHAND(ierr);
              }

And here is the corresponding patch for IMB v3.1:

.. code-block:: diff

   Index: IMB_3.1/src/IMB_window.c
   ===================================================================
   --- IMB_3.1/src/IMB_window.c(revision 1641)
   +++ IMB_3.1/src/IMB_window.c(revision 1642)
   @@ -140,6 +140,10 @@
                             c_info->rank, 0, 1, c_info->r_data_type, c_info->WIN);
              MPI_ERRHAND(ierr);
              }
   +          /* Added a call to MPI_WIN_FENCE here, per MPI-2.1
   +             11.2.1 */
   +          ierr = MPI_Win_fence(0, c_info->WIN);
   +          MPI_ERRHAND(ierr);
              ierr = MPI_Win_free(&c_info->WIN);
              MPI_ERRHAND(ierr);
    }
