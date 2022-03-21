General Tuning
==============

.. TODO How can I create a TOC just for this page here at the top?

/////////////////////////////////////////////////////////////////////////

What is the Modular Component Architecture (MCA)?
-------------------------------------------------

The Modular Component Architecture (MCA) is the backbone for much of
Open MPI's functionality.  It is a series of *projects*, *frameworks*,
*components*, and *modules* that are assembled at run-time to create
an MPI implementation.

* **Projects:** An Open MPI project is essentially the highest
  abstraction layer division of code.

  .. note:: The word "project" is unfortunately overloaded.  It can be
            used to mean the code/resources/people in the greater Open
            MPI community associated with the development of a
            particular software package, but it can also be used to
            mean a section of code within the Open MPI code base.

            For the purposes of this documentation, "project" means
            the latter: a section of code within the Open MPI code
            base.

* **Frameworks:** An MCA framework manages zero or more components at
  run-time and is targeted at a specific task (e.g., providing MPI
  collective operation functionality).  Each MCA framework supports a
  single component type, but may support multiple versions of that
  type.  The framework uses the services from the MCA base
  functionality to find and/or load components.

* **Components:** An MCA component is an implementation of a
  framework's interface.  It is a standalone collection of code that
  can be bundled into a plugin that can be inserted into the Open MPI
  code base, either at run-time and/or compile-time.

* **Modules:** An MCA module is an instance of a component (in the C++
  sense of the word "instance"; an MCA component is analogous to a C++
  class).  For example, if a node running an Open MPI application has
  multiple ethernet NICs, the Open MPI application will contain one
  TCP MPI point-to-point *component*, but two TCP point-to-point
  *modules*.

/////////////////////////////////////////////////////////////////////////

What are MCA parameters?
------------------------

MCA parameters are the basic unit of run-time tuning for Open
MPI.  They are simple "key = value" pairs that are used extensively
throughout the code base.  The general rules of thumb that the
developers use are:

#. Instead of using a constant for an important value, make it an MCA
   parameter.
#. If a task can be implemented in multiple, user-discernible ways,
   implement as many as possible and make choosing between them be an MCA
   parameter.

For example, an easy MCA parameter to describe is the boundary between
short and long messages in TCP wire-line transmissions.  "Short"
messages are sent eagerly whereas "long" messages use a rendezvous
protocol.  The decision point between these two protocols is the
overall size of the message (in bytes).  By making this value an MCA
parameter, it can be changed at run-time by the user or system
administrator to use a sensible value for a particular environment or
set of hardware (e.g., a value suitable for 1Gpbs Ethernet is probably
not suitable for 100 Gigabit Ethernet, and may require even a third
different value for 40 Gigabit Ethernet).

Note that MCA parameters may be set in several different ways
(described in another FAQ entry).  This allows, for example, system
administrators to fine-tune the Open MPI installation for their
hardware / environment such that normal users can simply use the
default values.

More specifically, HPC environments |mdash| and the applications that run
on them |mdash| tend to be unique.  Providing extensive run-time tuning
capabilities through MCA parameters allows the customization of Open
MPI to each system's / user's / application's particular needs.

/////////////////////////////////////////////////////////////////////////

What projects are included in the Open MPI code base?
-----------------------------------------------------

The following *projects* exist in Open MPI |ompi_ver|:

* **Open Porability Access Layer (OPAL):** Low-level, operating
  system and architecture portability code.
* **Open MPI (OMPI):** The MPI API and supporting infrastructure.
* **OpenSHMEM (OSHMEM):** The OpenSHMEM API and supporting
  infrastructure.

.. note:: Prior versions of Open MPI also included an Open MPI
          Runtime Envionrment (ORTE) project.  ORTE essentially
          evolved into the standalone `PMIx Runtime Reference
          Environment (PRRTE) <https://github.com/openpmix/prrte>`_,
          and is now considered a 3rd-party dependency of Open MPI
          -- not one of its included projects.

/////////////////////////////////////////////////////////////////////////

What frameworks are in Open MPI?
--------------------------------

Each project has its own frameworks.

.. error:: TODO This question may be moot due to :doc:`this list
           already in the higher-level doc </developers/frameworks>`.


/////////////////////////////////////////////////////////////////////////

How do I know what components are in my Open MPI installation?
--------------------------------------------------------------

The ``ompi_info`` command, in addition to providing a wealth of
configuration information about your Open MPI installation, will list
all components (and the frameworks that they belong to) that are
available.  These include system-provided components as well as
user-provided components.

Please note that starting with Open MPI v1.8, ``ompi_info`` categorizes its
parameter parameters in so-called levels, as defined by the MPI_T
interface.  You will need to specify ``--level 9`` (or
``--all``) to show *all* MCA parameters.
`See this Cisco Blog entry
<https://blogs.cisco.com/performance/open-mpi-and-the-mpi-3-mpi_t-interface/>`_
for further information.

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
visibile to all nodes via a networked filesystem).  Open MPI does not
automatically send components to remote nodes when MPI jobs are run.

/////////////////////////////////////////////////////////////////////////

How do I know what MCA parameters are available?
------------------------------------------------

The ``ompi_info`` command can list the parameters for a given
component, all the parameters for a specific framework, or all
parameters.  Most parameters contain a description of the parameter;
all will show the parameter's current value.

For example, the following shows all the MCA parameters for all
components that ``ompi_info`` finds:

.. code-block:: sh

   # Starting with Open MPI v1.7, you must use "--level 9" to see
   # all the MCA parameters (the default is "--level 1"):
   shell$ ompi_info --param all all --level 9

   # Before Open MPI v1.7, the "--level" command line options
   # did not exist; do not use it.
   shell$ ompi_info --param all all

This example shows all the MCA parameters for all BTL components that
``ompi_info`` finds:

.. code-block:: sh

   # All remaining examples assume Open MPI v1.7 or later (i.e.,
   # they assume the use of the "--level" command line option)
   shell$ ompi_info --param btl all --level 9

This example shows all the MCA parameters for the TCP BTL component:

.. code-block:: sh

   shell$ ompi_info --param btl tcp --level 9

/////////////////////////////////////////////////////////////////////////

.. _faq-general-tuning-setting-mca-params:

How do I set the value of MCA parameters?
-----------------------------------------

There are multiple ways to set MCA parameters, each of which are
listed below, and are resolved in the following priority order:

#. **Command line:** The highest-precedence method is setting MCA
   parameters on the command line.  For example:

   .. code-block:: sh

      shell$ mpirun --mca mpi_show_handle_leaks 1 -n 4 a.out

   This sets the MCA parameter ``mpi_show_handle_leaks`` to the value
   of 1 before running ``a.out`` with four processes.  In general, the
   format used on the command line is ``--mca <param_name> <value>``.

   Note that when setting multi-word values, you need to use quotes to
   ensure that the shell and Open MPI understand that they are a
   single value.  For example:

   .. code-block:: sh

      shell$ mpirun --mca param "value with multiple words" ...

#. **Environment variable:** Next, environment variables are searched.
   Any environment variable named ``OMPI_MCA_<param_name>`` will be
   used.  For example, the following has the same effect as the
   previous example (for sh-flavored shells):

   .. code-block:: sh

      shell$ OMPI_MCA_mpi_show_handle_leaks=1
      shell$ export OMPI_MCA_mpi_show_handle_leaks
      shell$ mpirun -n 4 a.out

   Note that setting environment variables to values with multiple words
   requires quoting, such as:

   .. code-block:: sh

      shell$ OMPI_MCA_param="value with multiple words"

#. **Tuning MCA parameter files:** Simple text files can be used to
   set MCA parameter values for a specific application.  :ref:`See this FAQ
   entry for more details <faq-general-tuning-tune-param-files>`.

#. **Aggregate MCA parameter files:** Simple text files can be used to
   set MCA parameter values for a specific application.  :ref:`See this FAQ
   entry for more details <faq-general-tuning-amca-param-files>`.

   .. warning:: The use of AMCA param files is deprecated.

#. **Files:** Finally, simple text files can be used to set MCA
   parameter values.  Parameters are set one per line (comments are
   permitted).  For example:

   .. code-block:: ini

      # This is a comment
      # Set the same MCA parameter as in previous examples
      mpi_show_handle_leaks = 1

   Note that quotes are *not* necessary for setting multi-word values
   in MCA parameter files.  Indeed, if you use quotes in the MCA
   parameter file, they will be used as part of the value itself.  For
   example:

   .. code-block:: ini

      # The following two values are different:
      param1 = value with multiple words
      param2 = "value with multiple words"

   By default, two files are searched (in order):

   #. ``$HOME/.openmpi/mca-params.conf``: The user-supplied set of
      values takes the highest precedence.
   #. ``$prefix/etc/openmpi-mca-params.conf``: The system-supplied set
      of values has a lower precedence.

   More specifically, the MCA parameter ``mca_param_files`` specifies
   a colon-delimited path of files to search for MCA parameters.
   Files to the left have lower precedence; files to the right are
   higher precedence.

   .. note:: Keep in mind that, just like components, these parameter
             files are *only* relevant where they are "visible"
             (:ref:`see this FAQ entry
             <faq-general-tuning-install-components>`).  Specifically,
             Open MPI does not read all the values from these files
             during startup and then send them to all nodes in the job
             |mdash| the files are read on each node during each
             process' startup.  This is intended behavior: it allows
             for per-node customization, which is especially relevant
             in heterogeneous environments.

/////////////////////////////////////////////////////////////////////////

.. _faq-general-tuning-amca-param-files:

What are Aggregate MCA (AMCA) parameter files?
----------------------------------------------

.. error:: TODO This entire entry needs to be checked for correctness.
           Are AMCA files actually deprecated?

.. warning:: The use of AMCA param files is still available in Open
             MPI |ompi_ver|, but is deprecated, and may disappear
             in a future version of Open MPI.

Aggregate MCA (AMCA) parameter files contain MCA parameter key/value
pairs similar to the ``$HOME/.openmpi/mca-params.conf`` file described
in :ref:`this FAQ entry <faq-general-tuning-setting-mca-params>`.

The motivation behind AMCA parameter sets came from the realization
that certain applications require a large number of MCA parameters are
to run well and/or execute as the user expects.  Since these MCA
parameters are application-specific (or even application-run-specific)
they should not be set in a global manner, but only pulled in as
determined by the user.

MCA parameters set in AMCA parameter files will override any MCA
parameters supplied in global parameter files (e.g.,
``$HOME/.openmpi/mca-params.conf``), but not command line or
environment parameters.

AMCA parameter files are typically supplied on the command line via
the ``--am`` option.

For example, consider an AMCA parameter file called ``foo.conf``
placed in the same directory as the application ``a.out``. A user
will typically run the application as:

.. code-block:: sh

   shell$ mpirun -n 2 a.out

To use the ``foo.conf`` AMCA parameter file, this command line
changes to:

.. code-block:: sh

   shell$ mpirun -n 2 --am foo.conf a.out

If the user wants to override a parameter set in ``foo.conf`` they
can add it to the command line:

.. code-block:: sh

   shell$ mpirun -n 2 --am foo.conf --mca btl tcp,self a.out

AMCA parameter files can be coupled if more than one file is to be
used. If we have another AMCA parameter file called ``bar.conf``
that we want to use, we add it to the command line as follows:

.. code-block:: sh

   shell$ mpirun -n 2 --am foo.conf:bar.conf a.out

AMCA parameter files are loaded in priority order. This means that
``foo.conf`` AMCA file has priority over the ``bar.conf`` file. So
if the ``bar.conf`` file sets the MCA parameter
``mpi_leave_pinned=0`` and the ``foo.conf`` file sets this MCA
parameter to ``mpi_leave_pinned=1`` then the latter will be used.

The location of AMCA parameter files are resolved in a similar way as
the shell:

#. If no path operator is provided (i.e., ``foo.conf``), then
   Open MPI will search the ``$sysconfdir/amca-param-sets`` directory,
   then the current working directory.
#. If a relative path is specified, then only that path will be
   searched (e.g., ``./foo.conf``, ``baz/foo.conf``).
#. If an absolute path is specified, then only that path will be
   searched (e.g., ``/bip/boop/foo.conf``).

Although the typical use case for AMCA parameter files is to be
specified on the command line, they can also be set as MCA parameters
in the environment. The MCA parameter ``mca_base_param_file_prefix``
contains a ``:``-delimited list of AMCA parameter files exactly as
they would be passed to the ``--am`` command line option. The MCA
parameter ``mca_base_param_file_path`` specifies the path to search
for AMCA files with relative paths. By default this is
``$sysconfdir/amca-param-sets/:$CWD``.

/////////////////////////////////////////////////////////////////////////

.. _faq-general-tuning-tune-param-files:

How do I set application specific environment variables in global parameter files?
----------------------------------------------------------------------------------

.. error:: TODO This entire entry needs to be checked for correctness.

The ``mpirun`` ``--tune`` CLI options allows users to specify both MCA
parameters and environment variables from within a single file.

MCA parameters set in tuned parameter files will override any MCA
parameters supplied in global parameter files (e.g.,
``$HOME/.openmpi/mca-params.conf``), but not command line or
environment parameters.

Tuned parameter files are typically supplied on the command line via
the ``--tune`` option.

For example, consider an tuned parameter file called ``foo.conf``
placed in the same directory as the application ``a.out``. A user
will typically run the application as:

.. code-block:: sh

   shell$ mpirun -n 2 a.out

To use the ``foo.conf`` tuned parameter file, this command line
changes to:

.. code-block:: sh

   shell$ mpirun -n 2 --tune foo.conf a.out

Tuned parameter files can be coupled if more than one file is to be
used. If we have another tuuned parameter file called ``bar.conf``
that we want to use, we add it to the command line as follows:

.. code-block:: sh

   shell$ mpirun -n 2 --tune foo.conf,bar.conf a.out


The contents of tuned files consist of one or more lines, each of
which contain zero or more `-x` and `--mca` options.  Comments are not
allowed.  For example, the following tuned file:

.. code-block::

   -x envvar1=value1 -mca param1 value1 -x envvar2
   -mca param2 value2
   -x envvar3

is equivalent to:

.. code-block:: sh

   shell$ mpirun \
       -x envvar1=value1 -mca param1 value1 -x envvar2 \
       -mca param2 value2
       -x envvar3 \
       ...rest of mpirun command line...

Although the typical use case for tuned parameter files is to be
specified on the command line, they can also be set as MCA parameters
in the environment.  The MCA parameter ``mca_base_envvar_file_prefix``
contains a ``,``-delimited list of tuned parameter files exactly as
they would be passed to the ``--tune`` command line option.  The MCA
parameter ``mca_base_envvar_file_path`` specifies the path to search
for tune files with relative paths.

.. error:: TODO Check that these MCA var names ^^ are correct.

/////////////////////////////////////////////////////////////////////////

How do I select which components are used?
------------------------------------------

Each MCA framework has a top-level MCA parameter that helps guide
which components are selected to be used at run-time.  Specifically,
there is an MCA parameter of the same name as each MCA framework that
can be used to *include* or *exclude* components from a given run.

For example, the ``btl`` MCA parameter is used to control which BTL
components are used (e.g., MPI point-to-point communications;
:doc:`see the MCA frameworks listing </developers/frameworks>` for a full
listing).  It can take as a value a comma-separated list of components
with the optional prefix ``^``.  For example:

.. code-block:: sh

   # Tell Open MPI to exclude the tcp and uct BTL components
   # and implicitly include all the rest
   shell$ mpirun --mca btl ^tcp,uct ...

   # Tell Open MPI to include *only* the components listed here and
   # implicitly ignore all the rest (i.e., the loopback, shared memory,
   # etc.) MPI point-to-point components):
   shell$ mpirun --mca btl self,sm,usnic ...

Note that ``^`` can *only* be the prefix of the entire value because
the inclusive and exclusive behavior are mutually exclusive.
Specifically, since the exclusive behavior means "use all components
*except* these", it does not make sense to mix it with the inclusive
behavior of not specifying it (i.e., "use all of these components").
Hence, something like this:

.. code-block:: sh

   shell$ mpirun --mca btl self,sm,usnic,^tcp ...

does not make sense because it says both "use only the ``self``, ``sm``,
and ``usnic`` components" and "use all components except ``tcp``" and
will result in an error.

Just as with all MCA parameters, the ``btl`` parameter (and all
framework parameters) :ref:`can be set in multiple ways
<faq-general-tuning-setting-mca-params>`.

/////////////////////////////////////////////////////////////////////////

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

Open MPI will, by default, enable processor and memory affinty when
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
MPI's ``mpirun``, and therefore may need to use :ref:`a different
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
