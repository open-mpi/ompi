.. _label-mca:

The Modular Component Architecture (MCA)
========================================

Open MPI is a highly-customizable system; it can be configured via
configuration files, command line parameters, and environment
variables.  The main functionality of Open MPI's configuration system
is through the Modular Component Architecture (MCA).

* This section describes the MCA itself and how to set MCA parameters at
  run time.
* Later sections in this documentation describe different parts of
  Open MPI's functionality, and the specific names and values of MCA
  parameters that can be used to affect Open MPI's behavior.

.. note:: :ref:`The PMIx and PRRTE software packages
          <label-running-role-of-pmix-and-prte>` also use the MCA for
          their configuration, composition, and run-time tuning.

/////////////////////////////////////////////////////////////////////////

Terminology
-----------

The Modular Component Architecture (MCA) is the backbone for much of
Open MPI's functionality.  It is a series of *projects*, *frameworks*,
*components*, and *modules* that are assembled at run-time to create
an MPI implementation.

MCA *parameters* (also known as MCA *variables*) are used to customize
Open MPI's behavior at run-time.

Each of these entities are described below.

Projects
^^^^^^^^

A *project* is essentially the highest abstraction layer division in
the Open MPI code base.

.. note:: The word "project" is unfortunately overloaded.  It can be
          used to mean the code/resources/people in the greater Open
          MPI community associated with the development of a
          particular software package, but it can also be used to mean
          a major, top-level section of code within the Open MPI code
          base.

          For the purposes of this documentation, "project" means the
          latter: a major, top-level section of code within the Open
          MPI code base.

The following *projects* exist in Open MPI |ompi_ver|:

* **Open Portability Access Layer (OPAL):** Low-level, operating
  system and architecture portability code.
* **Open MPI (OMPI):** The MPI API and supporting infrastructure.
* **OpenSHMEM (OSHMEM):** The OpenSHMEM API and supporting
  infrastructure.

.. note:: Prior versions of Open MPI also included an Open MPI
          Runtime Environment (ORTE) project.  ORTE essentially
          evolved into the standalone `PMIx Runtime Reference
          Environment (PRRTE) <https://github.com/openpmix/prrte>`_
          and is now considered a 3rd-party dependency of Open MPI
          |mdash| not one of its included projects.

          See :ref:`the role of PMIx and PRRTE
          <label-running-role-of-pmix-and-prte>` for more information.

.. _label-mca-frameworks:

Frameworks
^^^^^^^^^^

An MCA framework manages zero or more components at run-time and is
targeted at a specific task (e.g., providing MPI collective operation
functionality).  Although each MCA framework supports only a single
type of component, it may support multiple components of that type.

Some of the more common frameworks that users may want or need to
customize include the following:

* ``btl``: Byte Transport Layer; these components are exclusively used
  as the underlying transports for the ``ob1`` PML component.
* ``coll``: MPI collective algorithms
* ``io``: MPI I/O
* ``mtl``: MPI Matching Transport Layer (MTL); these components are
  exclusively used as the underlying transports for the ``cm`` PML
  component.
* ``pml``: Point-to-point Messaging Layer (PML).  These components are
  used to implement MPI point-to-point messaging functionality.

There are many frameworks within Open MPI; the exact set varies
between different versions of Open MPI.  You can use the
:ref:`ompi_info(1) <man1-ompi_info>` command to see the full list of
frameworks that are included in Open MPI |ompi_ver|.

.. _label-mca-components:

Components
^^^^^^^^^^

An MCA component is an implementation of a framework's formal
interface.  It is a standalone collection of code that can be bundled
into a plugin that can be inserted into the Open MPI code base, either
at run-time and/or compile-time.

.. note:: Good synonyms for Open MPI's "component" concept are
          "plugin", or "add-on".

The exact set of components varies between different versions of Open
MPI.  Open MPI's code base includes support for many components, but
not all of them may be present or available on your system.  You can
use the :ref:`ompi_info(1) <man1-ompi_info>` command to see what
components are included in Open MPI |ompi_ver| on your system.

.. _label-mca-modules:

Modules
^^^^^^^

An MCA module is an instance of a component (in the C++ sense of the
word "instance"; an MCA component is analogous to a C++ class).  For
example, if a node running an Open MPI application has two Ethernet
NICs, the Open MPI application will contain one TCP MPI point-to-point
*component*, but two TCP point-to-point *modules*.

Parameters (variables)
^^^^^^^^^^^^^^^^^^^^^^

MCA *parameters* (sometimes called MCA *variables*) are the basic unit
of run-time tuning for Open MPI.  They are simple "key = value" pairs
that are used extensively throughout Open MPI.  The general rules of
thumb that the developers use are:

#. Instead of using a constant for an important value, make it an MCA
   parameter.
#. If a task can be implemented in multiple, user-discernible ways,
   implement as many as possible, and use an an MCA parameter to
   choose between them at run-time.

For example, an easy MCA parameter to describe is the boundary between
short and long messages in TCP wire-line transmissions.  "Short"
messages are sent eagerly whereas "long" messages use a rendezvous
protocol.  The decision point between these two protocols is the
overall size of the message (in bytes).  By making this value an MCA
parameter, it can be changed at run-time by the user or system
administrator to use a sensible value for a particular environment or
set of hardware (e.g., a value suitable for 1Gpbs Ethernet is probably
not suitable for 100 Gigabit Ethernet, and may require even a third
different value for 25 Gigabit Ethernet).

/////////////////////////////////////////////////////////////////////////

.. _label-running-setting-mca-param-values:

Setting MCA parameter values
----------------------------

MCA parameters may be set in several different ways.

.. admonition:: Rationale
   :class: tip

   Having multiple methods to set MCA parameters allows, for example,
   system administrators to fine-tune the Open MPI installation for
   their hardware / environment such that normal users can simply use
   the default values (that were set by the system administrators).

   HPC environments |mdash| and the applications that run on them
   |mdash| tend to be unique.  Providing extensive run-time tuning
   capabilities through MCA parameters allows the customization of
   Open MPI to each system's / user's / application's particular
   needs.

The following are the different methods to set MCA parameters, listed
in priority order:

#. Command line parameters
#. Environment variables
#. Tuning MCA parameter files
#. Configuration files

.. danger:: Due to how the PMIx and PRRTE projects both evolved to
            become independent projects from Open MPI (:ref:`see this
            section for more detail
            <label-running-role-of-pmix-and-prte>`), they both have
            their own MCA system for setting MCA parameters.

            Hence, all the information about MCA parameters below
            *also* applies to PMIx and PRRTE.

Command line parameters
^^^^^^^^^^^^^^^^^^^^^^^

The highest-precedence method is setting MCA parameters on the command
line.  For example:

.. code-block:: sh

   shell$ mpirun --mca mpi_show_handle_leaks 1 -np 4 a.out

This sets the MCA parameter ``mpi_show_handle_leaks`` to the value of
1 before running ``a.out`` with four processes.  In general, the
format used on the command line is ``--mca <param_name> <value>``.

.. note:: When setting a value that includes spaces, you need to use
          quotes to ensure that the shell understands that the
          multiple tokens are a single value.  For example:

          .. code-block:: sh

             shell$ mpirun --mca param "value with multiple words" ...

.. warning:: Setting Open MPI MCA parameters via the command line
             entails using the ``--mca`` CLI option.  When setting
             PMIx- and PRRTE-specific MCA parameters via configuration
             files, use a different CLI option:

             +----------+----------------+
             | Open MPI | ``--mca``      |
             +----------+----------------+
             | PMIx     | ``--pmixmca``  |
             +----------+----------------+
             | PRRTE    | ``--prtemca``  |
             +----------+----------------+

Environment variables
^^^^^^^^^^^^^^^^^^^^^

Next, environment variables are searched.  Any environment variable
named ``OMPI_MCA_<param_name>`` will be used.  For example, the
following has the same effect as the previous example (for sh-flavored
shells):

.. code-block:: sh

   shell$ export OMPI_MCA_mpi_show_handle_leaks=1
   shell$ mpirun -np 4 a.out

.. note:: Just like with command line values, setting environment
          variables to values with multiple words requires shell
          quoting, such as:

          .. code-block:: sh

             shell$ export OMPI_MCA_param="value with multiple words"

.. warning:: Setting Open MPI MCA parameters via environment variables
             entails prefixing the parameter name with ``OMPI_MCA_``.
             When setting PMIx- and PRRTE-specific MCA parameters via
             environment variables, use a different prefix:

             +----------+----------------+
             | Open MPI | ``OMPI_MCA_``  |
             +----------+----------------+
             | PMIx     | ``PMIX_MCA_``  |
             +----------+----------------+
             | PRRTE    | ``PRRTE_MCA_`` |
             +----------+----------------+

Tuning MCA parameter files
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. error:: TODO This entire section needs to be checked for correctness.

Simple text files can be used to set MCA parameter values for a
specific application.

The ``mpirun --tune`` CLI option allows users to specify both MCA
parameters and environment variables from within a single file.

MCA parameters set in tuned parameter files will override any MCA
parameters supplied in global parameter files (e.g.,
``$HOME/.openmpi/mca-params.conf``), but not command line or
environment parameters.

Consider a tuned parameter file name ``foo.conf`` that is placed in
the same directory as the application ``a.out``. A user will typically
run the application as:

.. code-block:: sh

   shell$ mpirun -np 2 a.out

To use the ``foo.conf`` tuned parameter file, this command line
changes to:

.. code-block:: sh

   shell$ mpirun -np 2 --tune foo.conf a.out

Tuned parameter files can be coupled if more than one file is to be
used. If there is another tuned parameter file called ``bar.conf``, it
can be added to the command line as follows:

.. code-block:: sh

   shell$ mpirun -np 2 --tune foo.conf,bar.conf a.out

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
contains a comma-delimited list of tuned parameter files exactly as
they would be passed to the ``--tune`` command line option.  The MCA
parameter ``mca_base_envvar_file_path`` specifies the path to search
for tuned files with relative paths.

.. error:: TODO Check that these MCA var names ^^ are correct.

Configuration files
^^^^^^^^^^^^^^^^^^^

Finally, simple configuration text files can be used to set MCA
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

More specifically, the MCA parameter ``mca_param_files`` specifies a
colon-delimited path of files to search for MCA parameters.  Files to
the left have lower precedence; files to the right are higher
precedence.

.. note:: Keep in mind that, just like components, these parameter
          files are *only* relevant where they are "visible"
          (:ref:`see this FAQ entry
          <installing-custom-components-label>`).  Specifically,
          Open MPI does not read all the values from these files
          during startup and then send them to all nodes in the job.
          Instead, the files are read on each node during each
          process' startup.

          *This is intended behavior:* it allows for per-node
          customization, which is especially relevant in heterogeneous
          environments.

.. error:: TODO This table needs to be checked for correctness.

.. warning:: Setting Open MPI MCA parameters via configuration files
             entails editing (by default) the ``mca-params.conf`` or
             ``openmpi-mca-params.conf`` files.  When setting PMIx-
             and PRRTE-specific MCA parameters via configuration
             files, set them (by default) in different files:

             +----------+------------------------------------------+
             | Open MPI | ``$HOME/.openmpi/mca-params.conf`` or    |
             |          | ``$prefix/etc/openmpi-mca-params.conf``  |
             +----------+------------------------------------------+
             | PMIx     | ``$HOME/.pmix/mca-params.conf`` or       |
             |          | ``$prefix/etc/openpmix-mca-params.conf`` |
             +----------+------------------------------------------+
             | PRRTE    | ``$HOME/.prrte/mca-params.conf`` or      |
             |          | ``$prefix/etc/prte-mca-params.conf``     |
             +----------+------------------------------------------+

/////////////////////////////////////////////////////////////////////////

.. _label-running-displaying-mca-param-values:

Displaying MCA parameter values
-------------------------------

MCA parameters are the "life's blood" of Open MPI. MCA parameters are
used to control both detailed and large-scale behavior of Open MPI and
are present throughout the code base.

This raises an important question: since MCA parameters can be set from a
file, the environment, the command line, and even internally within Open MPI,
how do I actually know what MCA params my job is seeing, and their value?

One way, of course, is to use the :ref:`ompi_info(1) <man1-ompi_info>`
command, which is documented elsewhere (you can use ``man ompi_info``,
or ``ompi_info --help`` to get more info on this command). However,
this still doesn't fully answer the question since ``ompi_info`` isn't
an MPI process.

To help relieve this problem, Open MPI provides the MCA parameter
``mpi_show_mca_params`` that directs the ``MPI_COMM_WORLD`` rank 0
process to report the name of MCA parameters, their current value as
seen by that process, and the source that set that value.  The
parameter can take several values that define which MCA parameters to
report:

* ``all``: report all MCA params. Note that this typically generates a
  rather long list of parameters since it includes all of the default
  parameters defined inside Open MPI
* ``default``: MCA params that are at their default settings - i.e.,
  all MCA params that are at the values set as default within Open MPI
* ``file``: MCA params that had their value set by a file
* ``api``: MCA params set using Open MPI's internal APIs, perhaps to
  override an incompatible set of conditions specified by the user
* ``enviro``: MCA params that obtained their value either from the
  local environment or the command line. Open MPI treats environmental
  and command line parameters as equivalent, so there currently is no
  way to separate these two sources

These options can be combined in any order by separating them with
commas.

Here is an example of the output generated by this parameter:

.. code-block:: sh

   shell$ mpirun --mca mpi_show_mca_params enviro hello_c
   [local-hostname:12345] mpi_show_mca_params=enviro (environment)
   Hello, World, I am 0 of 1

Note that several MCA parameters set by Open MPI itself for internal
uses are displayed in addition to the ones actually set by the user.

Since the output from this option can be long, and since it can be
helpful to have a more permanent record of the MCA parameters used for
a job, a companion MCA parameter ``mpi_show_mca_params_file`` is
provided. If ``mpi_show_mca_params_file`` is *also* set, the output
listing of MCA parameters will be directed into the specified file
instead of being printed to stdout.  For example:

.. code-block:: sh

   shell$ mpirun --mca mpi_show_mca_params enviro \
       --mca mpi_show_mca_param_file /tmp/foo.txt hello_c
   Hello, World, I am 0 of 1
   shell$ cat /tmp/foo.txt
   #
   # This file was automatically generated on Sun Feb  7 14:34:31 2021
   # by MPI_COMM_WORLD rank 0 (out of a total of 16) on savbu-usnic-a
   #
   mpi_show_mca_params=enviro (environment)
   mpi_show_mca_params_file=/tmp/foo.txt (environment)

/////////////////////////////////////////////////////////////////////////

.. _label-running-selecting-framework-components:

Selecting which Open MPI components are used at run time
--------------------------------------------------------

Each MCA framework has a top-level MCA parameter that helps guide
which components are selected to be used at run-time.  Specifically,
every framework has an MCA parameter of the same name that can be used
to *include* or *exclude* components from a given run.

For example, the ``btl`` MCA parameter is used to control which BTL
components are used.  It takes a comma-delimited list of component
names, and may be optionally prefixed with ``^``.  For example:

.. note:: The Byte Transfer Layer (BTL) framework is used as the
          underlying network transports with the `ob1` Point-to-point
          Messaging Layer (PML) component.

.. code-block:: sh

   # Tell Open MPI to include *only* the BTL components listed here and
   # implicitly ignore all the rest:
   shell$ mpirun --mca btl self,sm,usnic ...

   # Tell Open MPI to exclude the tcp and uct BTL components
   # and implicitly include all the rest
   shell$ mpirun --mca btl ^tcp,uct ...

Note that ``^`` can *only* be the prefix of the *entire*
comma-delimited list because the inclusive and exclusive behavior are
mutually exclusive.  Specifically, since the exclusive behavior means
"use all components *except* these", it does not make sense to mix it
with the inclusive behavior of not specifying it (i.e., "use all of
these components").  Hence, something like this:

.. code-block:: sh

   shell$ mpirun --mca btl self,sm,usnic,^tcp ...

does not make sense |mdash| and will cause an error |mdash| because it
says "use only the ``self``, ``sm``, and ``usnic`` components" but
also "use all components except ``tcp``".  These two statements
clearly contradict each other.

/////////////////////////////////////////////////////////////////////////

.. _label-mca-common-parameters:

Common MCA parameters
---------------------

Open MPI has a *large* number of MCA parameters available.  Users can
use the :ref:`ompi_info(1) <man1-ompi_info>` command to see *all*
available MCA parameters.

.. note:: Similarly, you can use the ``pmix_info(1)`` and
          ``prte_info(1)`` commands to see all the MCA parameters
          available for the PMIx and PRRTE projects, respectively.

          The documentation for these commands are not included in the
          Open MPI docs, but they are both quite similar to
          :ref:`ompi_info(1) <man1-ompi_info>`.

The vast majority of these MCA parameters, however, are not useful to
most users.  Indeed, there only are a handful of MCA parameters that
are commonly used by end users.  :ref:`As described in the
ompi_info(1) man page <man1-ompi_info-levels>`, MCA parameters are
grouped into nine levels, corresponding to the MPI standard's tool
support verbosity levels.  In general:

* Levels 1-3 are intended for the end user.

  * These parameters are generally used to effect whether an Open MPI
    job will be able to run correctly.

  .. tip:: Parameters in levels 1-3 are probably applicable to
           most end users.

* Levels 4-6 are intended for the application tuner.

  * These parameters are generally used to tune the performance of an
    Open MPI job.

* Levels 7-9 are intended for the MPI implementer.

  * These parameters are esoteric and really only intended for those
    who work deep within the implementation of Open MPI code base
    itself.

Although the full list of MCA parameters can be found in the output of
``ompi_info(1)``, the following list of commonly-used parameters is
presented here so that they can easily be found via internet searches:

* Individual framework names are used as MCA parameters to
  :ref:`select which components will be used
  <label-running-selecting-framework-components>`.  For example, the
  ``btl`` MCA parameter is used to select which components will be
  used from the ``btl`` framework.  The ``coll`` MCA parameter is used
  to select which ``coll`` components are used.  And so on.

* Individual framework names with the ``_base_verbose`` suffix
  appended (e.g., ``btl_base_verbose``, ``coll_base_verbose``, etc.)
  can be used to set the general verbosity level of all the components
  in that framework.

  * This can be helpful when troubleshooting why certain components
    are or are not being selected at run time.

* Many network-related components support "include" and "exclude"
  types of components (e.g., ``btl_tcp_if_include`` and
  ``btl_tcp_if_exclude``).  The "include" parameters specify an
  explicit set of network interfaces to use; the "exclude" parameters
  specify an explicit set of network interfaces to ignore.  Check the
  output from :ref:`ompi_info(1)'s <man1-ompi_info>` full list to see
  if the network-related component you are using has "include" and
  "exclude" network interface parameters.

  .. important:: You can only use the "include" *or* the "exclude"
                 parameter |mdash| they are mutually exclusive from each
                 other.
* ``mca_base_component_show_load_errors``: By default, Open MPI
  emits a warning message if it fails to open a DSO component at run
  time.  This typically happens when a shared library that the DSO
  requires is not available.

  .. admonition:: Rationale
     :class: tip

     In prior versions of Open MPI, components defaulted to building
     as DSOs (vs. being included in their parent libraries, such as
     ``libmpi.so``).  On misconfigured systems, sometimes network
     acceleration libraries would not be present, meaning that
     HPC-class networking components failed to open at run time.  As
     such, Open MPI would typically fall back to TCP as a network
     transport, which usually led to poor performance of end-user
     applications.

     Having Open MPI warn about such failures to load was useful
     because it alerted users to the misconfiguration.

  .. note:: By default, Open MPI |ompi_ver| includes all components in
            its base libraries (e.g., on Linux, ``libmpi.so`` includes
            all the components that were built with Open MPI, and
            therefore no component need to be opened dynamically), and
            does not build its components as DSOs.

            This MCA parameter *only* affects the behavior of when a
            component DSO fails to open.

  This MCA parameter can take four general values:

  #. ``yes`` or a boolean "true" value (e.g., ``1``): Open MPI will
     emit a warning about every component DSO that fails to load.

  #. ``no`` or a boolean "false" value (e.g., ``0``): Open MPI will
     never emit warnings about component DSOs that fail to load.

  #. A comma-delimited list of frameworks and/or components: Open MPI
     will emit a warning about any dynamic component that fails to
     open and matches a token in the list. "Match" is defined as:

     * If a token in the list is only a framework name, then any
       component in that framework will match.
     * If a token in the list specifies both a framework name and a
       component name (in the form ``framework/component``), then
       only the specified component in the specified framework will
       match.

     For example, if the value of this MCA parameter is
     ``accelerator,btl/uct``, then Open MPI warn if any component in
     the accelerator framework or if the UCT BTL fails to load at run
     time.

  #. The value can also be a ``^`` character followed by a
     comma-delimited list of ``framework[/component]`` values: This
     is similar to the comma-delimited list of tokens, except it will
     only emit warnings about dynamic components that fail to load
     and do *not* match a token in the list.

     For example, if the value of this MCA parameter is
     ``^accelerator,btl/uct``, then Open MPI will only warn about the
     failure to load DSOs that are neither in the accelerator
     framework nor are the UCT BTL.

.. _label-mca-backward-compat:

MCA Parameter Changes Between Open MPI 4.x and 5.x
--------------------------------------------------

When Open MPI :ref:`switched from using ORTE to PRRTE as its run-time
environment, <label-running-role-of-pmix-and-prte>` some MCA
parameters were renamed to be more consistent and/or allow more
flexible behavior.  The deprecated Open MPI MCA parameters listed
below are currently replaced by a corresponding new PRRTE parameter,
but may be removed in future releases.

.. note:: In all cases listed below, the deprecated MCA parameter is
          an Open MPI MCA parameter, meaning that its corresponding
          environment variable was prefixed with ``OMPI_MCA_`` (e.g.,
          ``OMPI_MCA_orte_xml_output``).  However, the corresponding
          new MCA parameter is a PRRTE MCA parameter, meaning that its
          corresponding environment variable is prefixed with
          ``PRTE_MCA_`` (e.g., ``PRTE_MCA_output``).

          .. important:: Yes, that's a single ``R`` in the
                         ``PRTE_MCA_`` environment variable prefix.
                         `See this explanation
                         <https://docs.prrte.org/>`_ for the when one
                         R or two R's are used in the PRRTE name.


.. list-table::
    :header-rows: 1

    * - Behavior
      - Deprecated MCA parameter
      - Replaced with

    * - Control buffering of stream output
      - ``orte_ess_base_stream_buffering``

        Values:  0 | 1 | 2

      - ``ompi_stream_buffering``

        Values: same

    * - Output a brief periodic report on launch progress
      - ``orte_report_launch_progress``

        Values: boolean
      - ``state_base_show_launch_progress``

        Values: same

    * - Provide all output in XML format
      - ``orte_xml_output``

        Values: boolean
      - ``output``

        Value: ``xml``

    * - Tag all output with [job,rank]
      - ``orte_tag_output``

        Values: boolean
      - ``output``

        Value: ``tag``

    * - Timestamp all application process output
      - ``orte_timestamp_output``

        Values: boolean
      - ``output``

        Value: ``timestamp``

    * - Redirect output from application processes into filename / job
        / rank / stdout / stderr / stdddiag.
      - ``orte_output_filename``

        Value: ``<filenname>``
      - ``output``

        Value: ``file=<filename>``

    * - Display a detailed process map just before launch
      - ``rmaps_base_display_devel_map``

        Values: boolean
      - ``display``

        Value: ``map-devel``

    * - Display the topology as part of the process map just before
        launch
      - ``rmaps_base_display_topo_with_map``

        Values: ``<value>``
      - ``display``

        Value: ``topo=<value>``

    * - Whether to report process bindings to stderr
      - ``hwloc_base_report_bindings``

        Values: boolean
      - ``display``

        Value: ``bind``

    * - Display the process map just before launch
      - ``rmaps_base_display_map``

        Values: boolean
      - ``display``

        Value: ``map``

    * - Display the allocation being used by this job
      - ``orte_display_alloc``

        Values: boolean
      - ``display``

        Value: ``allocation``

    * - Do not run any MPI applications on the local node
      - ``rmaps_base_no_schedule_local``

        Values: boolean
      - ``rmaps_default_mapping_policy``

        Value: ``[<mapping>]:nolocal``

    * - Nodes are allowed to be oversubscribed, even on a managed
        system, and overloading of processing elements
      - ``rmaps_base_oversubscribe``

        Values: boolean
      - ``rmaps_default_mapping_policy``

        Value: ``[<mapping>]:oversubscribe``

    * - Nodes are not to be oversubscribed, even if the system
        supports such operation
      - ``rmaps_base_no_oversubscribe``

        Values: boolean
      - ``rmaps_default_mapping_policy``

        Value: ``[<mapping>]:nooversubscribe``

    * - Use hardware threads as independent CPUs
      - ``hwloc_base_use_hwthreads_as_cpus``

        Values: boolean
      - ``rmaps_default_mapping_policy``

        Value: ``[<mapping>]:hwtcpus``

    * - Comma-separated list of ranges specifying logical cpus
        allocated to this job
      - ``hwloc_base_cpu_set``

        Value: ``<value>``
      - ``rmaps_default_mapping_policy``

        Value: ``pe-list=<value>``

    * - List of processor IDs to bind processes to
      - ``hwloc_base_cpu_list``

        Value: ``<value>``
      - ``rmaps_default_mapping_policy``

        Value: ``pe-list=<value>``

    * - Bind processes to cores
      - ``hwloc_base_bind_to_core``

        Values: boolean
      - ``hwloc_default_binding_policy``

        Value: ``core``

    * - Bind processes to sockets
      - ``hwloc_base_bind_to_socket``

        Values: boolean
      - ``hwloc_default_binding_policy``

        Value: ``package``

    * - Whether to map and rank processes round-robin by node
      - ``rmaps_base_bynode``

        Values: boolean
      - ``rmaps_default_mapping_policy``

        Value: ``node``

    * - Whether to map and rank processes round-robin by core
      - ``rmaps_base_bycore``

        Values: boolean
      - ``rmaps_default_mapping_policy``

        Value: ``core``

    * - Whether to map and rank processes round-robin by slot
      - ``rmaps_base_byslot``

        Values: boolean
      - ``rmaps_default_mapping_policy``

        Value: ``slot``

    * - Number of cpus to use for each process
      - ``rmaps_base_cpus_per_rank``

        Value: ``<X>``
      - ``rmaps_default_mapping_policy``

        Value: ``[<mapping>]:pe=<X>``

    * - Launch n processes per node on all allocated nodes
      - ``rmaps_ppr_n_pernode``

        Value: ``<X>``
      - ``rmaps_default_mapping_policy``

        Value: ``ppr:<X>:node``

    * - Launch one process per available node
      - ``rmaps_ppr_pernode``

        Values: boolean
      - ``rmaps_default_mapping_policy``

        Value: ``ppr:1:node``

    * - Launch n processes per socket on all allocated nodes
      - ``rmaps_ppr_n_persocket``

        Value: integer ``<X>``
      - ``rmaps_default_mapping_policy``

        Value: ``ppr:<X>:package``

    * - Comma-separated list of number of processes on a given
        resource type
      - ``rmaps_ppr_pattern``

        Value: ``<value>``
      - ``rmaps_default_mapping_policy``

        Value: ``ppr:<value>``

    * - Provide a rankfile file
      - ``orte_rankfile``

        Value: ``<filename>``
      - ``rmaps_default_mapping_policy``

        Value: ``rankfile:file=<filename>``

Examples
^^^^^^^^^^^^^^^^^^^

Converting many parameters in the table above are straightforward, where an
integer or boolean value is involved, but some of the conversions require
substituting a boolean with a value to the new parameter, or even constructing
a more complicated composite value for the new parameter.  Examples of all
of these types of conversions are given below.

Simple values, where only the name of the MCA parameter changed
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: ini

   # Old environment variable: (integer value)
   export OMPI_MCA_orte_ess_base_stream_buffering=2

   # New environment variable: (integer value)
   export PRTE_MCA_ompi_stream_buffering=2

.. code-block:: ini

   # Old environment variable: (boolean value)
   export OMPI_MCA_orte_report_launch_progress=1

   # New environment variable: (boolean value)
   export PRTE_MCA_state_base_show_launch_progress=1

Convert from boolean value to parameter for variable
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: ini

   # Old environment variable: (boolean value)
   export OMPI_MCA_orte_xml_output=1

   # New environment variable: (parameter value)
   export PRTE_MCA_output=xml

.. code-block:: ini

   # Old environment variables: (boolean value)
   export OMPI_MCA_orte_xml_output=1
   export OMPI_MCA_orte_timestamp_output=1

   # New environment variable: (parameter value)
   export PRTE_MCA_output=xml,timestamp

.. code-block:: ini

   # Old environment variable: (boolean value)
   export OMPI_MCA_rmaps_base_display_devel_map=1

   # New environment variable: (parameter value)
   export PRTE_MCA_display=map-devel

.. code-block:: ini

   # Old environment variables: (boolean value)
   export OMPI_MCA_rmaps_base_display_devel_map=1
   export OMPI_MCA_rmaps_base_report_bindings=1

   # New environment variable: (parameter value)
   export PRTE_MCA_display=map-devel,bind

Convert from string value to parameter for variable
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: ini

   # Old environment variable: (string value)
   export OMPI_MCA_orte_output_filename=output.txt

   # New environment variable: (parameter value)
   export PRTE_MCA_output=file=output.txt

.. code-block:: ini

   # Old environment variables: (boolean value)
   export OMPI_MCA_orte_xml_output=1
   export OMPI_MCA_orte_timestamp_output=1

   # Old environment variable: (string value)
   export OMPI_MCA_orte_output_filename=output.txt

   # New environment variable: (parameter value)
   export PRTE_MCA_output=xml,timestamp,file=output.txt

.. code-block:: ini

   # Old environment variable: (string value)
   export OMPI_MCA_rmaps_base_display_topo_with_map=node

   # New environment variable: (parameter value)
   export PRTE_MCA_display=topo=node

.. code-block:: ini

   # Old environment variables: (boolean value)
   export OMPI_MCA_rmaps_base_display_devel_map=1
   export OMPI_MCA_rmaps_base_report_bindings=1

   # Old environment variable: (string value)
   export OMPI_MCA_rmaps_base_display_topo_with_map=node

   # New environment variable: (parameter value)
   export PRTE_MCA_display=map-devel,bind,topo=node

Converting mapping parameters
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Mapping parameters were previously prefixed with ``rmaps_base_`` or ``hwloc_base_``
(and also the ``orte_rankfile`` parameter).  These have been updated
to the ``rmaps_default_mapping_policy`` and ``hwloc_default_binding_policy``
parameters to be more consistent and indicate that they are the *default*
mapping for processes.  Some of the old parameters are now values for a
new parameter and some are now suffixes, as shown in the examples below.

The examples below show conversions from old boolean parameters to new
parameter values:

.. code-block:: ini

   # Old environment variable: (boolean value)
   export OMPI_MCA_rmaps_base_bycore=1

   # New environment variable: (parameter value)
   export PRTE_MCA_rmaps_default_mapping_policy=core

.. code-block:: ini

   # Old environment variable: (boolean value)
   export OMPI_MCA_hwloc_base_bind_to_socket=1

   # New environment variable: (parameter value)
   export PRTE_MCA_hwloc_default_binding_policy=package


The examples below show conversions from old parameters that have integer or
string values to new parameter values with those same values:

.. code-block:: ini

   # Old environment variable: (string value)
   export OMPI_MCA_hwloc_base_cpu_set=1,3,8

   # New environment variable: (parameter value)
   export PRTE_MCA_rmaps_default_mapping_policy=pe-list=1,3,8

.. code-block:: ini

   # Old environment variable: (integer value)
   export OMPI_MCA_rmaps_ppr_n_persocket=4

   # New environment variable: (parameter value)
   export PRTE_MCA_rmaps_default_mapping_policy=ppr:4:package

.. code-block:: ini

   # Old environment variable: (string value)
   export OMPI_MCA_orte_rankfile=rankfile.txt

   # New environment variable: (parameter value)
   export PRTE_MCA_rmaps_default_mapping_policy=rankfile:file=rankfile.txt

The examples below show conversions from old parameters that map to suffixes
for new parameter values:

.. code-block:: ini

   # Old environment variable: (boolean value)
   export OMPI_MCA_hwloc_base_use_hwthreads_as_cpus=1

   # New environment variable: (standalone suffix)
   export PRTE_MCA_rmaps_default_mapping_policy=:hwtcpus

.. code-block:: ini

   # Old environment variable: (boolean value)
   export OMPI_MCA_rmaps_base_oversubscribe=1

   # New environment variable: (standalone suffix)
   export PRTE_MCA_rmaps_default_mapping_policy=:oversubscribe

The examples below show conversions from old parameters that map to suffixes
combined with parameters that have values:

.. code-block:: ini

   # Old environment variable: (string value)
   export OMPI_MCA_hwloc_base_cpu_set=1,3,8

   # Old environment variable: (boolean value)
   export OMPI_MCA_rmaps_base_oversubscribe=1

   # New environment variable: (suffix on value)
   export PRTE_MCA_rmaps_default_mapping_policy=pe-list=1,3,8:oversubscribe

.. code-block:: ini

   # Old environment variable: (integer value)
   export OMPI_MCA_rmaps_ppr_n_persocket=4

   # Old environment variable: (boolean value)
   export OMPI_MCA_hwloc_base_use_hwthreads_as_cpus=1

   # New environment variable: (suffix on value)
   export PRTE_MCA_rmaps_default_mapping_policy=ppr:4:package:hwtcpus

Multiple suffixes may be appended to a mapping value:

.. code-block:: ini

   # Old environment variable: (integer value)
   export OMPI_MCA_rmaps_ppr_n_persocket=4

   # Old environment variables: (boolean value)
   export OMPI_MCA_hwloc_base_use_hwthreads_as_cpus=1
   export OMPI_MCA_rmaps_base_oversubscribe=1

   # New environment variable: (suffix on value)
   export PRTE_MCA_rmaps_default_mapping_policy=ppr:4:package:hwtcpus:oversubscribe

