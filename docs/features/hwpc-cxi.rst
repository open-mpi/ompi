HPE Cassini (CXI) Hardware Performance Counters (HWPC)
======================================================

The ``hwpc_cxi`` hook component supports collecting and reporting HPE Cassini (CXI) hardware performance counters (HWPC).

The feature determines what to track during ``MPI_Init()`` and emits reports
during ``MPI_Finalize()``. Depending on verbosity settings, reporting can include
global summary statistics to stdout and optional host-specific detail files with
NIC-level counter deltas. Counter data is sampled at init/finalize boundaries and 
does not inject report work into the main application execution interval.

What The Feature Does
---------------------

The HWPC_CXI feature supports:

* Tracking default timeout-related counters.
* Tracking additional counter sets at higher report levels.
* Accepting user-selected counters from a file instead of default extended sets.
* Accepting counter group names (case-insensitive), individual mnemonic names,
  and exact counter names (including categorized forms).
* Emitting one-line network timeout summaries, aggregate min/mean/max + rate
  summaries, and optional NIC-level detail reports.

A network timeout is treated as a Slingshot network event (for example, a link
flap) that causes packet reissue from a NIC perspective. One event may surface
as multiple NIC-level timeouts.

At report level 1, the one-line timeout summary is suppressed when no timeouts
are detected and verbose output is disabled. It is emitted when timeouts are
detected, verbose output is enabled, or a higher report level is selected.

Building
--------

The ``hwpc_cxi`` component is built by default and has no build-time
dependencies; it reads counters from sysfs (``/sys/class/cxi``) and
``/run/cxi`` at run time:

* When the report level is not ``0``, ``MPI_Init()`` checks every node for a
  Cassini device (``/sys/class/cxi/cxi0``).  If any node lacks one, the
  first rank on each such node prints a warning and counter collection is
  disabled for the whole job.
* ``--disable-hwpc-cxi`` skips building the component.

The ``Miscellaneous`` section of the ``configure`` output summary reports
``HWPC-CXI support`` as ``yes`` or ``no (disabled)``.

Runtime Configuration
---------------------

Runtime options are configured through MCA parameters.

This feature is intended for Slingshot 11 (or newer) environments.

Two common forms are supported:

Environment variables:

.. code-block:: sh

   export OMPI_MCA_ompi_hook_hwpc_cxi_counter_report=2
   export OMPI_MCA_ompi_hook_hwpc_cxi_counter_summary_filter_zeros=true

``mpirun`` / ``mpiexec`` MCA options:

.. code-block:: sh

   mpirun --mca hook_hwpc_cxi_counter_report 2 ...

MCA Options
-----------

``ompi_hook_hwpc_cxi_counter_report``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Controls whether Cassini counters are collected and the verbosity of reporting.

* Default: ``0`` (collection is opt-in)

* ``0``: no Cassini counters collected; feature disabled completely 
* ``1``: network timeout counters collected; one-line display when timeouts are detected or verbose output is enabled
* ``2``: option 1 + CXI counter summary report
* ``3``: option 2 + details for all NICs on a node if said node experiences a timeout
* ``4``: option 2 + details for all NICs on all nodes if any node experiences a timeout
* ``5``: option 2 + details for all NICs on all nodes regardless of any timeouts

Operational notes:

* Valid range is ``0`` to ``5``.
* At level ``0`` the component adds no work to ``MPI_Init()`` or
  ``MPI_Finalize()``.
* At level ``1`` and above, ``MPI_Init()`` duplicates ``MPI_COMM_WORLD``
  (held until ``MPI_Finalize()``), splits a communicator of one leader per
  node, runs several small agreement collectives, and has each node leader
  read the selected counters from sysfs; ``MPI_Finalize()`` samples again
  and runs reduction collectives across the node leaders.  Expect added
  startup and shutdown time that grows with job size.
* Extra counter summaries are available from level ``2`` and above.
* NIC-level details are enabled from level ``3`` and above.
* To obtain meaningful delta reports, runtimes should be at least a few
  seconds.

``ompi_hook_hwpc_cxi_counter_verbose``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Enables verbose diagnostics about selected and collected counters.

* Type: boolean
* Default: ``false``

``ompi_hook_hwpc_cxi_counter_summary_filter_zeros``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Controls whether zero-delta counters are filtered from summary.

* Type: boolean
* Default: ``true``

``ompi_hook_hwpc_cxi_counter_mpi_t_enable``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Controls whether collected CXI counters are registered as MPI_T performance
variables. Enabling registration does not affect counter collection or
reporting.  Only the lowest local rank on each node registers the variables,
so other ranks in the same job see a shorter performance variable list.

* Type: boolean
* Default: ``false``
* Environment variable:
  ``OMPI_MCA_ompi_hook_hwpc_cxi_counter_mpi_t_enable=true``

``ompi_hook_hwpc_cxi_counter_file``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Path to a file with an alternate list of counter names to collect.

* If set, this list is used for counter selection.
* Default timeout counters are still collected in addition.
* Retry-handler counters should be prefixed with ``rh:``.
* Recommended with ``ompi_hook_hwpc_cxi_counter_report >= 2``.
* Default: not set

``ompi_hook_hwpc_cxi_counter_report_file``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Filename prefix for writing detailed reports to host-specific files.

* Used by report levels ``3``, ``4``, and ``5``.
* Output naming pattern: ``<hwpc_cxi_counter_report_file>.<hostname>``
* Useful on large jobs where stdout can be mixed or truncated.
* Required for levels ``3``, ``4``, and ``5`` to write detailed output.
* Default: not set

Output Behavior Summary
-----------------------

At finalize time, output may include:

* A one-line network timeout summary (for non-quiet modes).
* A global CXI summary table with min/mean/max and rates (levels ``2``).
  The table only counts NICs whose counter changed: ``Nonzero`` is the number
  of such NICs.  The ``(/s)`` beside ``Min`` and ``Max`` is the rate of the NIC
  that produced that delta; the one beside ``Mean`` is the mean rate.
* NIC-level detailed counters in host-specific report files (levels ``3``,
  ``4``, and ``5``) when a report-file prefix is configured.

If no report-file prefix is set, detailed host files are not produced. Summary
output for the selected report level is still written to stdout.

Counter Naming And Inputs
-------------------------

User input can reference:

* Counter groups (case-insensitive), for example ``CxiPerfStats``.
* Individual mnemonic names.
* Exact counter names, including category/classification variants.

For detailed Cassini counter semantics and taxonomy, refer to the HPE Cassini
performance guide documentation.

Notes
-----

* Zero filtering applies to summary counter rows; headings and context lines may
  still appear depending on report mode.
* Levels ``3``, ``4``, and ``5`` require ``counter_report_file`` to produce
  detailed host files; the module does not generate a default prefix.
