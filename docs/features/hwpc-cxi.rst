HPE Cassini (CXI) Hardware Performance Counters (HWPC)
======================================================

The ``hwpc_cxi`` hook component supports collecting and reporting HPE Cassini (CXI) hardware performance counters (HWPC).

The feature determines what to track during ``MPI_Init()`` and emits reports
during ``MPI_Finalize()``. Depending on verbosity settings, reporting can include
global summary statistics to stdout and, at levels 3-5, host-specific detail
files with NIC-level counter deltas. Counter data is sampled at init/finalize
boundaries and does not inject report work into the main application execution
interval.

What The Feature Does
---------------------

The HWPC_CXI feature supports:

* Tracking default timeout-related counters.
* Tracking additional counter sets at higher report levels.
* Accepting user-selected counters from a file instead of default extended sets.
* Accepting counter group names (case-insensitive), individual mnemonic names,
  and exact counter names (including categorized forms).
* Emitting one-line network timeout summaries, aggregate min/mean/max + rate
  summaries, and NIC-level detail report files.

A network timeout is treated as a Slingshot network event (for example, a link
flap) that causes packet reissue from a NIC perspective. One event may surface
as multiple NIC-level timeouts.

The network timeout count adds each NIC's SCT and SPT timeouts as counted by
the retry handler (``rh:sct_timeouts`` and ``rh:spt_timeouts``).  The PCT
counters ``pct_sct_timeouts`` and ``pct_spt_timeouts`` count the same events
and are always collected with them, but are never added as well; they are
used only on a NIC where the retry handler's counter could not be read (for
example, because the retry handler is not running).  Other selected
counters, including ``rh:tct_timeouts``, appear in the reports but are not
part of the count.

Counters are per NIC, not per job. The component samples every Cassini NIC on
the node, not only the NICs the job uses, and a NIC's counters include traffic
from every process and job using that NIC. On nodes shared with other jobs,
the reports therefore include those jobs' traffic.

At report level 1, the one-line timeout summary is suppressed when no timeouts
are detected and verbose output is disabled. It is emitted when timeouts are
detected, a NIC's timeout counters could not be read, verbose output is
enabled, or a higher report level is selected.

Building
--------

The ``hwpc_cxi`` component is built by default and has no build-time
dependencies; it reads counters from sysfs (``/sys/class/cxi``) and
``/run/cxi`` at run time:

* When the report level is not ``0``, ``MPI_Init()`` checks each node for a
  Cassini device (any ``/sys/class/cxi/cxi<N>`` entry; numbering may have
  gaps).  Enabling the feature on a node without one is a fatal error: the
  job aborts with a message naming the host and the missing path.
* At most 16 Cassini devices per node are monitored.  On a node with more,
  the 16 lowest-numbered devices are monitored and a warning is printed.
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

   export OMPI_MCA_hook_hwpc_cxi_counter_report=2
   export OMPI_MCA_hook_hwpc_cxi_counter_summary_filter_zeros=true

``mpirun`` / ``mpiexec`` MCA options:

.. code-block:: sh

   mpirun --mca hook_hwpc_cxi_counter_report 2 ...

MCA Options
-----------

``hook_hwpc_cxi_counter_report``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Controls whether Cassini counters are collected and the verbosity of reporting.

* Default: ``0`` (collection is opt-in)

Each level may be given by number or by name:

* ``0`` / ``quiet``: no Cassini counters collected; feature disabled completely
* ``1`` / ``timeouts``: network timeout counters collected; one-line display
  when timeouts are detected, timeout counters could not be read, or verbose
  output is enabled.  If a node can read none of the four timeout counters,
  collection is disabled for the job with a warning naming the node.
* ``2`` / ``summary``: level 1 + CXI counter summary report
* ``3`` / ``timeout-nics``: level 2 + details for each NIC that experienced a
  timeout
* ``4`` / ``all-nics-on-timeout``: level 2 + details for all NICs on all nodes
  if any node experiences a timeout
* ``5`` / ``all-nics``: level 2 + details for all NICs on all nodes regardless
  of any timeouts

Operational notes:

* Valid range is ``0`` to ``5``.
* The level must be set to the same value on every rank, as is normal for
  MCA parameters propagated by the launcher.  Different nonzero levels are
  detected and disable collection, but mixing level ``0`` on some ranks with
  a nonzero level on others is not supported and can hang or fail
  ``MPI_Init()``.
* At level ``0`` the component adds no work to ``MPI_Init()`` or
  ``MPI_Finalize()``.
* At level ``1`` and above, ``MPI_Init()`` splits a communicator of one
  leader per node from ``MPI_COMM_WORLD`` (held until ``MPI_Finalize()``),
  runs a fixed number of small agreement collectives that does not grow with
  the number of selected counters, and has each node leader read the selected
  counters from sysfs.  ``MPI_Finalize()`` waits for every rank to reach it
  (one barrier on ``MPI_COMM_WORLD``), so the final sample includes all of the
  job's traffic, including traffic from ranks that finish later than their
  node's leader.  It then samples again and runs reduction collectives across
  the node leaders.  Expect added startup and shutdown time that grows with job
  size.
* These ``MPI_Finalize()`` collectives run as soon as ``MPI_Finalize()``
  starts, before it calls the attribute-delete callbacks on
  ``MPI_COMM_SELF``.  If such a callback communicates with a rank that is
  waiting for that message before it calls ``MPI_Finalize()``, the job
  deadlocks when reporting is enabled.  The ``comm_method`` hook has the same
  limitation.
* Extra counter summaries are available from level ``2`` and above.
* NIC-level details are enabled from level ``3`` and above.
* To obtain meaningful delta reports, runtimes should be at least a few
  seconds.

``hook_hwpc_cxi_counter_verbose``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Enables verbose diagnostics about selected and collected counters.  Without
it, warnings about the counter list (for example, a selected counter that
does not exist) are printed once, by rank 0; with it, every node's leader
prints its own.

* Type: boolean
* Default: ``false``

``hook_hwpc_cxi_counter_summary_filter_zeros``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Controls whether zero-delta counters are filtered from summary.

* Type: boolean
* Default: ``true``

``hook_hwpc_cxi_counter_mpi_t_enable``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Controls whether collected CXI counters are registered as MPI_T performance
variables. Enabling registration does not affect counter collection or
reporting, but it needs collection to be enabled: set
``hook_hwpc_cxi_counter_report`` to ``1`` (``timeouts``) or higher, which
prints at most a one-line network timeout summary.  At the default level
``0`` the component does not run, so no variables are registered and Open MPI
prints a warning.  Only the lowest local rank on each node registers the
variables, so other ranks in the same job see a shorter performance variable
list.  Set the same value on every rank; the ranks do not compare it.
A ``COUNTER`` variable never decreases: if the underlying counter is reset or
wraps (for example, when the retry handler restarts), the variable keeps
counting from its previous value.

* Type: boolean
* Default: ``false``
* Environment variable:
  ``OMPI_MCA_hook_hwpc_cxi_counter_mpi_t_enable=true``

``hook_hwpc_cxi_counter_file``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Path to a file with an alternate list of counter names to collect.

* If set, this list is used for counter selection.
* Default timeout counters are still collected in addition.
* Retry-handler counters should be prefixed with ``rh:``.
* Ignored at level ``1``, which collects only the timeout counters.
* If the file cannot be opened, or contains no recognized counter names, a
  warning is printed and the default counter list is used instead.
* The path must be shorter than 512 characters; a longer path is an error
  that disables collection for the job.
* Every node must be able to read the file and see the same contents.  The
  node leaders compare their selected counter lists during ``MPI_Init()``; if
  any node differs (for example, because the file is missing there and it fell
  back to the defaults), collection is disabled for the whole job with a
  warning.
* Recommended with ``hook_hwpc_cxi_counter_report >= 2``.
* Default: not set

``hook_hwpc_cxi_counter_report_file``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Filename prefix for writing detailed reports to host-specific files.

* Used by report levels ``3``, ``4``, and ``5``.
* Output naming pattern: ``<hook_hwpc_cxi_counter_report_file>.<hostname>``
* A relative prefix, including the default, is relative to the working
  directory of each node's leader rank (usually the directory the job was
  launched from).  Use an absolute path to put the files elsewhere.
* Characters other than letters, digits, ``_``, ``-``, ``.``, and ``/`` are
  replaced with ``_``.  The file-name part must be shorter than 64 characters
  and the whole path shorter than 512; a longer prefix is an error that
  disables collection for the job.
* Useful on large jobs where stdout can be mixed or truncated.
* Setting it to an empty string selects the default.
* Each job overwrites the files, so jobs that share a prefix and a host (for
  example, a job and the children it starts with ``MPI_Comm_spawn()``, or two
  jobs that both use the default from the same directory) should use different
  prefixes.
* Default: ``hwpc_cxi_report`` (files ``hwpc_cxi_report.<hostname>``)

Output Behavior Summary
-----------------------

At finalize time, output may include:

* A one-line network timeout summary (for non-quiet modes).
* A global CXI summary table with min/mean/max and rates (level ``2`` and
  above).
  The table only counts NICs whose counter changed: ``Nonzero`` is the number
  of such NICs.  The ``(/s)`` beside ``Min`` and ``Max`` is the rate of the NIC
  that produced that delta; the one beside ``Mean`` is the mean rate.
* A counter whose change could not be measured on a NIC is reported as
  unavailable, never as ``0``.  This happens when the counter could not be read
  at ``MPI_Init()`` or ``MPI_Finalize()`` (for example, the retry handler
  restarted and its ``/run/cxi/cxi<N>`` files disappeared), or when a
  cumulative counter went backwards.  The node leader prints a warning for
  failed reads at ``MPI_Finalize()``; detail reports show ``unavailable`` in the
  delta and rate columns; the summary table leaves such readings out and states
  how many there were; and if any NIC's timeout counters were unavailable, the
  timeout line reads ``at least N network timeouts`` and names how many NICs
  were affected.
* Retry-handler gauges (``rh:*_in_use``, ``rh:nid_tree_count``,
  ``rh:switch_tree_count``, ``rh:parked_nids``, ``rh:parked_switches``) hold a
  current level rather than a running count.  Their delta is the level at
  ``MPI_Finalize()``, their rate is ``0``, and their MPI_T pvars use the
  ``LEVEL`` class instead of ``COUNTER``.
* NIC-level detailed counters in host-specific report files (levels ``3``,
  ``4``, and ``5``), named with ``hook_hwpc_cxi_counter_report_file`` or its
  default, ``hwpc_cxi_report``.  Each node leader creates its file at these
  levels; at levels ``3`` and ``4`` the file stays empty when that level has
  nothing to report for the node.
* At levels ``3``, ``4``, and ``5``, rank 0 also writes
  ``<prefix>.counter_descriptions`` with the descriptions of the counter
  groups and mnemonics among the selected counters.

Summary output for the selected report level is always written to stdout.

Counter Naming And Inputs
-------------------------

User input can reference:

* Counter groups, for example ``CxiPerfStats``.
* Individual mnemonic names.
* Exact counter names, including category/classification variants.

In the counter file, names are separated by commas, spaces, tabs, or newlines.
Group and mnemonic names are matched case-insensitively; any other name must
match the counter's file name under ``/sys/class/cxi/cxi<N>/device/telemetry``
(or ``/run/cxi/cxi<N>`` with the ``rh:`` prefix), for example
``lpe_rndzv_puts_0``.  Comment lines are not supported.  A name that crosses
the 511th character of a line is skipped with a warning; keep lines shorter or
put each name on its own line.

For detailed Cassini counter semantics and taxonomy, refer to the HPE Cassini
performance guide documentation.

Notes
-----

* Zero filtering applies to summary counter rows; headings and context lines may
  still appear depending on report mode.
* Levels ``3``, ``4``, and ``5`` always write detailed host files; without
  ``hook_hwpc_cxi_counter_report_file`` they use the default prefix
  ``hwpc_cxi_report`` in each node leader's working directory.
* Counters are collected only for programs that call ``MPI_Init()`` or
  ``MPI_Init_thread()``; a program that uses only MPI Sessions collects
  nothing.
