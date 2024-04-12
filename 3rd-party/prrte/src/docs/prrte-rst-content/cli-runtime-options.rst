.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

The ``--runtime-options`` command line directive must be accompanied
by a comma-delimited list of case-insensitive options that control the
runtime behavior of the job. The full directive need not be provided
|mdash| only enough characters are required to uniquely identify the
directive.

Runtime options are typically ``true`` or ``false``, though this is
not a requirement on developers. Since the value of each option may
need to be set (e.g., to override a default set by MCA parameter), the
syntax of the command line directive includes the use of an ``=``
character to allow inclusion of a value for the option. For example,
one can set the ``ABORT-NONZERO-STATUS`` option to ``true`` by
specifying it as ``ABORT-NONZERO-STATUS=1``. Note that boolean options
can be set to ``true`` using a non-zero integer or a case-insensitive
string of the word ``true``.  For the latter representation, the user
need only provide at least the ``T`` character. The same policy
applies to setting a boolean option to ``false``.

Note that a boolean option will default to ``true`` if provided
without a value. Thus, ``--runtime-options abort-nonzero`` is
sufficient to set the ``ABORT-NONZERO-STATUS`` option to ``true``.

Supported values include:

* ``ERROR-NONZERO-STATUS[=(bool)]``: if set to false, this directs the
  runtime to treat a process that exits with non-zero status as a
  normal termination.  If set to true, the runtime will consider such
  an occurrence as an error termination and take appropriate action
  |mdash| i.e., the job will be terminated unless a runtime option
  directs otherwise. This option defaults to a true value if the
  option is given without a value.

* ``DONOTLAUNCH``: directs the runtime to map but not launch the
  specified job. This is provided to help explore possible process
  placement patterns before actually starting execution. No value need
  be passed as this is not an option that can be set by default in
  PRRTE.

* ``SHOW-PROGRESS[=(bool)]``: requests that the runtime provide
  progress reports on its startup procedure |mdash| i.e., the launch
  of its daemons in support of a job. This is typically used to debug
  DVM startup on large systems.  This option defaults to a true value
  if the option is given without a value.

* ``NOTIFYERRORS[=(bool)]``: if set to true, requests that the runtime
  provide a PMIx event whenever a job encounters an error |mdash|
  e.g., a process fails.  The event is to be delivered to each
  remaining process in the job. This option defaults to a true value
  if the option is given without a value.  See ``--help
  notifications`` for more detail as to the PMIx event codes available
  for capturing failure events.

* ``RECOVERABLE[=(bool)]``: if set to true, this indicates that the
  application wishes to consider the job as recoverable |mdash| i.e.,
  the application is assuming responsibility for recovering from any
  process failure. This could include application-driven spawn of a
  substitute process or internal compensation for the missing
  process. This option defaults to a true value if the option is given
  without a value.

* ``AUTORESTART[=(bool)]``: if set to true, this requests that the
  runtime automatically restart failed processes up to "max restarts"
  number of times. This option defaults to a true value if the option
  is given without a value.

* ``CONTINUOUS[=(bool)]``: if set to true, this informs the runtime
  that the processes in this job are to run until explicitly
  terminated. Processes that fail are to be automatically restarted up
  to "max restarts" number of times. Notification of process failure
  is to be delivered to all processes in the application. This is the
  equivalent of specifying ``RECOVERABLE``, ``NOTIFYERRORS``, and
  ``AUTORESTART`` options except that the runtime, not the
  application, assumes responsibility for process recovery. This
  option defaults to a true value if the option is given without a
  value.

* ``MAX-RESTARTS=<int>``: indicates the maximum number of times a
  given process is to be restarted. This can be set at the application
  or job level (which will then apply to all applications in that
  job).

* ``EXEC-AGENT=<path>`` indicates the executable that shall be used to
  start an application process. The resulting command for starting an
  application process will be ``<path> app <app-argv>``. The path may
  contain its own command line arguments.

* ``DEFAULT-EXEC-AGENT``: directs the runtime to use the system
  default exec agent to start an application process. No value need be
  passed as this is not an option that can be set by default in PRRTE.

* ``OUTPUT-PROCTABLE[(=channel)]``: directs the runtime to report the
  convential debugger process table (includes PID and host location of
  each process in the application). Output is directed to stdout if
  the channel is ``-``, stderr if ``+``, or into the specified file
  otherwise. If no channel is specified, output will be directed to
  stdout.

* ``STOP-ON-EXEC``: directs the runtime to stop the application
  process(es) immediately upon exec'ing them. The directive will apply
  to all processes in the job.

* ``STOP-IN-INIT``: indicates that the runtime should direct the
  application process(es) to stop in ``PMIx_Init()``. The directive
  will apply to all processes in the job.

* ``STOP-IN-APP``: indicates that the runtime should direct
  application processes to stop at some application-defined place and
  notify they are ready-to-debug. The directive will apply to all
  processes in the job.

* ``TIMEOUT=<string>``: directs the runtime to terminate the job after
  it has executed for the specified time. Time is specified in
  colon-delimited format |mdash| e.g., ``01:20:13:05`` to indicate 1
  day, 20 hours, 13 minutes and 5 seconds. Time specified without
  colons will be assumed to have been given in seconds.

* ``SPAWN-TIMEOUT=<string>``: directs the runtime to terminate the job
  if job launch is not completed within the specified time. Time is
  specified in colon-delimited format |mdash| e.g., ``01:20:13:05`` to
  indicate 1 day, 20 hours, 13 minutes and 5 seconds.  Time specified
  without colons will be assumed to have been given in seconds.

* ``REPORT-STATE-ON-TIMEOUT[(=bool)]``: directs the runtime to provide
  a detailed report on job and application process state upon job
  timeout. This option defaults to a true value if the option is given
  without a value.

* ``GET-STACK-TRACES[(=bool)]``: requests that the runtime provide
  stack traces on all application processes still executing upon
  timeout. This option defaults to a true value if the option is given
  without a value.

* ``REPORT-CHILD-JOBS-SEPARATELY[(=bool)]``: directs the runtime to
  report the exit status of any child jobs spawned by the primary job
  separately. If false, then the final exit status reported will be
  zero if the primary job and all spawned jobs exit normally, or the
  first non-zero status returned by either primary or child jobs.
  This option defaults to a true value if the option is given without
  a value.

* ``AGGREGATE-HELP-MESSAGES[(=bool)]``: directs the runtime to
  aggregate help messages, reporting each unique help message once
  accompanied by the number of processes that reported it. This option
  defaults to a true value if the option is given without a value.

* ``FWD-ENVIRONMENT[(=bool)]``: directs the runtime to forward the
  entire local environment in support of the application. This option
  defaults to a true value if the option is given without a value.

The ``--runtime-options`` command line option has no qualifiers.

.. note:: Directives are case-insensitive.  ``FWD-ENVIRONMENT`` is the
          same as ``fwd-environment``.
