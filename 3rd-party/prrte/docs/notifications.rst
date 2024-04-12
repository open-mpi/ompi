.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

Notifications
=============

PRRTE provides notifications on a variety of process and job
states. Each notification includes not only the PMIx event code that
generated it, but also information on the cause of the event to the
extent to which this is known.

Supported job events include:

* ``PMIX_READY_FOR_DEBUG``: indicates that all processes in the
  reported nspace have reached the specified debug stopping point.

* ``PMIX_LAUNCH_COMPLETE``: indicates that the reported nspace has
  been launched |mdash| i.e., the involved PRRTE daemons all report
  that their respective child processes have completed fork/exec.

* ``PMIX_ERR_JOB_CANCELED``: indicates that the job was cancelled by
  user command, usually issued via an appropriate PMIx-enabled tool.

* ``PMIX_ERR_JOB_FAILED_TO_LAUNCH``: indicates that the specified job
  failed to launch.  This can be due to a variety of factors that
  include inability to find the executable on at least one involved
  node.

Supported process events include:

* ``PMIX_ERR_PROC_TERM_WO_SYNC``: indicates that at least one process
  in the job called ``PMIx_Init``, thus indicating some notion of a
  global existence, and at least one process in the job subsequently
  exited without calling ``PMIx_Finalize``. This usually indicates a
  failure somewhere in the application itself that precluded an
  orderly shutdown of the process. Notification will include the
  process ID that exited in this manner.

* ``PMIX_EVENT_PROC_TERMINATED``: indicates that the reported process
  terminated normally.  Notification will include the process ID that
  exited and its exit status.

* ``PMIX_ERR_PROC_KILLED_BY_CMD``: indicates that the reported process
  was killed by PRRTE command. This typically occurs in response to a
  Ctrl-C (or equivalent) being applied to the PRRTE launcher, thereby
  instructing PRRTE to forcibly terminate its processes. The event
  currently will only be issued in the case where forcible termination
  is commanded via a tool that can pass the process IDs that are
  specifically to be terminated |mdash| otherwise, in the case of the
  Ctrl-C event previously described, all processes in the job will be
  terminated, leaving none to be notified. Notification will include
  the process ID that was terminated.

* ``PMIX_ERR_PROC_SENSOR_BOUND_EXCEEDED``: indicates that the
  specified process exceeded a previously-set sensor boundary |mdash|
  e.g., it may have grown beyond a defined memory limit. Such events
  may or may not automatically trigger termination by command,
  depending upon the behavior of the sensor. Notification will include
  the process ID that exceeded the sensor boundary plus whatever
  information the sensor provides regarding measurements and bounds.

* ``PMIX_ERR_PROC_ABORTED_BY_SIG``: indicates that the specified
  process was killed by a signal |mdash| e.g., a segmentation
  fault/violation or an externally applied signal. Notifications will
  include the process ID that was killed and the corresponding
  reported signal.

* ``PMIX_ERR_PROC_REQUESTED_ABORT:`` indicates that the specified
  process has aborted by calling the ``PMIx_Abort``
  function. Notification will include the process ID that called abort
  and its exit status.

* ``PMIX_ERR_EXIT_NONZERO_TERM``: indicates that the specified process
  terminated with a non-zero exit status. This notification is only
  generated in the case where the runtime option
  ``ERROR-NONZERO-STATUS`` is set to true, thereby indicating that a
  process exiting with non-zero status is to be considered an
  error. As PRRTE can be overwhelmed by a large job where every
  process exits with a non-zero status, only the *first* process in a
  given job that exits with a non-zero status will generate a
  notification unless the ``RECOVERABLE`` runtime option is also
  provided as otherwise the job will be immediately
  terminated. Notifications will include the process ID that exited
  and the status it returned.

* ``PMIX_ERR_PROC_RESTART``: indicates that the specified process has
  been restarted.  Additional information may include the hostname
  where the process is now executing.
