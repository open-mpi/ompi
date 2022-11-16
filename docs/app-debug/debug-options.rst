Open MPI Runtime Debugging Options
==================================

Open MPI has a series of MCA parameters for the MPI layer
itself that are designed to help with debugging.
These parameters :ref:`can be set <label-running-setting-mca-param-values>`
in the usual ways.  MPI-level MCA parameters can be
displayed by invoking the following command:

.. code-block:: sh

   # Use "--level 9" to see all the MCA parameters
   # (the default is "--level 1"):
   shell$ ompi_info --param mpi all --level 9

Here is a summary of the debugging parameters for the MPI layer:

* ``mpi_param_check``: If set to true (any positive value), and when
  Open MPI is compiled with parameter checking enabled (the default),
  the parameters to each MPI function can be passed through a series
  of correctness checks.  Problems such as passing illegal values
  (e.g., NULL or ``MPI_DATATYPE_NULL`` or other "bad" values) will be
  discovered at run time and an MPI exception will be invoked (the
  default of which is to print a short message and abort the entire
  MPI job).  If set to false, these checks are disabled, slightly
  increasing performance.

* ``mpi_show_handle_leaks``: If set to true (any positive value),
  Open MPI will display lists of any MPI handles that were not freed before
  :ref:`MPI_Finalize(3) <mpi_finalize>`  (e.g., communicators,
  datatypes, requests, etc.)

* ``mpi_no_free_handles``: If set to true (any positive value), do not
  actually free MPI objects when their corresponding MPI "free"
  function is invoked (e.g., do not free communicators when
  :ref:`MPI_Comm_free(3) <mpi_comm_free>`.  This can be helpful in tracking down
  applications that accidentally continue to use MPI handles after
  they have been freed.

* ``mpi_show_mca_params``: If set to true (any positive value), show a
  list of all MCA parameters and their values when MPI is initialized.
  This can be quite helpful for reproducibility of MPI applications.

* ``mpi_show_mca_params_file``: If set to a non-empty value, and if
  the value of ``mpi_show_mca_params`` is true, then output the list
  of MCA parameters to the filename value.  If this parameter is an
  empty value, the list is sent to ``stderr``.

* ``mpi_abort_delay``: If nonzero, print out an identifying message
  when :ref:`MPI_Abort(3) <mpi_abort>` is invoked showing the hostname and PID of the
  process that invoked :ref:`MPI_Abort(3) <mpi_abort>`, and then delay that many seconds
  before exiting.  A negative value means to delay indefinitely.  This
  allows a user to manually come in and attach a debugger when an
  error occurs.  Remember that the default MPI error handler |mdash|
  ``MPI_ERRORS_ABORT`` |mdash| invokes :ref:`MPI_Abort(3) <mpi_abort>`, so this
  parameter can be useful to discover problems identified by
  ``mpi_param_check``.

* ``mpi_abort_print_stack``: If nonzero, print out a stack trace (on
  supported systems) when :ref:`MPI_Abort(3) <mpi_abort>` is invoked.

