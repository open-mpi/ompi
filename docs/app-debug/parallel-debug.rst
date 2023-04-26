Using Parallel Debuggers to Debug Open MPI Applications
=======================================================

Debugging with TotalView
~~~~~~~~~~~~~~~~~~~~~~~~

Debugging MPI applications has changed with different releases of
TotalView and Open MPI; it is best to consult TotalView's
documentation for how you should debug Open MPI applications with
TotalView.

Starting with Open MPI v5.0.0, debugging with Totalview requires the
use of the MPIR shim module to attach to MPI processes.  See the
:ref:`using MPIR-based tools with Open MPI section
<using-mpir-based-tools-label>` for instructions on building and
installing the MPIR shim module.

Once TotalView is attached to Open MPI processes, it can also view
Open MPI's message queues (for some transports), which can aid in
debugging.

.. note:: The integration of Open MPI message queue support is
   problematic with 64-bit versions of TotalView prior to v8.3:

   * The message queues view will be truncated.
   * Both the communicators and requests lists will be incomplete.
   * Both the communicators and requests lists may be filled with wrong
     values (such as an ``MPI_Send`` to the destination
     ``MPI_ANY_SOURCE``).

   There are two workarounds:

   * Use a 32-bit version of TotalView
   * Upgrade to TotalView v8.3 or later

Debugging with DDT
~~~~~~~~~~~~~~~~~~

Debugging MPI application with DDT has changed with different releases
of DDT and Open MPI; it is best to consult DDT's documentation for how
you should debug Open MPI applications with DDT.
