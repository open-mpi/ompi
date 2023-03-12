Using Parallel Debuggers to Debug Open MPI Applications
=======================================================

Open MPI supports the TotalView API for attaching to parallel processes, which
several parallel debuggers support (e.g., DDT, fx2).
As part of v1.2.4 (released in September 2007), Open MPI also supports
the TotalView API for viewing message queues in running MPI processes.

Debugging with TotalView
~~~~~~~~~~~~~~~~~~~~~~~~

This has changed with different releases of TotalView and Open MPI; it
is best to consult TotalView's documentation for how you should debug
Open MPI applications with TotalView.

.. note:: The integration of Open MPI message queue support is
   problematic with 64-bit versions of TotalView prior to v8.3:

   * The message queues views will be truncated.
   * Both the communicators and requests list will be incomplete.
   * Both the communicators and requests list may be filled with wrong
     values (such as an ``MPI_Send`` to the destination
     ``MPI_ANY_SOURCE``).

   There are two workarounds:

   * Use a 32-bit version of TotalView
   * Upgrade to TotalView v8.3

Debugging with DDT
~~~~~~~~~~~~~~~~~~

This has changed with different releases of DDT and Open MPI; it is
best to consult DDT's documentation for how you should debug Open MPI
applications with DDT.

