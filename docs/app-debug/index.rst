.. Open MPI Application Debugging

Debugging Open MPI Parallel Applications
========================================

Debugging a serial applications includes solving problems like
logic errors, uninitialized variables, storage overlays and timing
problems.

Debugging a parallel application can be further complicated
by problems that can include additional race conditions and aysynchronous
events, as well as understanding execution of multiple application
processes running simultaneously.

This section of the documentation describes some techniques that can
be useful for parallel debugging. This section also describes some
tools that can be useful as well as some Open MPI runtime options
that can aid debugging.

.. toctree::
   :maxdepth: 1

   debug-tools
   debug-options
   serial-debug
   parallel-debug
   lost-output
   memchecker
   valgrind
   mpir-tools
