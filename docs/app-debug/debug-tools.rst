Parallel Debugging Tools
========================

There are two main categories of tools that can aid in
parallel debugging:

* **Debuggers:** Both serial and parallel debuggers are useful.  Serial
  debuggers are what most programmers are used to (e.g.,
  the GNU debugger, ``gdb``), while
  parallel debuggers can attach to all the individual processes in an
  MPI job simultaneously, treating the MPI application as a single
  entity.  This can be an extremely powerful abstraction, allowing the
  user to control every aspect of the MPI job, manually replicate race
  conditions, etc.

* **Profilers:** Tools that analyze your usage of MPI and display
  statistics and meta information about your application's run.  Some
  tools present the information "live" (as it occurs), while others
  collect the information and display it in a post mortem analysis.
