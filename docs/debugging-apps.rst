Parallel debugging
==================

Debugging in parallel is even complex than debugging in serial.  By
definition, there are multiple OS processes working in concert with
each other, which can make it complicated to determine how the overall
application is behaving (or misbehaving).

Tools
-----

There are two main categories of tools that can aid in parallel
debugging:

#. **Debuggers**: Both serial and parallel debuggers are useful.
   Serial debuggers are what most programmers are used to (e.g.,
   ``gdb``), while parallel debuggers can attach to all the individual
   processes in an MPI job simultaneously, treating the MPI
   application as a single entity.  This can be an extremely powerful
   abstraction, allowing the user to control every aspect of the MPI
   job, manually replicate race conditions, etc.

#. **Profilers**: Tools that analyze your usage of MPI and display
   statistics and meta information about your application's run.  Some
   tools present the information "live" (as it occurs), while others
   collect the information and display it in a post mortem analysis.

Both freeware and commercial solutions are available for each kind of
tool.

Debuggers
---------

Since the GNU parallel debugger (``gdb``) is fairly ubiquitiously
available, it is common to use a serial debugger for debugging Open
MPI applications.  Per the above discussion, parallel debuggers are
generally *better*, but ``gdb`` is free, and therefore quite common.

There are two common ways to use serial debuggers.

Attach to individual MPI processes after they are running
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For example, launch your MPI application as normal with ``mpirun``.
Then login to the node(s) where your application is running and use
the ``--pid`` option to ``gdb`` to attach to your application.

An inelegant-but-functional technique commonly used with this method
is to insert the following code in your application where you want to
attach:

.. code-block:: c

   // ...
   {
       volatile int i = 0;
       char hostname[256];
       gethostname(hostname, sizeof(hostname));
       printf("PID %d on %s ready for attach\n", getpid(), hostname);
       fflush(stdout);
       while (0 == i)
           sleep(5);
   }
   // ...

This code will output a line to stdout outputting the name of the host
where the process is running and the PID to attach to.  It will then
spin on the ``sleep()`` function forever waiting for you to attach
with a debugger.  Using ``sleep()`` as the inside of the loop means
that the processor won't be pegged at 100% while waiting for you to
attach.

Once you attach with a debugger, go up the function stack until you
are in this block of code (you'll likely attach during the
``sleep()``) then set the variable ``i`` to a nonzero value.  With
GDB, the syntax is:

.. code-block:: sh

   (gdb) set var i = 7

Then set a breakpoint after your block of code and continue execution
until the breakpoint is hit.  Now you have control of your live MPI
application and use of the full functionality of the debugger.

You  can even  add  conditionals to  only allow  this  "pause" in  the
application for specific MPI  processes (e.g., ``MPI_COMM_WORLD`` rank
0, or whatever process is misbehaving).

Use ``mpirun`` to launch separate instances of serial debuggers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This technique launches a separate window for each MPI process in
``MPI_COMM_WORLD``, each one running a serial debugger (such as
``gdb``) that will launch and run your MPI application.  Having a
separate window for each MPI process can be quite handy for low
process-count MPI jobs, but requires a bit of setup and configuration
that is outside of Open MPI to work properly.  A naive approach would
be to assume that the following would immediately work:

.. code-block:: sh

    shell$ mpirun -n 4 xterm -e gdb my_mpi_application

If running on a personal computer, this will probably work.  You can
also use `tmpi <https://github.com/Azrael3000/tmpi>`_ to launch the
debuggers in separate ``tmux`` panes instead of separate ``xterm``
windows, which has the advantage of synchronizing keyboard input
between all debugger instances.

Unfortunately, the ``tmpi`` or ``xterm`` approaches likely *won't*
work on an computing cluster. Several factors must be considered:

#.  What launcher is Open MPI using?  In an ``ssh``-based environment,
    Open MPI will default to using ``ssh``. But note that Open MPI
    closes the ``ssh`` sessions after the MPI job starts for
    scalability reasons.  This means that the built-in SSH X
    forwarding tunnels will be shut down before the ``xterms`` can be
    launched.  Although it is possible to force Open MPI to keep its
    SSH connections active (to keep the X tunneling available), we
    recommend using non-SSH-tunneled X connections, if possible (see
    below).

#. In non-``ssh`` environments (such as when using resource managers),
   the environment of the process invoking ``mpirun`` may be copied to
   all nodes.  In this case, the ``DISPLAY`` environment variable may
   not be suitable.

#. Some operating systems default to disabling the X11 server from
   listening for remote/network traffic.  For example, see `this post
   on the Open MPI user's mailing list
   <https://www.open-mpi.org/community/lists/users/2008/02/4995.php>`_
   describing how to enable network access to the X11 server on Fedora
   Linux.

#. There may be intermediate firewalls or other network blocks that
   prevent X traffic from flowing between the hosts where the MPI
   processes (and ``xterm``) are running and the host connected to
   the output display.

The easiest way to get remote X applications (such as ``xterm``) to
display on your local screen is to forego the security of SSH-tunneled
X forwarding.  In a closed environment such as an HPC cluster, this
may be an acceptable practice (indeed, you may not even have the
option of using SSH X forwarding if SSH logins to cluster nodes are
disabled), but check with your security administrator to be sure.

If using non-encrypted X11 forwarding is permissible, we recommend the
following:

#. For each non-local host where you will be running an MPI process,
   add it to your X server's permission list with the ``xhost``
   command.  For example:

   .. code-block:: sh

      shell$ cat my_hostfile
      inky
      blinky
      stinky
      clyde
      shell$ for host in `cat my_hostfile` ; do xhost +host ; done

#. Use the ``-x`` option to ``mpirun`` to export an appropriate
   DISPLAY variable so that the launched X applications know where to
   send their output.  An appropriate value is *usually* (but not
   always) the hostname containing the display where you want the
   output and the ``:0`` (or ``:0.0``) suffix.  For example:

   .. code-block:: sh

      shell$ hostname
      arcade.example.come
      shell$ mpirun -n 4 --hostfile my_hostfile \
          -x DISPLAY=arcade.example.com:0 xterm -e gdb my_mpi_application

   .. warning:: X traffic is fairly "heavy" |mdash| if you are
                operating over a slow network connection, it may take
                some time before the ``xterm`` windows appear on your
                screen.

#. If your ``xterm`` supports it, the ``-hold`` option may be useful.
   ``-hold`` tells ``xterm`` to stay open even when the application
   has completed.  This means that if something goes wrong (e.g.,
   ``gdb`` fails to execute, or unexpectedly dies, or ...), the
   ``xterm`` window will stay open, allowing you to see what happened,
   instead of closing immediately and losing whatever error message
   may have been output.

#. When you have finished, you may wish to disable X11 network
   permissions from the hosts that you were using.  Use ``xhost``
   again to disable these permissions:

   .. code-block:: sh

      shell$ for host in `cat my_hostfile` ; do xhost -host ; done

.. note:: ``mpirun`` will not complete until all the ``xterm``
          instances are complete.



Open MPI internal debugging aids
--------------------------------

Open MPI has a series of :ref:`MCA parameters <label-run-time-tuning>`
for the MPI layer itself that are designed to help with debugging.
MPI-level MCA parameters can be displayed by invoking the following
command:

.. code-block:: sh

   # Use "--level 9" to see all the MCA parameters
   # (the default is "--level 1"):
   shell$ ompi_info --param mpi all --level 9

Here is a summary of the debugging parameters for the MPI layer:

* ``mpi_param_check``: If set to true, and when Open MPI is compiled
  with parameter checking enabled (the default), the parameters to
  each MPI function can be passed through a series of correctness
  checks.  Problems such as passing illegal values (e.g., NULL or
  ``MPI_DATATYPE_NULL`` or other "bad" values) will be discovered at
  run time and an MPI exception will be invoked, which |mdash| by
  default |mdash| prints a short message and aborts the entire MPI
  job.  If set to false, these checks are disabled, marginally
  increasing performance.

* ``mpi_show_handle_leaks``: If set to true, Open MPI will display
  lists of any MPI handles that were not freed before ``MPI_FINALIZE``
  (e.g., communicators, datatypes, requests, etc.)

* ``mpi_no_free_handles``: If set to true (any positive value), do not
  actually free MPI objects when their corresponding MPI "free"
  function is invoked (e.g., do not free underlying communicator
  objects when ``MPI_COMM_FREE`` is invoked).  This can be helpful in
  tracking down applications that accidentally continue to use MPI
  handles after they have been freed.

* ``mpi_show_mca_params``: If set to true (any positive value), show a
  list of all MCA parameters and their values during ``MPI_INIT``.
  This can be quite helpful for reproducibility of MPI applications.

* ``mpi_show_mca_params_file``: If set to a non-empty value, and if
  the value of ``mpi_show_mca_params`` is true, then output the list
  of MCA parameters to the filename value.  If this parameter is an
  empty value, the list is sent to ``stderr``.

* ``mpi_abort_delay``: If nonzero, print out an identifying message
  when :ref:`MPI_Abort(3) <mpi_abort>` is invoked showing the hostname
  and PID of the process that invoked :ref:`MPI_Abort(3) <mpi_abort>`,
  and then delay that many seconds before exiting.  A negative value
  means to delay indefinitely.  This allows a user to manually come in
  and attach a debugger when an error occurs.  Remember that the
  default MPI error handler |mdash| ``MPI_ERRORS_ABORT`` |mdash|
  invokes :ref:`MPI_Abort(3) <mpi_abort>`, so this parameter can be
  useful to discover problems identified by ``mpi_param_check``.

* ``mpi_abort_print_stack``: If nonzero, print out a stack trace (on
  supported systems) when :ref:`MPI_Abort(3) <mpi_abort>` is invoked.
