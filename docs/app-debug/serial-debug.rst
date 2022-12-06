Using Serial Debuggers to Debug Open MPI Applications
=====================================================

Since the GNU debugger (``gdb``) is fairly ubiquitiously
available, it is common to use a serial debugger for debugging Open
MPI applications.  Parallel debuggers are generally *better*, but
``gdb`` is free, and therefore quite common.

There are two common ways to use serial debuggers.

Attach to Individual Running MPI processes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can use multiple invocations of a serial debugger to individually
attach to every application process, or to a subset of application process. 

If you are using ``gdb``, then you would log onto the nodes where you want
to debug application processes and invoke gdb specifying the
gdb ``--pid`` option and the PID for each application process you want to
attach to.

You can use a similar technique to attach to application processes in
an application that was just invoked by :ref:`mpirun(1) <man1-mpirun>`.

An inelegant-but-functional technique commonly used with this method
is to insert the following code in your application where you want to
suspend the application process until the debugger is attached, such as
just after MPI is initialized.

.. code-block:: c

   {
       volatile int i = 0;
       char hostname[256];
       gethostname(hostname, sizeof(hostname));
       printf("PID %d on %s ready for attach\n", getpid(), hostname);
       fflush(stdout);
       while (0 == i)
           sleep(5);
   }

This code will output a line to stdout outputting the name of the host
where the process is running and the PID to attach to.  It will then
spin on the ``sleep(3)`` function forever waiting for you to attach
with a debugger.  Using ``sleep(3)`` as the inside of the loop means
that the processor won't be pegged at 100% while waiting for you to
attach.

Once you attach with a debugger, go up the function stack until you
are in this block of code (you'll likely attach during the
``sleep(3)``) then set the variable ``i`` to a nonzero value.  With
GDB, the syntax is:

.. code-block:: sh

   (gdb) set var i = 7

Then set a breakpoint after your block of code and continue execution
until the breakpoint is hit.  Now you have control of your live MPI
application and use of the full functionality of the debugger.

You  can even  add  conditionals to  only allow  this  "pause" in  the
application for specific MPI  processes (e.g., ``MPI_COMM_WORLD`` rank
0, or whatever process is misbehaving).

Use ``mpirun`` to Launch Separate Instances of Serial Debuggers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
    Open MPI will use ``ssh`` by default.
    But note that Open MPI closes the ``ssh``
    sessions when the MPI job starts for scalability reasons.  This
    means that the built-in SSH X forwarding tunnels will be shut down
    before the ``xterms`` can be launched.  Although it is possible to
    force Open MPI to keep its SSH connections active (to keep the X
    tunneling available), we recommend using non-SSH-tunneled X
    connections, if possible (see below).

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
