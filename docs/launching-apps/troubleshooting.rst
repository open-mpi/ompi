Troubleshooting
===============

Launching MPI jobs can be a complex process that involves many moving parts.
This section attempts to provide solutions to some of the most common
problems users encounter.

Messages about missing symbols
------------------------------

Open MPI loads a lot of plugins (sometimes called "components" or
"modules") at run time.  Sometimes a plugin can fail to load because it
can't resolve all the symbols that it needs.  There are a few reasons
why this can happen.

* The plugin is for a different version of Open MPI.  :ref:`See this
  section <building-open-mpi-install-overwrite-label>` for an
  explanation of how Open MPI might try to open the "wrong" plugins.
* An application is trying to manually dynamically open ``libmpi`` in
  a private symbol space.  For example, if an application is not
  linked against ``libmpi``, but rather calls something like this:

  .. code-block:: c

     /* This is a Linux example |mdash| the issue is similar/the same on other
        operating systems */
     handle = dlopen("libmpi.so", RTLD_NOW | RTLD_LOCAL);

  This is due to some deep run-time linker voodoo |mdash| it is
  discussed towards the end of `this post to the Open MPI developer's
  list
  <https://www.mail-archive.com/devel@lists.open-mpi.org/msg07981.html>`_.
  Briefly, the issue is this:

  #. The dynamic library ``libmpi`` is opened in a "local" symbol
     space.
  #. ``MPI_INIT`` is invoked, which tries to open Open MPI's plugins.
  #. Open MPI's plugins rely on symbols in ``libmpi`` (and other Open
     MPI support libraries); these symbols must be resolved when the
     plugin is loaded.
  #. However, since ``libmpi`` was opened in a "local" symbol space,
     its symbols are not available to the plugins that it opens.
  #. Hence, the plugin fails to load because it can't resolve all of
     its symbols, and displays a warning message to that effect.

  The ultimate fix for this issue is a bit bigger than Open MPI,
  unfortunately |mdash| it's a POSIX issue (as briefly described in the
  devel mailing list posting, above).

  However, there are several common workarounds:

  * Dynamically open ``libmpi`` in a public / global symbol scope
    |mdash| not a private / local scope.  This will enable
    ``libmpi``'s symbols to be available for resolution when Open MPI
    dynamically opens its plugins.
  * If ``libmpi`` is opened as part of some underlying framework where
    it is not possible to change the private / local scope to a public
    / global scope, then dynamically open ``libmpi`` in a public /
    global scope before invoking the underlying framework.  This
    sounds a little gross (and it is), but at least the run-time
    linker is smart enough to not load ``libmpi`` twice |mdash| but it
    does keeps ``libmpi`` in a public scope.
  * Use the ``--disable-dlopen`` or ``--disable-mca-dso`` options to
    Open MPI's ``configure`` script (see this TODO NONEXISTENT FAQ entry
    for more details on these
    options).  These options slurp all of Open MPI's plugins up in to
    ``libmpi`` |mdash| meaning that the plugins physically reside in
    ``libmpi`` and will not be dynamically opened at run time.
  * Build Open MPI as a static library by configuring Open MPI with
    ``--disable-shared`` and ``--enable-static``.  This has the same
    effect as ``--disable-dlopen``, but it also makes ``libmpi.a`` (as
    opposed to a shared library).

Errors about missing libraries
------------------------------

When building Open MPI with the compilers that have libraries in
non-default search path locations, you may see errors about those
compiler's support libraries when trying to launch MPI applications if
their corresponding environments were not setup properly.

For example, you may see warnings similar to the following:

.. code-block:: sh

   # With the Intel compiler suite
   shell$ mpirun -n 1 --host node1.example.com mpi_hello
   prted: error while loading shared libraries: libimf.so: cannot open shared object file: No such file or directory
   --------------------------------------------------------------------------
   A daemon (pid 11893) died unexpectedly with status 127 while
   attempting to launch so we are aborting.
   ...more error messages...

   # With the PGI compiler suite
   shell$ mpirun -n 1 --host node1.example.com mpi_hello
   prted: error while loading shared libraries: libpgcc.so: cannot open shared object file: No such file or directory
   ...more error messages...

   # With the PathScale compiler suite
   shell$ mpirun -n 1 --host node1.example.com mpi_hello
   prted: error while loading shared libraries: libmv.so: cannot open shared object file: No such file or directory
   ...more error messages...

Specifically, Open MPI first attempts to launch a "helper" daemon
``prted`` on ``node1.example.com``, but it failed because one of
``prted``'s dependent libraries was not able to be found.  The
libraries shown above (``libimf.so``, ``libpgcc.so``, and
``libmv.so``) are specific to their compiler suites (Intel, PGI, and
PathScale, respectively).  As such, it is likely that the user did not
setup the compiler library in their environment properly on this node.

Double check that you have setup the appropriate compiler environment
on the target node, for both interactive and non-interactive logins.

.. note:: It is a common error to ensure that the compiler environment
          is setup properly for *interactive* logins, but not for
          *non-interactive* logins.

Here's an example of a user-compiled MPI application working fine
locally, but failing when invoked non-interactively on a remote node:

.. code-block:: sh

   # Compile a trivial MPI application
   head_node$ cd $HOME
   head_node$ mpicc mpi_hello.c -o mpi_hello

   # Run it locally; it works fine
   head_node$ ./mpi_hello
   Hello world, I am 0 of 1.

   # Run it remotely interactively; it works fine
   head_node$ ssh node2.example.com

   Welcome to node2.
   node2$ ./mpi_hello
   Hello world, I am 0 of 1.
   node2$ exit

   # Run it remotely *NON*-interactively; it fails
   head_node$ ssh node2.example.com $HOME/mpi_hello
   mpi_hello: error while loading shared libraries: libimf.so: cannot open shared object file: No such file or directory

In cases like this, check your shell script startup files and verify
that the appropriate compiler environment is setup properly for
non-interactive logins.

Problems when running across multiple hosts
-------------------------------------------

When you are able to run MPI jobs on a single host, but fail to run
them across multiple hosts, try the following:

#. Ensure that your launcher is able to launch across multiple hosts.
   For example, if you are using ``ssh``, try to ``ssh`` to each
   remote host and ensure that you are not prompted for a password.
   For example:

   .. code-block::

      shell$ ssh remotehost hostname
      remotehost

   If you are unable to launch across multiple hosts, check that your
   SSH keys are setup properly.  Or, if you are running in a managed
   environment, such as in a Slurm, Torque, or other job launcher,
   check that you have reserved enough hosts, are running in an
   allocated job, etc.

#. Ensure that your ``PATH`` and ``LD_LIBRARY_PATH`` are set correctly
   on each remote host on which you are trying to run.  For example,
   with ``ssh``:

   .. code-block::

      shell$ ssh remotehost env | grep -i path
      PATH=...path on the remote host...
      LD_LIBRARY_PATH=...LD library path on the remote host...

   If your ``PATH`` or ``LD_LIBRARY_PATH`` are not set properly, see
   :ref:`this section <running-prerequisites-label>` for
   the correct values.  Keep in mind that it is fine to have multiple
   Open MPI installations installed on a machine; the *first* Open MPI
   installation found by ``PATH`` and ``LD_LIBARY_PATH`` is the one
   that matters.

#. Run a simple, non-MPI job across multiple hosts.  This verifies
   that the Open MPI run-time system is functioning properly across
   multiple hosts.  For example, try running the ``hostname`` command:

   .. code-block::

      shell$ mpirun --host remotehost hostname
      remotehost
      shell$ mpirun --host remotehost,otherhost hostname
      remotehost
      otherhost

   If you are unable to run non-MPI jobs across multiple hosts, check
   for common problems such as:

   #. Check your non-interactive shell setup on each remote host to
      ensure that it is setting up the ``PATH`` and
      ``LD_LIBRARY_PATH`` properly.
   #.  Check that Open MPI is finding and launching the correct
       version of Open MPI on the remote hosts.
   #. Ensure that you have firewalling disabled between hosts (Open
      MPI opens random TCP and sometimes random UDP ports between
      hosts in a single MPI job).
   #. Try running with the ``plm_base_verbose`` MCA parameter at level
      10, which will enable extra debugging output to see how Open MPI
      launches on remote hosts.  For example:

      .. code-block::

         mpirun --mca plm_base_verbose 10 --host remotehost hostname``

#. Now run a simple MPI job across multiple hosts that does not
   involve MPI communications.  The ``hello_c`` program in the
   ``examples`` directory in the Open MPI distribution is a good
   choice.  This verifies that the MPI subsystem is able to initialize
   and terminate properly.  For example:

   .. code-block::

      shell$ mpirun --host remotehost,otherhost hello_c
      Hello, world, I am 0 of 1, (Open MPI VERSION, package: Open MPI jsquyres@example.com Distribution, ident: VERSION, DATE)
      Hello, world, I am 1 of 1, (Open MPI VERSION, package: Open MPI jsquyres@example.com Distribution, ident: VERSION, DATE)

   If you are unable to run simple, non-communication MPI jobs, this
   can indicate that your Open MPI installation is unable to
   initialize properly on remote hosts.  Double check your
   non-interactive login setup on remote hosts.

#. Now run a simple MPI job across multiple hosts that does does some
   simple MPI communications.  The ``ring_c`` program in the
   ``examples`` directory in the Open MPI distribution is a good
   choice.  This verifies that the MPI subsystem is able to pass MPI
   traffic across your network.  For example:

   .. code-block::

      shell$ mpirun --host remotehost,otherhost ring_c
      Process 0 sending 10 to 0, tag 201 (1 processes in ring)
      Process 0 sent to 0
      Process 0 decremented value: 9
      Process 0 decremented value: 8
      Process 0 decremented value: 7
      Process 0 decremented value: 6
      Process 0 decremented value: 5
      Process 0 decremented value: 4
      Process 0 decremented value: 3
      Process 0 decremented value: 2
      Process 0 decremented value: 1
      Process 0 decremented value: 0
      Process 0 exiting

   If you are unable to run simple MPI jobs across multiple hosts,
   this may indicate a problem with the network(s) that Open MPI is
   trying to use for MPI communications.  Try limiting the networks
   that it uses, and/or exploring levels 1 through 3 MCA parameters
   for the communications module that you are using.  For example, if
   you're using the TCP BTL, see the output of:

   .. code-block::

      ompi_info --level 3 --param btl tcp
