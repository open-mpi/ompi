Running MPI applications
========================

.. TODO How can I create a TOC just for this page here at the top?

/////////////////////////////////////////////////////////////////////////

.. _faq-running-mpi-apps-run-prereqs-label:

What prerequisites are necessary for running an Open MPI job?
-------------------------------------------------------------

In general, Open MPI requires that its executables are in your
``PATH`` on every node on which you will run and if Open MPI was
compiled was dynamic libraries (which is the default), the directory
where its libraries are located must be in your ``LD_LIBRARY_PATH`` on
every node.

For example, if Open MPI was installed with a prefix of ``/opt/openmpi``,
then the following should be in your ``PATH`` and ``LD_LIBRARY_PATH``

.. list-table::
   :header-rows: 1

   * - Environment variable
     - Value to add

   * - ``PATH``
     - ``/opt/openmpi/bin``

   * - ``LD_LIBRARY_PATH``
     - ``/opt/openmpi/lib``

.. error:: TODO Josh H points out that we might also want to mention
           ``OMPIHOME`` for PRRTE's ``.ini`` file here.  Leaving this
           as a future to-do item, since PRRTE's ``.ini`` file support
           does not exist yet.

Depending on your environment, you may need to set these values in
your shell startup files (e.g., ``.bashrc``, ``.cshrc``, etc.).

.. note:: There are exceptions to this rule |mdash| see :ref:`this FAQ
          entry <faq-running-mpi-apps-mpirun-prefix-label>` for a
          description of the ``--prefix`` option to ``mpirun``.

See :ref:`this FAQ entry
<faq-running-mpi-apps-adding-ompi-to-path-label>` for more details on
how to add Open MPI to your ``PATH`` and ``LD_LIBRARY_PATH``.

Additionally, Open MPI requires that jobs can be started on remote
nodes without any input from the keyboard.  For example, if using
``ssh`` as the remote agent, you must have your environment setup to
allow execution on remote nodes without entering a password or
passphrase.

/////////////////////////////////////////////////////////////////////////

What ABI guarantees does Open MPI provide?
------------------------------------------

:ref:`See this section for a description of Open MPI's versioning and
ABI scheme <version_numbers_section_label>`.  The short version is:

#. Open MPI is source code compatible across all versions.  This means
   that you can compile and link your compliant MPI application
   against :ref:`any version of Open MPI that supports the version of
   the MPI standard <faq_supported_systems_mpi_compliance_label>` to
   which your application was written.

#. Open MPI provided forward application binary interface (ABI)
   compatibility within a major series for MPI applications starting
   with v1.3.2.  Prior to that version, no ABI guarantees were
   provided.

#. Open MPI reserves the right to break ABI compatibility at new major
   release series.

/////////////////////////////////////////////////////////////////////////

Do I need a common filesystem on all my nodes?
----------------------------------------------

No, but it certainly makes life easier if you do.

A common environment to run Open MPI is in a "Beowulf"-class or
similar cluster (e.g., a bunch of 1U servers in a bunch of racks).
Simply stated, Open MPI can run on a group of servers or workstations
connected by a network.  As mentioned above, there are several
prerequisites, however (for example, you typically must have an
account on all the machines, you can or ``ssh`` between the
nodes without using a password, etc.).

Regardless of whether Open MPI is installed on a shared / networked
filesystem or independently on each node, it is usually easiest if
Open MPI is available in the same filesystem location on every node.
For example, if you install Open MPI to ``/opt/openmpi-$ver_current`` on
one node, ensure that it is available in ``/opt/openmpi-$ver_current``
on *all* nodes.

The :ref:`where to install
<building-open-mpi-installation-location-label>` FAQ question
contains some suggestions on where to install Open MPI.

/////////////////////////////////////////////////////////////////////////

.. _faq-running-mpi-apps-adding-ompi-to-path-label:

How do I add Open MPI to my ``PATH`` and ``LD_LIBRARY_PATH``?
-------------------------------------------------------------

Open MPI *must* be able to find its executables in your ``PATH``
on every node (if Open MPI was compiled as dynamic libraries, then its
library path must appear in ``LD_LIBRARY_PATH`` as well).  As such, your
configuration/initialization files need to add Open MPI to your ``PATH``
/ ``LD_LIBRARY_PATH`` properly.

How to do this may be highly dependent upon your local configuration;
you may need to consult with your local system administrator.  Some
system administrators take care of these details for you, some don't.
YMMV.  Some common examples are included below, however.

You must have at least a minimum understanding of how your shell works
to get Open MPI in your ``PATH`` / ``LD_LIBRARY_PATH`` properly.  Note
that Open MPI must be added to your ``PATH`` and ``LD_LIBRARY_PATH``
in the following situations:

#. When you login to an interactive shell

   If your interactive login environment is not configured properly,
   executables like ``mpicc`` will not be found, and it is typically
   obvious what is wrong.  The Open MPI executable directory can
   manually be added to the ``PATH``, or the user's startup files can
   be modified such that the Open MPI executables are added to the
   ``PATH`` every login.  This latter approach is preferred.

   All shells have some kind of script file that is executed at login
   time to set things like ``PATH`` and ``LD_LIBRARY_PATH`` and
   perform other environmental setup tasks.  This startup file is the
   one that needs to be edited to add Open MPI to the ``PATH`` and
   ``LD_LIBRARY_PATH``. Consult the manual page for your shell for
   specific details (some shells are picky about the permissions of
   the startup file, for example).  The table below lists some common
   shells and the startup files that they read/execute upon login:

   .. list-table::
      :header-rows: 1
      :widths: 10 90

      * - Shell
        - Interactive login startup files

      * - ``bash``
        - ``.bash_profile`` if it exists, or ``.bash_login`` if it
          exists, or ``.profile`` if it exists

          (in that order).  Note that some Linux distributions
          automatically come with

          ``.bash_profile`` scripts for users that automatically
          execute ``.bashrc`` as well.

          Consult the ``bash(1)`` man page for more information.

      * - ``zsh``
        - ``.zshrc`` followed by ``.zshenv``

      * - ``sh`` (or Bash

          named ``sh``)
        - ``.profile``

      * - ``csh``
        - ``.cshrc`` followed by ``.login``

      * - ``tcsh``
        - ``.tcshrc`` if it exists, ``.cshrc`` if it does not, followed by
          ``.login``

#. When you login to non-interactive shells on remote nodes

   If your non-interactive remote environment is not configured
   properly, executables like ``mpirun`` will not function properly,
   and it can be somewhat confusing to figure out.

   The startup files in question here are the ones that are
   automatically executed for a non-interactive login on a remote node
   (e.g., ``ssh othernode ps``).  Note that not all shells support
   this, and that some shells use different files for this than listed
   for interactive logins.  Some shells will supersede non-interactive
   login startup files with files for interactive logins.  That is,
   running non-interactive login startup file *may* automatically
   invoke interactive login startup file.  The following table lists
   some common shells and the startup file that is automatically
   executed, either by Open MPI or by the shell itself:

   .. list-table::
      :header-rows: 1
      :widths: 10 90

      * - Shell
        - Non-interactive login startup files

      * - ``bash``
        - ``.bashrc`` if it exists

      * - ``zsh``
        - ``.zshrc`` followed by ``.zshenv``

      * - ``sh`` (or Bash

          named ``sh``)
        - This shell does not execute any file automatically,

          so Open MPI will execute the ``.profile`` script

          before invoking Open MPI executables on remote nodes

      * - ``csh``
        - ``.cshrc``

      * - ``tcsh``
        - ``.tcshrc`` if it exists, ``.cshrc`` if it does not

/////////////////////////////////////////////////////////////////////////

.. _faq-running-mpi-apps-mpirun-prefix-label:

What if I can't modify my ``PATH`` and/or ``LD_LIBRARY_PATH``?
--------------------------------------------------------------

There are some situations where you cannot modify the ``PATH`` or
``LD_LIBRARY_PATH`` |mdash| e.g., some ISV applications prefer to hide
all parallelism from the user, and therefore do not want to make the
user modify their shell startup files.  Another case is where you want
a single user to be able to launch multiple MPI jobs simultaneously,
each with a different MPI implementation.  Hence, setting shell
startup files to point to one MPI implementation would be problematic.

In such cases, you have two options:

#. Use ``mpirun``'s ``--prefix`` command line option (described
   below).
#. Modify the wrapper compilers to include directives to include
   run-time search locations for the Open MPI libraries.

``mpirun``'s ``--prefix`` command line option takes as an argument the
top-level directory where Open MPI was installed.  While relative
directory names are possible, they can become ambiguous depending on
the job launcher used; using absolute directory names is strongly
recommended.

For example, say that Open MPI was installed into
``/opt/openmpi-$ver_current``.  You would use the ``--prefix`` option
thusly:

.. code-block::

   shell$ mpirun --prefix /opt/openmpi-$ver_current -n 4 a.out

This will prefix the ``PATH`` and ``LD_LIBRARY_PATH`` on both the
local and remote hosts with ``/opt/openmpi-$ver_current/bin`` and
``/opt/openmpi-$ver_current/lib``, respectively.  This is *usually*
unnecessary when using resource managers to launch jobs (e.g., Slurm,
Torque, etc.) because they tend to copy the entire local environment
|mdash| to include the ``PATH`` and ``LD_LIBRARY_PATH`` |mdash| to
remote nodes before execution.  As such, if ``PATH`` and
``LD_LIBRARY_PATH`` are set properly on the local node, the resource
manager will automatically propagate those values out to remote nodes.
The ``--prefix`` option is therefore usually most useful in
``ssh``-based environments (or similar).

It is possible to make this the default behavior by passing to
``configure`` the flag ``--enable-mpirun-prefix-by-default``.  This
will make ``mpirun`` behave exactly the same as ``mpirun --prefix
$prefix ...``, where ``$prefix`` is the value given to ``--prefix``
in ``configure``.

Finally, note that specifying the absolute pathname to ``mpirun`` is
equivalent to using the ``--prefix`` argument.  For example, the
following is equivalent to the above command line that uses
``--prefix``:

.. code-block::

   shell$ /opt/openmpi-$ver_current/bin/mpirun -n 4 a.out

/////////////////////////////////////////////////////////////////////////

How do I launch Open MPI parallel jobs?
---------------------------------------

Similar to many MPI implementations, Open MPI provides the commands
``mpirun`` and ``mpiexec`` to launch MPI jobs.  Several of the
questions in this FAQ category deal with using these commands.

Note, however, that in Open MPI, ``mpirun`` and ``mpiexec`` are
exactly identical.  Specifically, they are symbolic links to a common
back-end launcher command.

.. note:: The name of the back-end launcher command has changed over
          time (it used to be ``orterun``, it is now ``prte``).  This
          back-end name is largely irrelevant to the user.

The rest of this FAQ usually refers only to ``mpirun``, even though
the same discussions also apply to ``mpiexec`` (because they are both,
in fact, the same command).

/////////////////////////////////////////////////////////////////////////

.. _faq-running-mpi-apps-spmd-label:

How do I run a simple SPMD MPI job?
-----------------------------------

Open MPI provides both ``mpirun`` and ``mpiexec`` commands.  A simple way
to start a single program, multiple data (SPMD) application in
parallel is:

.. code-block::

   shell$ mpirun -n 4 my_parallel_application

This starts a four-process parallel application, running four copies
of the executable named ``my_parallel_application``.

The ``rsh`` starter component accepts the ``--hostfile`` option (and
its synonym, the ``--machinefile`` option) to indicate on which hosts
to start the processes:

.. code-block::

   shell$ cat my_hostfile
   host01.example.com
   host02.example.com
   shell$ mpirun --hostfile my_hostfile -n 4 my_parallel_application

This command will launch one copy of ``my_parallel_application`` on
each of ``host01.example.com`` and ``host02.example.com``.

More information about the ``--hostfile`` option, and hostfiles in
general, is available in :ref:`this FAQ entry
<faq-running-mpi-apps-mpirun-hostfile-label>`.

Note, however, that not all environments require a hostfile.  For
example, Open MPI will automatically detect when it is running in
batch / scheduled environments (such as Slurm, PBS/Torque, SGE,
LoadLeveler), and will use host information provided by those systems.

Also note that if using a launcher that requires a hostfile and no
hostfile is specified, all processes are launched on the local host.

/////////////////////////////////////////////////////////////////////////

How do I run an MPMD MPI job?
-----------------------------

Both the ``mpirun`` and ``mpiexec`` commands support multiple program,
multiple data (MPMD) style launches, either from the command line or
from a file.  For example:

.. code-block::

   shell$ mpirun -n 2 a.out : -n 2 b.out

This will launch a single parallel application, but the first two
processes will be instances of the ``a.out`` executable, and the
second two processes will be instances of the ``b.out`` executable.
In MPI terms, this will be a single ``MPI_COMM_WORLD``, but the
``a.out`` processes will be ranks 0 and 1 in ``MPI_COMM_WORLD``, while
the ``b.out`` processes will be ranks 2 and 3 in ``MPI_COMM_WORLD``.

``mpirun`` (and ``mpiexec``) can also accept a parallel application
specified in a file instead of on the command line.  For example:

.. code-block::

   shell$ mpirun --app my_appfile

where the file ``my_appfile`` contains the following:

.. code-block:: sh

   # Comments are supported; comments begin with #
   # Application context files specify each sub-application in the
   # parallel job, one per line.  The first sub-application is the 2
   # a.out processes:
   -n 2 a.out
   # The second sub-application is the 2 b.out processes:
   -n 2 b.out

This will result in the same behavior as running ``a.out`` and ``b.out``
from the command line.

Note that ``mpirun`` and ``mpiexec`` are identical in command-line options
and behavior; using the above command lines with ``mpiexec`` instead of
``mpirun`` will result in the same behavior.

/////////////////////////////////////////////////////////////////////////

How do I specify the hosts on which my MPI job runs?
----------------------------------------------------

There are three general mechanisms:


#. The ``--hostfile`` option to ``mpirun``.

   Use this option to specify a list of hosts on which to run.  Note
   that for compatibility with other MPI implementations,
   ``--machinefile`` is a synonym for ``--hostfile``.  See :ref:`this
   FAQ entry <faq-running-mpi-apps-mpirun-hostfile-label>` for more
   information about the ``--hostfile`` option.

#. The ``--host`` option to ``mpirun``.

   This option can be used to specify a list of hosts on which to run
   on the command line.  See :ref:`this FAQ entry
   <faq-running-mpi-apps-mpirun-host-label>` for more information
   about the ``--host`` option.

#. Running in a scheduled environment.

   If you are running in a scheduled environment (e.g., in a Slurm,
   Torque, or LSF job), Open MPI will automatically get the lists of
   hosts from the scheduler.

.. important:: The specification of hosts using any of the above
               methods has nothing to do with the network interfaces
               that are used for MPI traffic.  The list of hosts is
               *only* used for specifying which hosts on which to
               launch MPI processes.

/////////////////////////////////////////////////////////////////////////

.. _faq-running-mpi-aps-diagnose-multi-host-problems-label:

How can I diagnose problems when running across multiple hosts?
---------------------------------------------------------------

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
   :ref:`this FAQ entry <faq-running-mpi-apps-run-prereqs-label>` for
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
      Hello, world, I am 0 of 1, (Open MPI v$ver_current, package: Open MPI jsquyres@example.com Distribution, ident: $ver_current, DATE)
      Hello, world, I am 1 of 1, (Open MPI v$ver_current, package: Open MPI jsquyres@example.com Distribution, ident: $ver_current, DATE)

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

/////////////////////////////////////////////////////////////////////////

.. Missing libraries FAQ items addressing errors of the form:

   prted: error while loading shared libraries: libimf.so: cannot open shared
       object file: No such file or directory

   Compiler => Compiler library linked to orted

   $compilers``"Intel"`` = "libimf.so";
   $compilers``"PGI"`` = "libpgc.so";
   $compilers``"PathScale"`` = "libmv.so";

I get errors about missing libraries.  What should I do?
--------------------------------------------------------

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

/////////////////////////////////////////////////////////////////////////

Can I run non-MPI programs with ``mpirun`` / ``mpiexec``?
---------------------------------------------------------

Yes.

For example:

.. code-block::

   shell$ mpirun -n 2 --host a,b uptime

This will launch a copy of the Unix command ``uptime`` on the hosts ``a``
and ``b``.

Other questions in the FAQ section deal with the specifics of the
``mpirun`` command line interface; suffice it to say that it works
equally well for MPI and non-MPI applications.

/////////////////////////////////////////////////////////////////////////

Can I run GUI applications with Open MPI?
-----------------------------------------

Yes, but it will depend on your local setup and may require additional
setup.

In short: you will need to have graphics forwarding (e.g., X11
forwarding) enabled from the remote processes to the display where you
want output to appear.  In a secure environment, you can simply allow
all X requests to be shown on the target display and set the
``DISPLAY`` environment variable in all MPI processes' environments to
the target display, perhaps something like this:

.. code-block::

   shell$ hostname
   my_desktop.secure-cluster.example.com
   shell$ xhost +
   shell$ mpirun -n 4 -x DISPLAY=my_desktop.secure-cluster.example.com a.out

However, this technique is not generally suitable for unsecure
environments (because it allows anyone to read and write to your
display).  A slightly more secure way is to only allow X connections
from the nodes where your application will be running:

.. code-block::

   shell$ hostname
   my_desktop.secure-cluster.example.com
   shell$ xhost +compute1 +compute2 +compute3 +compute4
   compute1 being added to access control list
   compute2 being added to access control list
   compute3 being added to access control list
   compute4 being added to access control list
   shell$ mpirun -n 4 -x DISPLAY=my_desktop.secure-cluster.example.com a.out

(assuming that the four nodes you are running on are ``compute1``
through ``compute4``).

Other methods are available, but they involve sophisticated X
forwarding through ``mpirun`` and are generally more complicated than
desirable.

/////////////////////////////////////////////////////////////////////////

Can I run ncurses-based / curses-based / applications with funky input schemes with Open MPI?
---------------------------------------------------------------------------------------------

Maybe.  But probably not.

Open MPI provides fairly sophisticated stdin / stdout / stderr
forwarding.  However, it does not work well with curses, ncurses,
readline, or other sophisticated I/O packages that generally require
direct control of the terminal.

Every application and I/O library is different |mdash| you should try to
see if yours is supported.  But chances are that it won't work.

Sorry.  :-(

/////////////////////////////////////////////////////////////////////////

What other options are available to ``mpirun``?
-----------------------------------------------

``mpirun`` supports the ``--help`` option which provides a usage
message and a summary of the options that it supports.  It should be
considered the definitive list of what options are provided.

Several notable options are:

* ``--hostfile``: Specify a hostfile for launchers (such as the
  ``rsh`` launcher) that need to be told on which hosts to start
  parallel applications.  Note that for compatibility with other MPI
  implementations, *--machinefile* is a synonym for ``--hostfile``.
* ``--host``: Specify a host or list of hosts to run on (see
  :ref:`this FAQ entry for more details
  <faq-running-mpi-apps-mpirun-host-label>`).
* ``-n``: Indicate the number of processes to start.
* ``--mca``: Set MCA parameters (see the :doc:`Run-Time Tuning FAQ
  category </faq/tuning/>` for more details).
* ``--wdir DIRECTORY``: Set the working directory of the started
  applications.  If not supplied, the current working directory is
  assumed (or ``$HOME``, if the current working directory does not
  exist on all nodes).
* ``-x ENV_VARIABLE_NAME``: The name of an environment variable to
  export to the parallel application.  The ``-x`` option can be
  specified multiple times to export multiple environment variables to
  the parallel application.

/////////////////////////////////////////////////////////////////////////

.. _faq-running-mpi-apps-mpirun-hostfile-label:

How do I use the ``--hostfile`` option to ``mpirun``?
-----------------------------------------------------

.. error:: TODO For cross reference, this is the PRRTE man page
           section about ``--hostfile``:
           https://github.com/openpmix/prrte/blob/b70a6f1a8d424e396c40c999a656b04e76cc0f91/src/tools/prte/prte-map.1.md?plain=1#L236
           A subsequent commit removed this markdown file; the commit
           message refers to moving the markdown file to another git
           repo, but I didn't chase down where it went.

The ``--hostfile`` option to ``mpirun`` takes a filename that lists
hosts on which to launch MPI processes.

.. important:: The hosts listed in a hostfile have *nothing* to do
               with which network interfaces are used for MPI
               communication.  They are *only* used to specify on
               which hosts to launch MPI processes.

Hostfiles are simple text files with hosts specified, one per line.
Each host can also specify a default and maximum number of *slots* to
be used on that host (i.e., the maximum number of processes that will
be launched on that node).  Comments are also supported, and blank
lines are ignored.  For example:

.. code-block::

   # This is an example hostfile.  Comments begin with #.
   #
   # Since no slots are specified, the number of slots defaults to the
   # number of processor cores available on the machine.
   foo.example.com

   # We want to allow launching a maximum of 2 processes on this host
   # (e.g., potentially because it has two processor cores):
   bar.example.com slots=2

Slots are discussed in much more detail :ref:`in this FAQ entry
<faq-running-mpi-apps-mpirun-scheduling-label>`.

Hostfiles works in two different ways:

#. *Exclusionary:* If a list of hosts to run on has been provided by
   another source (e.g., by a hostfile or a batch scheduler such as
   Slurm, PBS/Torque, SGE, etc.), the hosts provided by the hostfile
   must be in the already-provided host list.  If the
   hostfile-specified nodes are *not* in the already-provided host
   list, ``mpirun`` will abort without launching anything.

   In this case, hostfiles act like an exclusionary filter |mdash|
   they limit the scope of where processes will be scheduled from the
   original list of hosts to produce a final list of hosts.

   For example, say that a scheduler job contains hosts ``node01``
   through ``node04``.  If you run:

   .. code-block::

      shell$ cat my_hosts
      node03
      shell$ mpirun -n 1 --hostfile my_hosts hostname

   This will run a single copy of ``hostname`` on the host ``node03``.

   However, presuming your job was allocated only to ``node03`` and
   you run the following:

   .. code-block::

      shell$ cat my_hosts
      node17
      shell$ mpirun -n 1 --hostfile my_hosts hostname

   This is an error (because ``node17`` is not allocated to your job),
   and ``mpirun`` will abort.

   Finally, note that in exclusionary mode, processes will *only* be
   executed on the hostfile-specified hosts, If this ends up causing
   an oversubscription situation, ``mpirun`` will abort by default.

#. *Inclusionary:* If a list of hosts has *not* been provided by
   another source, then the hosts provided by the ``--hostfile``
   option will be used as the original and final host list.

   In this case, ``--hostfile`` acts as an inclusionary agent; all
   ``--hostfile``-supplied hosts become available for scheduling
   processes.  For example (assume that you are *not* in a scheduling
   environment where a list of nodes is being transparently supplied):

   .. code-block::

      shell$ cat my_hosts
      node01.example.com slots=1
      node02.example.com slots=1
      node03.example.com slots=1
      shell$ mpirun -n 3 --hostfile my_hosts hostname

   This will launch a single copy of ``hostname`` on the hosts
   ``node01.example.com``, ``node02.example.com``, and
   ``node03.example.com``.

Note, too, that ``--hostfile`` is essentially a per-application switch.
Hence, if you specify multiple applications (as in an MPMD job),
``--hostfile`` can be specified multiple times:

.. code-block::

   shell$ cat hostfile_1
   node01.example.com
   shell$ cat hostfile_2
   node02.example.com
   shell$ mpirun -n 1 --hostfile hostfile_1 hostname : -n 1 --hostfile hostfile_2 uptime
   node01.example.com
    06:11:45 up 1 day,  2:32,  0 users,  load average: 21.65, 20.85, 19.84

Notice that ``hostname`` was launched on ``node01.example.com`` and
``uptime`` was launched on ``node02.example.com``.

/////////////////////////////////////////////////////////////////////////

.. _faq-running-mpi-apps-mpirun-host-label:

How do I use the ``--host`` option to ``mpirun``?
-------------------------------------------------

The ``--host`` option to ``mpirun`` takes a comma-delimited list of
hosts on which to run.  For example:

.. code-block::

   shell$ mpirun -n 3 --host a,b,c hostname

Will launch *one* copy of ``hostname`` on each of hosts ``a``, ``b``,
and ``c``.  Specifically: each host defaults to 1 slot, unless
specified by the ``:N`` suffix.  For example:

.. code-block::

   shell$ mpirun --host a,b:2,c:3 hostname

Will launch one copy of ``hostname`` on ``a``, two copies of
``hostname`` on ``b``, and three copies of ``hostname`` and ``c``.

Slots are discussed in much more detail :ref:`in this FAQ entry
<faq-running-mpi-apps-mpirun-scheduling-label>`.

.. important:: The hosts specified by the ``--host`` option have
               *nothing* to do with which network interfaces are used
               for MPI communication.  They are *only* used to specify
               on which hosts to launch MPI processes.

``--host`` works in two different ways:

#. *Exclusionary:* If a list of hosts to run on has been provided by
   another source (e.g., by a hostfile or a batch scheduler such as
   Slurm, PBS/Torque, SGE, etc.), the hosts provided by the ``--host``
   option must be in the already-provided host list.  If the
   ``--host``-specified nodes are *not* in the already-provided host
   list, ``mpirun`` will abort without launching anything.

   In this case, the ``--host`` option acts like an exclusionary
   filter |mdash| it limits the scope of where processes will be
   scheduled from the original list of hosts to produce a final list
   of hosts.

   For example, say that the hostfile ``my_hosts`` contains the hosts
   ``node1`` through ``node4``.  If you run:

   .. code-block::

      shell$ mpirun -n 1 --hostfile my_hosts --host node3 hostname

   This will run a single copy of ``hostname`` on the host ``node3``.
   However, if you run:

   .. code-block::

      shell$ mpirun -n 1 --hostfile my_hosts --host node17 hostname

   This is an error (because ``node17`` is not listed in
   ``my_hosts``); ``mpirun`` will abort.

   Finally, note that in exclusionary mode, processes will *only* be
   executed on the ``--host``-specified hosts.  If this ends up
   causing an oversubscription situation, ``mpirun`` will abort by
   default.

#. *Inclusionary:* If a list of hosts has *not* been provided by
   another source, then the hosts provided by the ``--host`` option
   will be used as the original and final host list.

   In this case, ``--host`` acts as an inclusionary agent; all
   ``--host``-supplied hosts become available for scheduling
   processes.  For example (assume that you are *not* in a scheduling
   environment where a list of nodes is being transparently supplied):

   .. code-block::

      shell$ mpirun -n 3 --host a,b,c hostname

   This will launch a single copy of ``hostname`` on the hosts ``a``,
   ``b``, and ``c``.

Note, too, that ``--host`` is essentially a per-application switch.
Hence, if you specify multiple applications (as in an MPMD job),
``--host`` can be specified multiple times:

.. code-block::

   shell$ mpirun -n 1 --host a hostname : -n 1 --host b uptime

This will launch ``hostname`` on host ``a`` and ``uptime`` on host ``b``.

/////////////////////////////////////////////////////////////////////////

.. _faq-running-mpi-apps-slots-label:

What are "slots"?
-----------------

*Slots* are Open MPI's representation of how many processes can be
launched on a given host.

Open MPI maintains the number of slots for each host in a given
parallel job, and |mdash| by default |mdash| will not let you launch
more processes on a host than it has slots.

.. important:: It is common to set the number of slots on a host to be
               less than or equal to the number of processor cores on
               that host.

               **But it is important to realize that Open MPI's concept
               of slots is actually unrelated to the number of
               physical processor cores on a host.**

               Specifically: the number of slots on a host can be less
               than, equal to, or more than the number of processor
               cores on a host.

If you wish to run more processes on a host than it has slots,
:ref:`see the FAQ entry on oversubscription
<faq-running-mpi-apps-oversubscribing-label>`.

/////////////////////////////////////////////////////////////////////////

.. _faq-running-mpi-apps-default-slots-label:

How are the number of slots calculated?
---------------------------------------

The number of slots on a host depends on a few factors:

#. If the host is specified by a job scheduler (e.g., Slurm,
   PBS/Torque, etc.), the job scheduler specifies the number of slots
   for that host.

#. If the host is specified in a hostfile:

   #. If the ``slots`` parameter is specified, that value is used for
      the number of slots on that host.
   #. Otherwise:

      #. If ``--map-by :HWTCPUS`` was specified, the number of slots
         defaults to the number of hardware threads on that host.
      #. Otherwise, the number of slots defaults to the number of
         processor cores on that host.

#. If the host is specified via the ``--host`` command line option:

   #. If the ``:N`` suffix is specified, ``N`` is used for the number
      of slots on that host.
   #. Otherwise, the number of slots defaults to 1.
   #. If the same host name is specified multiple times, the slots
      value for that host is increased by ``N`` if ``:N`` is
      specified, or increased by 1 if ``:N`` is not specified.

.. caution:: The exact scheme used to determine the number of slots
             has varied between different major versions of Open MPI.
             The scheme described above is relevant for Open MPI
             |ompi_series|.

Max slot counts, however, are rarely specified by schedulers.  The max
slot count for each node will default to "infinite" if it is not
provided (meaning that Open MPI will oversubscribe the node if you ask
it to |mdash| see more on oversubscribing in :ref:`this FAQ entry
<faq-running-mpi-apps-oversubscribing-label>`).

.. error:: TODO Ralph: do we still have the concept of "max slots"?
           Issue is open:
           https://github.com/openpmix/prrte/issues/770.

Here are some examples, all from unscheduled environments:

#. Use a hostfile and specify the ``slots`` parameter.

   .. code-block:: sh

      shell$ cat my-hostfile
      node01.example.come slots=4
      shell$ mpirun --hostfile my-hostfile hostname
      node01
      node01
      node01
      node01

   This launched 4 processes because ``slots=4`` was specified in the
   hostfile.

#. Use a hostfile and do *not* specify the ``slots`` parameter (assume
   that ``node01.example.com`` has 2 processor cores):

   .. code-block:: sh

      shell$ cat my-hostfile
      node01.example.come
      shell$ mpirun --hostfile my-hostfile hostname
      node01
      node01

   This launched 2 processes because ``slots`` was not specified, and
   ``node02`` has 2 processor cores.

#. Use ``--host``:

   .. code-block:: sh

      shell$ mpirun --host node01.example.com hostname
      node01

   This launched 1 processes because ``--host`` with no ``:N`` suffix
   increments the slot count for that host by 1.

#. Use ``--host`` with a ``:N`` suffix:

   .. code-block:: sh

      shell$ mpirun --host node01.example.com:2 hostname
      node01
      node01

   This launched 2 processes because ``:2`` was specified on the
   command line.

#. Use ``--host`` with a ``:N`` suffix, and mention the host multiple times:

   .. code-block:: sh

      shell$ mpirun --host node01.example.com:2,node01.example.com hostname
      node01
      node01
      node01

   This launched 3 processes because ``:2`` was specified on the
   command line, and then ``node01.example.com`` was specified an
   additional time, incrementing the slot count for that host to 3.

/////////////////////////////////////////////////////////////////////////

.. _faq-running-mpi-apps-mpirun-scheduling-label:

How do I control how my processes are scheduled across hosts?
-------------------------------------------------------------

The short version is that if you are not oversubscribing your hosts
(i.e., trying to run more processes than slots available on that
host), scheduling is pretty simple and occurs either on a by-slot or
by-node round robin schedule.  If you're oversubscribing, the issue
gets much more complicated |mdash| keep reading.

The more complete answer is: Open MPI schedules processes to nodes by
asking two questions from each application on the ``mpirun`` command
line:

#. *How many* processes should be launched?
#. *Where* should those processes be launched?

The "how many" question is directly answered with the ``-n`` switch
to ``mpirun``.  If ``-n`` is not specified on the ``mpirun`` command
line, its value is the sum of the slots on all the nodes.

The "where" question is a little more complicated, and depends on
three factors:

#. The final node list (e.g., after ``-hostname`` / ``--host``
   exclusionary or inclusionary processing)
#. The scheduling policy (which applies to all applications in a
   single job)
#. The default and maximum number of slots on each host

.. error:: TODO Ralph: do we still have the concept of "max slots"?
           Issue is open:
           https://github.com/openpmix/prrte/issues/770.

Open MPI currently supports two scheduling policies: by slot and by
node:

#. *By slot:* This is the default scheduling policy, but can also be
   explicitly requested by using either the ``--map-by slot`` option
   to ``mpirun`` or by setting the MCA parameter
   ``rmaps_default_mapping_policy`` to the string ``slot``.

   In this mode, Open MPI will schedule processes on a node until all
   of its default slots are exhausted before proceeding to the next
   node.  In MPI terms, this means that Open MPI tries to maximize the
   number of adjacent ranks in ``MPI_COMM_WORLD`` on the same host
   without oversubscribing that host.

   For example:

   .. code-block::

      shell$ cat my-hosts
      node0 slots=2 max_slots=20
      node1 slots=2 max_slots=20
      shell$ mpirun --hostfile my-hosts -n 8 --map-by slot hello | sort
      Hello World I am rank 0 of 8 running on node0
      Hello World I am rank 1 of 8 running on node0
      Hello World I am rank 2 of 8 running on node1
      Hello World I am rank 3 of 8 running on node1
      Hello World I am rank 4 of 8 running on node0
      Hello World I am rank 5 of 8 running on node0
      Hello World I am rank 6 of 8 running on node1
      Hello World I am rank 7 of 8 running on node1

#. *By node:* This policy can be requested either by using the
   ``--map-by node`` option to ``mpirun`` or by setting the MCA parameter
   ``rmaps_default_mapping_policy`` to the string "node".

   In this mode, Open MPI will schedule a single process on each node
   in a round-robin fashion (looping back to the beginning of the node
   list as necessary) until all processes have been scheduled.  Nodes
   are skipped once their default slot counts are exhausted.

   For example:

   .. code-block::

      shell$ cat my-hosts
      node0 slots=2 max_slots=20
      node1 slots=2 max_slots=20
      shell$ mpirun --hostname my-hosts -n 8 --map-by node hello | sort
      Hello World I am rank 0 of 8 running on node0
      Hello World I am rank 1 of 8 running on node1
      Hello World I am rank 2 of 8 running on node0
      Hello World I am rank 3 of 8 running on node1
      Hello World I am rank 4 of 8 running on node0
      Hello World I am rank 5 of 8 running on node1
      Hello World I am rank 6 of 8 running on node0
      Hello World I am rank 7 of 8 running on node1

In both policies, if the default slot count is exhausted on all nodes
while there are still processes to be scheduled, Open MPI will trigger
an oversubscription condition.

If ``:OVERSUBSCRIBE`` is added as a modifier to the ``--map-by``
option (e.g., ``mpirun --map-by node:OVERSUBSCRIBE ...`` -- :ref:`see
this FAQ item <faq-running-mpi-apps-oversubscribing-label>` for more
details), Open MPI will continue to loop through the list of nodes
again and try to schedule one more process to each node until all
processes are scheduled.  Nodes are skipped in this process if their
maximum slot count is exhausted.  If the maximum slot count is
exhausted on all nodes while there are still processes to be
scheduled, Open MPI will abort without launching any processes.

If ``:OVERSUBSCRIBE`` is *not* specified and an oversubscription
condition occurs, Open MPI will abort without launching any processes.

/////////////////////////////////////////////////////////////////////////

.. _faq-running-mpi-apps-oversubscribing-label:

Can I oversubscribe nodes (run more processes than processors)?
---------------------------------------------------------------

Yes.  But it very much matters *how* you do it.

Specifically: it is critical that Open MPI *knows* that you are
oversubscribing the node, or **severe** performance degradation can
result.

.. important:: Here is a good general rule to follow: **never specify
               a number of slots that is more than the available
               number of processors.**

For example, if you want to run 4 processes on a host with 2 processor
cores, then indicate that you only have 2 slots but want to run 4
processes.  For example:

.. code-block:: sh

   # In a hostfile, the number of slots will default to the number of
   # processor cores on the host
   shell$ cat my-hostfile
   localhost
   shell$ mpirun -n 4 --hostfile my-hostfile a.out

Specifically: we strongly suggest that you do **NOT** have a hostfile
that contains ``slots=4`` (because there are only two available
processor cores).

That being said, the above command will fail, because you are trying
to run 4 processes but there are only 2 slots available.  You must
specifically tell Open MPI that it is ok to oversubscribe via
``--map-by :OVERSUBSCRIBE``:

.. code-block:: sh

   shell$ cat my-hostfile
   # For the purposes of this example, explicitly tell Open MPI
   # that we have 2 slots on the host.
   localhost slots=2
   shell$ mpirun -n 4 --hostfile my-hostfile --map-by :OVERSUBSCRIBE a.out

The reason you should tell Open MPI whether you're oversubscribing or
not (i.e., never specify a ``slots`` value more than the number of
processor cores available) is because Open MPI basically runs its
message passing progression engine in two modes: *aggressive* and
*degraded*.

#. *Degraded:* When Open MPI thinks that it is in an oversubscribed
   mode (i.e., more processes are running than there are processor
   cores available), MPI processes will automatically run in
   *degraded* mode and frequently yield the processor to its peers,
   thereby allowing all processes to make progress.

   .. note:: Be sure to see :ref:`this FAQ entry
             <faq-tuning-using-paffinity-label>` that describes how
             degraded mode affects processor and memory
             affinity.

#. *Aggressive:* When Open MPI thinks that it is in an exactly- or
   under-subscribed mode (i.e., the number of running processes is
   equal to or less than the number of available processor cores), MPI
   processes will automatically run in *aggressive* mode, meaning that
   they will never voluntarily give up the processor to other
   processes.  With some network transports, this means that Open MPI
   will spin in tight loops attempting to make message passing
   progress, effectively causing other processes to not get any CPU
   cycles (and therefore never make any progress).

For example, on a node with a two processor cores:

.. code-block::

   shell$ cat my-hostfile
   localhost slots=4
   shell$ mpirun -n 4 --hostfile my-hostfile a.out

This would cause all 4 MPI processes to run in *aggressive* mode
because Open MPI thinks that there are 4 available processor cores to
use.  This is actually a lie (there are only 2 processor core |mdash|
not 4), and can cause extremely bad performance.

/////////////////////////////////////////////////////////////////////////

Can I force Agressive or Degraded performance modes?
----------------------------------------------------

Yes.

The MCA parameter ``mpi_yield_when_idle`` controls whether an MPI
process runs in Aggressive or Degraded performance mode.  Setting it
to 0 forces Aggressive mode; setting it to 1 forces Degraded mode (see
:ref:`this FAQ entry <faq-tuning-setting-mca-params-label>` to see how
to set MCA parameters).

Note that this value *only* affects the behavior of MPI processes when
they are blocking in MPI library calls.  It does not affect behavior
of non-MPI processes, nor does it affect the behavior of a process
that is not inside an MPI library call.

Open MPI normally sets this parameter automatically (see :ref:`this
FAQ entry <faq-running-mpi-apps-oversubscribing-label>` for details).
Users are cautioned against setting this parameter unless you are
really, absolutely, positively sure of what you are doing.

/////////////////////////////////////////////////////////////////////////

.. _faq-running-mpi-apps-totalview-label:

How do I run with the TotalView parallel debugger?
--------------------------------------------------

This has changed with different releases of TotalView and Open MPI; it
is best to consult TotalView's documentation for how you should debug
Open MPI applications with TotalView.

/////////////////////////////////////////////////////////////////////////

.. _faq-running-mpi-apps-ddt-label:

How do I run with the DDT parallel debugger?
--------------------------------------------

This has changed with different releases of DDT and Open MPI; it is
best to consult DDT's documentation for how you should debug Open MPI
applications with DDT.

/////////////////////////////////////////////////////////////////////////

How do I dynamically load libmpi at runtime?
--------------------------------------------

If you want to load a the shared library ``libmpi`` explicitly at
runtime either by using ``dlopen()`` from C/C ++ or something like the
``ctypes`` package from Python, some extra care is required.  The
default configuration of Open MPI uses ``dlopen()`` internally to load
its support components.  These components rely on symbols available in
``libmpi``.  In order to make the symbols in ``libmpi`` available to
the components loaded by Open MPI at runtime, ``libmpi`` must be
loaded with the ``RTLD_GLOBAL`` option.

In C/C++, this option is specified as the second parameter to the
POSIX ``dlopen(3)`` function.

When using ``ctypes`` with Python, this can be done with the second
(optional) parameter to ``CDLL()``.  For example (shown below in Mac OS
X, where Open MPI's shared library name ends in ``.dylib``; other
operating systems use other suffixes, such as ``.so``):

.. code-block:: python

   from ctypes import *

   mpi = CDLL('libmpi.0.dylib', RTLD_GLOBAL)

   f = pythonapi.Py_GetArgcArgv
   argc = c_int()
   argv = POINTER(c_char_p)()
   f(byref(argc), byref(argv))
   mpi.MPI_Init(byref(argc), byref(argv))

   # Your MPI program here

   mpi.MPI_Finalize()

Other scripting languages should have similar options when dynamically
loading shared libraries.

/////////////////////////////////////////////////////////////////////////

What MPI environment variables exist?
-------------------------------------

Open MPI provides the following environment variables that will be
defined on every MPI process:

* ``OMPI_COMM_WORLD_SIZE``: the number of processes in this process's
  MPI_COMM_WORLD
* ``OMPI_COMM_WORLD_RANK``: the MPI rank of this process in
  MPI_COMM_WORLD
* ``OMPI_COMM_WORLD_LOCAL_SIZE``: the number of ranks from this job
  that are running on this node.
* ``OMPI_COMM_WORLD_LOCAL_RANK``: the relative rank of this process on
  this node within its job. For example, if four processes in a job
  share a node, they will each be given a local rank ranging from 0 to
  3.
* ``OMPI_UNIVERSE_SIZE``: the number of process slots allocated to
  this job. Note that this may be different than the number of
  processes in the job.
* ``OMPI_COMM_WORLD_NODE_RANK``: the relative rank of this process on
  this node looking across *all* jobs.
