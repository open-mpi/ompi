Launching with SSH
==================

When launching Open MPI jobs in a non-scheduled environment, ``ssh``
is typically used to launch commands on remote nodes.  As listed in
the :doc:`quick start section </launching-apps/quickstart>`,
successfully launching MPI applications with ``ssh`` requires the
following:

#. You must be able to non-interactively login |mdash| without
   entering a password or passphrase |mdash| to all remote nodes from
   all remotes nodes.
#. Open MPI's executables must be findable (e.g., in your ``PATH``).
#. Open MPI's libraries must be findable (e.g., in your
   ``LD_LIBRARY_PATH``).

Specifying the hosts for an MPI job
-----------------------------------

There are three mechanisms for specifying the hosts that an MPI job will run on:

#. The ``--hostfile`` option to :ref:`mpirun(1) <man1-mpirun>`.

   Use this option to specify a list of hosts on which to run.  Note
   that for compatibility with other MPI implementations,
   ``--machinefile`` is a synonym for ``--hostfile``.
   See :ref:`this section <running-scheduling-hostfile-option-label>` for more
   information about the ``--hostfile`` option.

#. The ``--host`` option to :ref:`mpirun(1) <man1-mpirun>`.

   This option can be used to specify a list of hosts on which to run
   on the command line.
   See :ref:`this section <running-scheduling-host-option-label>` for more
   information about the ``--host`` option.

#. Running in a scheduled environment.

   If you are running in a scheduled environment (e.g., in a Slurm,
   Torque, or LSF job), Open MPI will automatically get the lists of
   hosts from the scheduler.  See the next subsections for details about
   launching MPI jobs in supported scheduled environements.

.. important:: The specification of hosts using any of the above
               methods has nothing to do with the network interfaces
               that are used for MPI traffic.  The list of hosts is
               *only* used for specifying which hosts on which to
               launch MPI processes.

Non-interactive ``ssh`` logins
------------------------------

SSH keys must be setup such that the following can be executed without
being prompted for password or passphrase:

.. code-block:: sh

   shell$ ssh othernode echo hello
   hello
   shell$

Consult instructions and tutorials from around the internet to learn
how to setup SSH keys.  Try Google search terms like "passwordless
SSH" or "SSH key authentication".

For simplicity, it may be desirable to configure your SSH keys
without passphrases.  This adds some risk, however (e.g., if your SSH
keys are compromised).  But it simplifies your SSH setup because you
will not need to use ``ssh-agent``.  Evaluate the risk level you are
comfortable with.

.. important:: Open MPI uses a tree-based pattern to launch processes
   on remote nodes.  This means that Open MPI must be able to
   non-interactively login |mdash| without being prompted for password
   or passphrase |mdash| *to any node* in the host list *from any
   node* in the host list.

   It may *not* be sufficient to only setup an SSH key from the node
   where you are invoking :ref:`mpirun(1) <man1-mpirun>` to all other
   nodes.

If you have a shared ``$HOME`` filesystem between your nodes, you can
setup a single SSH key that is used to login to all nodes.

Finding Open MPI executables and libraries
------------------------------------------

Once Open MPI is able to use ``ssh`` to invoke executables on a remote
node, it must be able to find its helper executables and shared
libraries on that remote node.

If Open MPI is installed in a system-level folder (e.g., in
``/usr/bin``), Open MPI will likely be able to find its executables
and libraries on the remote node with no additional assistance.

If, however, Open MPI is installed into a path that is not searched by
default, you will need to provide assistance so that Open MPI can find
its executables and libraries.

.. important:: For simplicity, it is *strongly* recommended that you
   install Open MPI in the same location on all nodes in your job.
   See the :doc:`Installation location section
   </installing-open-mpi/installation-location>` for more details.

You can do this in one of two ways.

Use "prefix" behavior
^^^^^^^^^^^^^^^^^^^^^

.. note:: "Prefix" behavior is only available with :ref:`mpirun(1)
   <man1-mpirun>`; it is not available via resource manager direct
   launch mechanisms.  However, this section is about using ``ssh`` to
   launch MPI jobs, which means that there is no resource manager, and
   therefore there is no direct launch mechanism available.

When "prefix" behavior is enabled, Open MPI will automatically set the
``$PATH`` and ``$LD_LIBRARY_PATH`` on remote nodes before executing
remote commands.

.. important:: Open MPI assumes that the installation ``prefix``,
   ``bindir``, and ``libdir`` are the same on the remote node as they
   are on the local node.  If they are not, *then you should not use
   the "prefix" behavior.*

You can enable "prefix" behavior in one of three ways:

#. Use an absolute path name to invoke :ref:`mpirun(1) <man1-mpirun>`.

   .. code-block:: sh

      shell$ $HOME/my-openmpi/bin/mpirun --hostfile my-hostfile.txt mpi-hello-world

   Simply using the absolute path name to :ref:`mpirun(1)
   <man1-mpirun>` tells Open MPI to enable "prefix" mode.


#. Use the ``--prefix`` option to :ref:`mpirun(1) <man1-mpirun>`.

  .. code-block:: sh

     shell$ $HOME/my-openmpi/bin/mpirun --hostfile my-hostfile.txt \
         --prefix $HOME/my-openmpi \
         mpi-hello-world

   The ``-prefix`` option takes a single argument: the prefix path to
   use for the bindir and libdir on the remote node.

#. Configure Open MPI with ``--enable-mpirun-prefix-by-default``.

   If Open MPI is built this way, :ref:`mpirun(1) <man1-mpirun>` will
   always enable "prefix" behavior.

Set the ``PATH`` and ``LD_LIBRARY_PATH`` in your shell startup files
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Consider the case where Open MPI was configured with:

.. code-block:: sh

   shell$ ./configure --prefix=$HOME/my-openmpi ...

In this cause, Open MPI will be installed into ``$HOME/my-openmpi``.
This path is almost certainly not in any system-default search paths,
so it must be added to the ``$PATH`` and ``$LD_LIBRARY_PATH``
environment variables.

Specifically: the goal is that the following non-interactive commands
must be able to execute without error:

.. code-block:: sh

   # First, ensure that this command returns the correct ompi_info
   # instance (i.e., $HOME/my-openmpi/bin/ompi_info).
   shell$ ssh remotenode which ompi_info
   /home/myusername/my-openmpi/bin/ompi_info

   # Next, ensure that you can run that ompi_info command without
   # error
   shell$ ssh remotenode ompi_info

   # ... lots of output ...

Ensure that you do not see any errors about libraries that cannot be
found.

All shells have some kind of script file that is executed at login
time perform environmental setup tasks.  This startup file is the one
that needs to be edited to:

#. Add Open MPI's executable path (which is likely ``$prefix/bin``, or
   ``$HOME/my-openmpi/bin`` in this example) to the ``$PATH``
   environment variable.
#. Add Open MPI's library path (which is likely ``$prefix/lib``, or
   ``$HOME/my-openmpi/lib`` in this example) to the
   ``$LD_LIBRARY_PATH`` environment variable.

You probably want to add Open MPI's libraries to the *front* of
``$PATH`` and ``$LD_LIBRARY_PATH`` to ensure that this Open MPI
installation's files are found *first*.

Consult the manual page for your shell for specific details (some
shells are picky about the permissions of the startup file, for
example).  The list below contains some common shells and the startup
files that they read/execute upon login:

.. list-table::
   :header-rows: 1

   * - Shell
     - Non-interactive login
     - Interactive login

   * - ``bash`` or ``zsh``
     - ``$HOME/.bashrc`` if it exists.
     - #. ``$HOME/.bash_profile`` if it exists, or
       #. ``$HOME/.bash_login`` if it exists, or
       #. ``$HOME/.profile`` if it exists (in that order).

       Note that some Linux distributions automatically come
       with ``$HOME/.bash_profile`` scripts for users that
       automatically execute ``$HOME/.bashrc`` as well. Consult the
       bash man page for more information.

   * - ``sh``
     - This shell does not execute any file automatically, so Open MPI
       will execute the ``$HOME/.profile`` script before invoking Open
       MPI executables on remote nodes
     - ``$HOME/.profile``

   * - ``csh``
     - ``$HOME/.cshrc``
     - ``$HOME/.cshrc`` followed by ``$HOME/.login``

   * - ``tcsh``
     - #. ``$HOME/.tcshrc`` if it exists, or
       #. ``$HOME/.cshrc`` if it does not
     - #. ``$HOME/.tcshrc`` if it exists, or
       #. ``$HOME/.cshrc`` if it does not

       Afterwards, execute ``$HOME/.login``
