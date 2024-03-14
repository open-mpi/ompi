.. _running-prerequisites-label:

Prerequisites
=============

Successful launch of Open MPI jobs requires the ability to
find Open MPI's executables and shared libraries on all nodes at run
time.

In general, Open MPI's ``mpicc`` sets the paths to these with the ``runpath`` linker
option when an application is compiled and linked.
If the Open MPI executables and libraries can be found via in system-default
search paths or the ``runpath`` embedded in an application (i.e., without the
user needing to set or modify ``PATH`` or ``LD_LIBRARY_PATH``), then
nothing additional needs to be done.  However, if the Open MPI binaries
are not found, the instructions below may be used to locate them.

If the ``runpath`` embedded in an application is not sufficient to locate
the Open MPI binaries, then the ``PATH`` and ``LD_LIBRARY_PATH`` can be used
to locate them.
In general, Open MPI requires that its executables are in your
``PATH`` on every node on which you will run and if Open MPI was
compiled as dynamic libraries (which is the default), the directory
where its libraries are located must be in your ``LD_LIBRARY_PATH`` on
every node.
For example:

* If Open MPI is installed in ``/usr/bin`` and ``/usr/lib``), that is
  usually sufficient, and the user does not need to do anything extra.
* If Open MPI is installed in a location that is not searched by
  default, users may need to add ``$prefix/bin`` to their ``PATH`` and
  ``$libdir`` (which defaults to ``$prefix/lib``) to their
  ``LD_LIBRARY_PATH``.

  .. caution:: In scheduled environments, ensuring Open MPI's
               executables and libraries can be found on the node that
               executes :ref:`mpirun(1) <man1-mpirun>` may be
               sufficient.

               In non-scheduled environments, users may need to set
               the ``PATH`` and ``LD_LIBRARY_PATH`` environment
               variables in their shell setup files (e.g.,
               ``$HOME/.bashrc``) so that non-interactive
               ``ssh``-based logins will be able to find the Open MPI
               executables and libraries.

               For example, if Open MPI was installed with a prefix of
               ``/opt/openmpi``, then the following should be in your
               ``PATH`` and ``LD_LIBRARY_PATH``

               .. list-table::
                  :header-rows: 1

                  * - Environment variable
                    - Value to add

                  * - ``PATH``
                    - ``/opt/openmpi/bin``

                  * - ``LD_LIBRARY_PATH``
                    - ``/opt/openmpi/lib``

               Depending on your environment, you may need to set these
               values in your shell startup files (e.g., ``.bashrc``,
               ``.cshrc``, etc.).

Additionally, Open MPI requires that jobs can be started on remote
nodes without any input from the keyboard.  For example, if using
``ssh`` as the remote agent, you must have your environment setup to
allow execution on remote nodes without entering a password or
passphrase.

Adding Open MPI to ``PATH`` and ``LD_LIBRARY_PATH``
---------------------------------------------------

Open MPI *must* be able to find its executables in your ``PATH``
on every node (if Open MPI was compiled as dynamic libraries, then its
library path must appear in ``LD_LIBRARY_PATH`` as well).  As such, your
configuration/initialization files need to add Open MPI to your ``PATH``
/ ``LD_LIBRARY_PATH`` properly.

How to do this may be highly dependent upon your local configuration;
you may need to consult with your local system administrator.  Some
system administrators take care of these details for you, some don't.
Some common examples are included below, however.

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
   properly, executables like :ref:`mpirun(1) <man1-mpirun>` will not function properly,
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


Using the ``--prefix`` option with mpirun
-----------------------------------------

If users are unable to add the relevant directories to ``PATH`` and
``LD_LIBRARY_PATH``, the :ref:`mpirun(1) <man1-mpirun>` ``--prefix``
option *may* be sufficient.

There are some situations where you cannot modify the ``PATH`` or
``LD_LIBRARY_PATH`` |mdash| e.g., some ISV applications prefer to hide
all parallelism from the user, and therefore do not want to make the
user modify their shell startup files.  Another case is where you want
a single user to be able to launch multiple MPI jobs simultaneously,
each with a different MPI implementation.  Hence, setting shell
startup files to point to one MPI implementation would be problematic.

In such cases, you have two options:

#. Use the :ref:`mpirun(1) <man1-mpirun>` ``--prefix`` command line
   option (described below).
#. Modify the wrapper compilers to include directives to include
   run-time search locations for the Open MPI libraries.

:ref:`mpirun(1) <man1-mpirun>`'s ``--prefix`` command line option takes as an argument the
top-level directory where Open MPI was installed.  While relative
directory names are possible, they can become ambiguous depending on
the job launcher used; using absolute directory names is strongly
recommended.

For example, say that Open MPI was installed into
``/opt/openmpi-VERSION``.  You would use the ``--prefix`` option
thusly:

.. code-block::

   shell$ mpirun --prefix /opt/openmpi-VERSION -n 4 a.out

This will prefix the ``PATH`` and ``LD_LIBRARY_PATH`` on both the
local and remote hosts with ``/opt/openmpi-VERSION/bin`` and
``/opt/openmpi-VERSION/lib``, respectively.  This is *usually*
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
will make :ref:`mpirun(1) <man1-mpirun>` behave exactly the same as
``mpirun --prefix $prefix ...``, where ``$prefix`` is the value given
to ``--prefix`` in ``configure``.

Finally, note that specifying the absolute pathname to :ref:`mpirun(1)
<man1-mpirun>` is equivalent to using the ``--prefix`` argument.  For
example, the following is equivalent to the above command line that
uses ``--prefix``:

.. code-block::

   shell$ /opt/openmpi-VERSION/bin/mpirun -n 4 a.out

.. error:: TODO Josh H points out that we might also want to mention
           ``OMPIHOME`` for PRRTE's ``.ini`` file here.  Leaving this
           as a future to-do item, since PRRTE's ``.ini`` file support
           does not exist yet.
