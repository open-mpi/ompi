Launching with PBS Pro, OpenPBS, and Torque
===========================================

Open MPI obtains its allocation from PBS Pro, OpenPBS, Torque, and
related resource managers, and launches processes through them.

Building with PBS support
-------------------------

PBS support is not built by default.  Enable it at configure time with
``--with-pbs``, optionally giving the PBS installation directory:

.. code-block:: sh

   shell$ ./configure --with-pbs=/opt/pbs ...

Configure looks for the ``pbs_tmrsh`` program in ``DIR/bin``, or in
``PATH`` if no directory is given.  If a directory is given and
``pbs_tmrsh`` is not found there, configure stops with an error.  PBS
support can only be built on Linux.

Verify PBS support
------------------

The :ref:`prte_info(1) <prrte:man1-prte_info>` command can be used to determine whether or not an
installed Open MPI includes PBS support:

.. code-block::

   shell$ prte_info | grep ras

If the Open MPI installation includes PBS support, you should see a
line similar to the one below.  The MCA version information varies
depending on which version of Open MPI is installed.

.. code-block::

       MCA ras: pbs (MCA v2.1.0, API v2.0.0, Component v3.0.0)

.. note:: PRRTE is the software layer that provides run-time
   environment support to Open MPI.  Open MPI typically hides most
   PMIx and PRRTE details from the end user, but this is one place
   that Open MPI is unable to hide the fact that PRRTE provides this
   functionality, not Open MPI.  Hence, users need to use the
   :ref:`prte_info(1) <prrte:man1-prte_info>` command to check for PBS support (not
   :ref:`ompi_info(1) <man1-ompi_info>`).

Launching
---------

The PBS support is selected when the ``PBS_ENVIRONMENT`` and
``PBS_JOBID`` environment variables are set, as they are inside a PBS
job.  Open MPI then obtains both the list of hosts and how many
processes to start on each host from the PBS node file
(``$PBS_NODEFILE``).  Hence, it is unnecessary to specify the
``--hostfile``, ``--host``, or ``-n`` options to :ref:`mpirun(1) <man1-mpirun>`.

Open MPI starts its run-time daemons on the other nodes of the job
with ``pbs_tmrsh`` instead of ``ssh``, so ``ssh`` is not required.
The absolute path of ``pbs_tmrsh`` found at configure time is used at
run time, so ``pbs_tmrsh`` must be installed at that path on the
compute nodes.

For example, with PBS Pro:

.. code-block:: sh

   # Allocate a PBS job with 4 nodes
   shell$ qsub -I -l select=4

   # Now run an Open MPI job on all the nodes allocated by PBS
   shell$ mpirun mpi-hello-world

With Torque, the allocation request is ``qsub -I -l nodes=4``.  Or,
if submitting a script:

.. code-block:: sh

   shell$ cat my_script.sh
   #!/bin/sh
   mpirun mpi-hello-world
   shell$ qsub -l select=4 my_script.sh

This will run the MPI processes on the nodes that were allocated by
PBS.

.. warning:: Do not modify ``$PBS_NODEFILE``!

   We've had reports from some sites that system administrators modify
   the ``$PBS_NODEFILE`` in each job according to local policies.
   This will currently cause Open MPI to behave in an unpredictable
   fashion.  As long as no new hosts are added to the hostfile, it
   *usually* means that Open MPI will incorrectly map processes to
   hosts, but in some cases it can cause Open MPI to fail to launch
   processes altogether.
