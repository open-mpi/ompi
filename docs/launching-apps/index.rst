.. _label-running-mpi-applications:

Launching MPI applications
==========================

Open MPI can launch MPI processes in a wide variety of environments,
but they can generally be broken down into two categories:

#. Scheduled environments: these are systems where a resource manager
   and/or scheduler are used to control access to the compute nodes.
   Popular resource managers include Slurm, PBS/Pro/Torque, and LSF.
#. Non-scheduled environments: these are systems where resource
   managers are not used.  Launches are typically local (e.g., on a
   single laptop or workstation) or via ``ssh`` (e.g., across a small
   number of nodes).

Similar to many MPI implementations, Open MPI provides the commands
:ref:`mpirun(1) <man1-mpirun>` and :ref:`mpiexec(1) <man1-mpiexec>` to
launch MPI jobs.  This section deals with using these commands.

Note, however, that in Open MPI, :ref:`mpirun(1) <man1-mpirun>` and
:ref:`mpiexec(1) <man1-mpiexec>` are exactly identical.  Specifically,
they are symbolic links to a common back-end launcher command.

.. note:: The name of the back-end launcher command has changed over
          time (it used to be ``orterun``, it is now ``prte``).  This
          back-end name is largely irrelevant to the user.

The rest of this section usually refers only to :ref:`mpirun(1)
<man1-mpirun>`, even though the same discussions also apply to
:ref:`mpiexec(1) <man1-mpiexec>` (because they are both, in fact, the
same command).


.. toctree::
   :maxdepth: 1

   quickstart
   prerequisites
   pmix-and-prrte
   scheduling

   localhost
   ssh
   slurm
   lsf
   tm
   gridengine

   unusual
   troubleshooting
