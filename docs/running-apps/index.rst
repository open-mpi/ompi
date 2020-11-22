.. _label-running-mpi-applications:

Running MPI applications
========================

Open MPI can launch MPI processes in a wide variety of environments,
but they can generally be broken down into two categories:

#. Scheduled environments: these are systems where a resource manager
   and/or scheduler are used to control access to the compute nodes.
   Popular resource managers include Slurm, PBS/Pro/Torque, and LSF.
#. Non-scheduled environments: these are systems where resource
   managers are not used.  Launches are typically local (e.g., on a
   single laptop or workstation) or via ``ssh`` (e.g., across a small
   number of nodes).

.. toctree::
   :maxdepth: 1

   quickstart
   pmix-and-prrte

   localhost
   ssh
   slurm
   lsf
   tm
   gridengine
