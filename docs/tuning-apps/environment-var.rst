Environment variables set for MPI applications
==============================================

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
* ``OMPI_UNIVERSE_SIZE``: the number of :ref:`process slots
  <running-scheduling-slots-label>` allocated to this job. Note that
  this may be different than the number of processes in the job.
* ``OMPI_COMM_WORLD_NODE_RANK``: the relative rank of this process on
  this node looking across *all* jobs.
