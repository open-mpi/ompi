Reducing startup time for jobs
==============================

.. error:: TODO This whole section needs to be checked.

There are several ways to reduce the startup time on large
clusters. Some of them are described on this page. We continue to work
on making startup even faster, especially on the large clusters coming
in future years.

Open MPI |ompi_ver| is significantly faster and more robust than its
predecessors. We recommend that anyone running large jobs and/or on
large clusters make the upgrade to the |ompi_series| series.

Several major launch time enhancements have been made starting with the
v3.0 release. Most of these take place in the background |mdash| i.e., there
is nothing you (as a user) need do to take advantage of them. However,
there are a few that are left as options until we can assess any potential
negative impacts on different applications.

Some options are available when launching via ``mpirun`` or when launching using
the native resource manager launcher (e.g., ``srun`` in a Slurm environment).
These are activated by setting the corresponding MCA parameter, and include:

* Setting the ``pmix_base_async_modex`` MCA parameter will eliminate a
  global out-of-band collective operation during ``MPI_INIT``. This
  operation is performed in order to share endpoint information prior
  to communication. At scale, this operation can take some time and
  scales at best logarithmically. Setting the parameter bypasses the
  operation and causes the system to lookup the endpoint information
  for a peer only at first message. Thus, instead of collecting
  endpoint information for all processes, only the endpoint
  information for those processes this peer communicates with will be
  retrieved. The parameter is especially effective for applications
  with sparse communication patterns |mdash| i.e., where a process
  only communicates with a few other peers. Applications that use
  dense communication patterns (i.e., where a peer communicates
  directly to all other peers in the job) will probably see a negative
  impact of this option.

  .. note:: This option is only available in PMIx-supporting
            environments, or when launching via ``mpirun``

* The ``async_mpi_init`` parameter is automatically set to ``true``
  when the ``pmix_base_async_modex`` parameter has been set, but can
  also be independently controlled. When set to ``true``, this parameter
  causes ``MPI_Init`` to skip an out-of-band barrier operation at the end
  of the procedure that is not required whenever direct retrieval of
  endpoint information is being used.

* Similarly, the ``async_mpi_finalize`` parameter skips an out-of-band
  barrier operation usually performed at the beginning of
  ``MPI_FINALIZE``. Some transports (e.g., the ``usnic`` BTL) require this
  barrier to ensure that all MPI messages are completed prior to
  finalizing, while other transports handle this internally and thus
  do not require the additional barrier. Check with your transport
  provider to be sure, or you can experiment to determine the proper
  setting.
