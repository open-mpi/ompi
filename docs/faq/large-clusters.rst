Large Clusters
==============

.. TODO How can I create a TOC just for this page here at the top?

/////////////////////////////////////////////////////////////////////////

.. important:: This page will ultimately go away.  All of the FAQ
               content is being folded in elsewhere in the docs.
               Please do not maintain/extend this page.

How do I reduce startup time for jobs on large clusters?
--------------------------------------------------------

.. JMS I have asked Ralph what to put there.

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

/////////////////////////////////////////////////////////////////////////

How do I reduce the time to wireup OMPI's out-of-band communication system?
---------------------------------------------------------------------------

.. JMS I have asked Ralph what to put there.  This section might be
   moot...?

Open MPI's run-time uses an *out-of-band* (OOB) communication
subsystem to pass messages during the launch, initialization, and
termination stages for the job. These messages allow ``mpirun`` to tell
its daemons what processes to launch, and allow the daemons in turn to
forward stdio to ``mpirun``, update ``mpirun`` on process status, etc.

The OOB uses TCP sockets for its communication, with each daemon
opening a socket back to ``mpirun`` upon startup. In a large cluster,
this can mean thousands of connections being formed on the node where
``mpirun`` resides, and requires that ``mpirun`` actually process all
these connection requests. ``mpirun`` defaults to processing
connection requests sequentially |mdash| so on large clusters, a
backlog can be created that can cause remote daemons to timeout
waiting for a response.

Fortunately, Open MPI provides an alternative mechanism for processing
connection requests that helps alleviate this problem. Setting the MCA
parameter ``oob_tcp_listen_mode`` to ``listen_thread`` causes
``mpirun`` to startup a separate thread dedicated to responding to
connection requests. Thus, remote daemons receive a quick response to
their connection request, allowing ``mpirun`` to deal with the message
as soon as possible.

.. error:: TODO This seems very out of date.  We should have content
           about PMIx instant on.

This parameter can be included in the default MCA parameter file,
placed in the user's environment, or added to the ``mpirun`` command
line.  See :ref:`this FAQ entry <label-running-setting-mca-param-values>`
for more details on how to set MCA parameters.
