Large Clusters
==============

.. TODO How can I create a TOC just for this page here at the top?

/////////////////////////////////////////////////////////////////////////

How do I reduce startup time for jobs on large clusters?
--------------------------------------------------------

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

.. _faq-large-clusters-network-vs-local:

Where should I put my libraries: Network vs. local filesystems?
---------------------------------------------------------------

Open MPI itself doesn't really care where its libraries and plugins
are stored.  However, where they are stored does have an impact on
startup times, particularly for large clusters, which can be mitigated
somewhat through use of Open MPI's configuration options.

Startup times will always be minimized by storing the libraries and
plugins local to each node, either on local disk or in ramdisk. The
latter is sometimes problematic since the libraries do consume some
space, thus potentially reducing memory that would have been available
for MPI processes.

There are two main considerations for large clusters that need to
place the Open MPI libraries on networked file systems:

* While dynamic shared objects ("DSO") are more flexible, you
  definitely do *not* want to use them when the Open MPI libraries
  will be mounted on a network file system! Doing so will lead to
  significant network traffic and delayed start times, especially on
  clusters with a large number of nodes. Instead, be sure to configure
  your build with ``--disable-dlopen``. This will include the DSO's in
  the main libraries, resulting in much faster startup times.

* Many networked file systems use automount for user level
  directories, as well as for some locally administered system
  directories. There are many reasons why system administrators may
  choose to automount such directories. MPI jobs, however, tend to
  launch very quickly, thereby creating a situation wherein a large
  number of nodes will nearly simultaneously demand automount of a
  specific directory. This can overload NFS servers, resulting in
  delayed response or even failed automount requests.

  Note that this applies to both automount of directories containing
  Open MPI libraries as well as directories containing user
  applications. Since these are unlikely to be the same location,
  multiple automount requests from each node are possible, thus
  increasing the level of traffic.

/////////////////////////////////////////////////////////////////////////

Static vs. shared libraries?
----------------------------

It is perfectly fine to use either shared or static
libraries. Shared libraries will save memory when operating multiple
processes per node, especially on clusters with high numbers of cores
on a node, but can also take longer to launch on networked file
systems.

.. note:: Be sure to also see :ref:`this FAQ entry about network
          vs. local storage <faq-large-clusters-network-vs-local>` for
          suggestions on how to mitigate such problems.

/////////////////////////////////////////////////////////////////////////

How do I reduce the time to wireup OMPI's out-of-band communication system?
---------------------------------------------------------------------------

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
line.  See :ref:`this FAQ entry <faq-tuning-setting-mca-params-label>`
for more details on how to set MCA parameters.

/////////////////////////////////////////////////////////////////////////

I know my cluster's configuration - how can I take advantage of that knowledge?
-------------------------------------------------------------------------------

Clusters rarely change from day-to-day, and large clusters rarely
change at all.  If you know your cluster's configuration, there are
several steps you can take to both reduce Open MPI's memory footprint
and reduce the launch time of large-scale applications.  These steps
use a combination of build-time configuration options to eliminate
components |mdash| thus eliminating their libraries and avoiding
unnecessary component open/close operations |mdash| as well as
run-time MCA parameters to specify what modules to use by default for
most users.

One way to save memory is to avoid building components that will
actually never be selected by the system. Unless MCA parameters
specify which components to open, built components are always opened
and tested as to whether or not they should be selected for use. If
you know that a component can build on your system, but due to your
cluster's configuration will never actually be selected, then it is
best to simply configure OMPI to not build that component by using the
``--enable-mca-no-build`` configure option.

For example, if you know that your system will only utilize the
``ob1`` component of the PML framework, then you can ``no_build`` all
the others. This not only reduces memory in the libraries, but also
reduces memory footprint that is consumed by Open MPI opening all the
built components to see which of them can be selected to run.

In some cases, however, a user may optionally choose to use a
component other than the default.  For example, you may want to build
all of the PRRTE ``routed`` framework components, even though the vast
majority of users will simply use the default ``debruijn``
component.  This means you have to allow the system to build the other
components, even though they may rarely be used.

You can still save launch time and memory, though, by setting the
``routed=debruijn`` MCA parameter in the default MCA parameter file.
This causes OMPI to not open the other components during startup, but
allows users to override this on their command line or in their
environment so no functionality is lost |mdash| you just save some
memory and time.
