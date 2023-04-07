Libraries
=========

Location: network vs. local filesystems
---------------------------------------

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
  will be mounted on a network file system that is shared to a large
  cluster!  Doing so will lead to significant network traffic and
  delayed start times, especially on clusters with a large number of
  nodes.  Instead, be sure to :ref:`configure your build
  <building-ompi-cli-options-diable-dlopen-label>` with
  ``--disable-dlopen``.  This will include the DSO's in the main
  libraries, resulting in much faster startup times.

  .. note:: As of the Open MPI v5.0.x series, ``--disable-dlopen`` is
            now the default.

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

Static vs. shared
-----------------

It is perfectly fine to use either shared or static
libraries. Shared libraries will save memory when operating multiple
processes per node, especially on clusters with high numbers of cores
on a node, but can also take longer to launch on networked file
systems.
