Advice for System Administrators
================================

Several members of the Open MPI team have strong system
administrator backgrounds; we recognize the value of having software
that is friendly to system administrators.  Here are some of the reasons
that Open MPI is attractive for system administrators:

* Simple, standards-based installation
* Reduction of the number of MPI installations
* Ability to set system-level and user-level parameters
* Scriptable information sources about the Open MPI installation

See the rest of this section for more details.

Setting Global MCA Parameters
-----------------------------

MCA parameters are a way to tweak Open MPI's behavior at
run-time.  For example, MCA parameters can specify:

* Which interconnect networks to use
* Which interconnect networks *not* to use
* The size difference between eager sends and rendezvous protocol sends
* How many registered buffers to pre-pin (e.g., for InfiniBand)
* The size of the pre-pinned registered buffers
* ...etc.

It can be quite valuable for a system administrator to play with such
values a bit and find an "optimal" setting for a particular
operating environment.  These values can then be set in a global text
file that all users will, by default, inherit when they run Open MPI
jobs.

For example, say that you have a cluster with 2 ethernet networks |mdash|
one for NFS and other system-level operations, and one for MPI jobs.
The system administrator can tell Open MPI to not use the NFS TCP
network at a system level, such that when users invoke ``mpirun`` or
``mpiexec`` to launch their jobs, they will automatically only be using
the network meant for MPI jobs.

:ref:`See how to set MCA params
<label-running-setting-mca-param-values>` for information on how to
set global MCA parameters.

Setting MCA Parameters for a Global Open MPI installation
---------------------------------------------------------

Which global MCA parameters to set depends on both your specific
cluster setup and the applications that typically run there.

The best thing to do is to use the :ref:`ompi_info(1) <man1-ompi_info>` command
to see what parameters are available and relevant to you.  Specifically,
``ompi_info --all`` can be used to show all the parameters that are available
for each plug-in.  Two common places that system administrators like
to tweak are:

* *Only allow specific networks:* If you have a cluster with a
  high-speed interconnect (such as InfiniBand) and a
  low-speed ethernet network (e.g., 1Gpbps).  The high-speed network is
  intended for MPI jobs;
  the control network is intended for NFS and other
  administrative-level tasks.  In this case, you can simply turn off Open
  MPI's TCP support.  The ``btl`` framework contains Open MPI's network
  support; in this case, you want to disable the ``tcp`` plug-in.  You can
  do this by adding the following line in the file
  ``$prefix/etc/openmpi-mca-params.conf``:

  .. code-block::

      btl = ^tcp

  This tells Open MPI to load all BTL components *except* ``tcp``.

  Consider another example: your cluster has two TCP networks, one for
  NFS and administration-level jobs, and another for MPI jobs.  You can
  tell Open MPI to ignore the TCP network used by NFS by adding the
  following line in the file ``$prefix/etc/openmpi-mca-params.conf``:

  .. code-block::

      btl_tcp_if_exclude = lo,eth0

  The value of this parameter is the device names to exclude.  In this
  case, we're excluding ``lo`` (localhost, because Open MPI has its own
  internal loopback device) and ``eth0``.

* *Tune the parameters for specific networks:* Each network plug-in
  has a variety of different tunable parameters.  Use the ``ompi_info``
  command to see what is available.  You show *all* available parameters
  with:

  .. code-block::

      shell$ ompi_info --param all all

  .. note:: Starting with Open MPI v1.8, ``ompi_info`` categorizes its
     parameters in *levels*, as defined by the MPI_T interface from
     the MPI standard.  You will need to specify ``--level 9`` (or
     ``--all``) to show *all* MCA parameters.  `See this blog entry
     <https://blogs.cisco.com/performance/open-mpi-and-the-mpi-3-mpi_t-interface>`_
     for further information.

  .. code-block:: sh

     shell$ ompi_info --level 9
     # or
     shell$ ompi_info --all

  Beware: there are *many* variables available.  You can limit the
  output by showing all the parameters in a specific framework or in a
  specific plug-in with the command line parameters:

  .. code-block:: sh

     shell$ ompi_info --param btl all --level 9

  Shows all the parameters of all BTL components, and:

  .. code-block:: sh

     shell$ ompi_info --param btl tcp --level 9

  Shows all the parameters of just the ``tcp`` BTL component.

Adding a new plugin to a global Open MPI installation
-----------------------------------------------------

If you add a new component (such as support for a new network) to a global
Open MPI installation, Open MPI will
simply open the new plugin at run-time |mdash| your applications do not need
to be recompiled or re-linked.

:ref:`See this section <installing-custom-components-label>` for more
details.

Upgrading network hardware with a global Open MPI installation
--------------------------------------------------------------

If you upgrade your network hardware and your installation of Open MPI uses
shared libraries, you simply need to
recompile the Open MPI components that support that network and
re-install them.

More specifically, Open MPI shifts the dependency on the underlying
network away from the MPI applications and to the Open MPI plug-ins.
This is a major advantage over many other MPI implementations.
MPI applications will simply open the new plugin when they run.

User customization of a global Open MPI installation
----------------------------------------------------

It is typically sufficient for a single Open MPI
installation (or perhaps a small number of Open MPI installations,
depending on compiler interoperability,
:ref:`see installing multiple copies of Open MPI
<building-open-mpi-installation-location-multiple-copies-label>` for
more information) to serve an entire parallel
operating environment.

However, a system-wide Open MPI installation can be customized on a
per-user basis in two important ways:

* *Per-user MCA parameters:* Each user can
  :ref:`set their own set of MCA parameters, <label-running-setting-mca-param-values>`
  potentially overriding system-wide defaults.
* *Per-user plug-ins:* Users can install their own Open MPI
  plug-ins under ``$HOME/.openmpi/components``.  In this way, developers can
  experiment with new components without destabilizing Open MPI for the rest
  of the users on the system.  Or power users can download 3rd party components
  (perhaps even research-quality components) without affecting other users.
