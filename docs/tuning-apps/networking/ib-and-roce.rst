InfiniBand / RoCE support
=========================

Open MPI's support for InfiniBand and RoCE devices has changed over
time.  In the Open MPI |ompi_series| series, InfiniBand and RoCE
devices are supported via the UCX (``ucx``) PML.

.. note:: Prior versions of Open MPI also included the ``openib`` BTL
          for InfiniBand and RoCE devices.  Open MPI |ompi_series| no
          longer includes the ``openib`` BTL.

UCX
---

`UCX <https://openucx.org/>`_ is an open-source, optimized
communication library that supports multiple networks, including RoCE,
InfiniBand, uGNI, TCP, shared memory, and others.  UCX
mixes-and-matches the transports and protocols available on the system
to provide optimal performance.  It also has built-in support for GPU
transports (with CUDA and ROCm providers), which lets RDMA-capable
transports access GPU memory directly.

Using UCX with Open MPI
-----------------------

If Open MPI includes UCX support, then UCX is enabled and selected by
default for InfiniBand and RoCE network devices; typically, no
additional parameters are required.  In this case, the network port
with the highest bandwidth on the system is used for inter-node
communication, and shared memory is used for intra-node communication.

To select a specific network device to use (for example, the
``mlx5_0`` device, port 1):

.. code-block::

   shell$ mpirun -x UCX_NET_DEVICES=mlx5_0:1 ...

It is also possible to force the use of UCX for MPI point-to-point and
one-sided operations:

.. code-block::

   shell$ mpirun --mca pml ucx --mca osc ucx ...

For OpenSHMEM, in addition to the above, you can force the use of UCX
for remote memory access and atomic memory operations:

.. code-block::

   shell$ mpirun --mca pml ucx --mca osc ucx --mca scoll ucx --mca atomic ucx ...

RDMA over Converged Ethernet (RoCE)
-----------------------------------

RoCE (RDMA over Converged Ethernet) provides the InfiniBand native RDMA
transport on top of lossless Ethernet data links.  Because the
underlying link is Ethernet, there is no Subnet Manager, no Subnet
Administrator, no InfiniBand Service Level, and none of the other
InfiniBand Subnet Administration parameters.

Connection management in RoCE is based on the OFED RDMACM (RDMA
Connection Manager) service:

* The OS IP stack is used to resolve remote (IP, hostname) tuples to a
  DMAC.
* The outgoing Ethernet interface and VLAN are determined according to
  this resolution.
* The appropriate RoCE device is selected accordingly.
* Network parameters (such as MTU, SL, and timeout) are set locally by
  the RDMACM in accordance with kernel policy.

Running over RoCE
-----------------

To use RoCE with the UCX PML, specify the relevant Ethernet port with
the ``UCX_NET_DEVICES`` environment variable.  For example:

.. code-block::

   shell$ mpirun --mca pml ucx -x UCX_NET_DEVICES=mlx5_0:1 ...

UCX selects IPv4 RoCEv2 by default.  If different behavior is needed,
you can set a specific GID index:

.. code-block::

   shell$ mpirun --mca pml ucx -x UCX_NET_DEVICES=mlx5_0:1 -x UCX_IB_GID_INDEX=1 ...

Selecting the InfiniBand Service Level
--------------------------------------

To tell the UCX PML which InfiniBand Service Level (SL) to use, specify
it with the ``UCX_IB_SL`` environment variable.  For example:

.. code-block::

   shell$ mpirun --mca pml ucx -x UCX_IB_SL=N ...

The value ``N`` should be between 0 and 15, where 0 is the default.

Tuning MPI performance
----------------------

The ``ompi_info`` command can display all the parameters available for
any Open MPI component.  For example:

.. code-block::

   shell$ ompi_info --param pml ucx --level 9

.. important:: Unlike most other Open MPI components, the UCX PML mainly
               uses environment variables for run-time tuning |mdash|
               not Open MPI MCA parameters.  Consult `the UCX
               documentation <https://openucx.org/documentation/>`_ for
               details about which environment variables are available.

.. _faq-ib-troubleshoot-label:

Troubleshooting and getting help
--------------------------------

If you are experiencing a problem with Open MPI on an InfiniBand or
RoCE network, it is *most* helpful if you run a few steps before
sending an e-mail, both to perform some basic troubleshooting and to
provide enough information about your environment for others to help
you.  Please include answers to the following questions in your e-mail:

#. Which UCX and OpenFabrics versions are you running?  Please specify
   where you obtained the software (for example, from the OpenFabrics
   and/or UCX community web sites, already included in your Linux
   distribution, downloaded from NVIDIA's web site, etc.).

#. Which distribution and version of Linux are you running?  What is
   your kernel version?

#. What is the output of the ``ibv_devinfo`` command on a known "good"
   node and a known "bad" node?

   .. note:: There must be at least one port listed as ``PORT_ACTIVE``
             for Open MPI to work.  If there is not at least one
             ``PORT_ACTIVE`` port, something is wrong with your
             InfiniBand / RoCE environment, and Open MPI will not be
             able to run.

#. What is the output of the ``ifconfig`` command on a known "good"
   node and a known "bad" node?

   .. note:: Some Linux distributions do not put ``ifconfig`` in the
             default path for normal users; look for it at
             ``/sbin/ifconfig`` or ``/usr/sbin/ifconfig``.

#. If running under Bourne shells, what is the output of the ``ulimit
   -l`` command?  If running under C shells, what is the output of the
   ``limit | grep memorylocked`` command?

   This reports the maximum amount of memory that a user process is
   allowed to lock (pin) into physical RAM.  InfiniBand and RoCE
   transfers use RDMA, which requires the communication buffers to be
   locked so that the network hardware can access them directly.  If
   the reported value is not ``unlimited`` (or is not large enough for
   your application's buffers), memory registration can fail and MPI
   jobs may abort or perform poorly.  Raising this limit is a
   system-configuration task; see the `UCX documentation
   <https://openucx.org/documentation/>`__ and your OpenFabrics or
   vendor documentation for how to allow locked memory on your system.
