OpenFabrics Interfaces (OFI) / Libfabric support
================================================

.. error:: TODO This section needs to be converted from FAQ Q&A style
           to regular documentation style.

What is OFI / Libfabric?
------------------------

"OFI" stands for the `OpenFabrics Interfaces
<https://libfabric.org/>`_, which are implemented in the ``libfabric``
library.  These two terms are typically used interchangeably.

Open MPI supports many different underlying networks via Libfabric,
including (but not limited to):

* AWS EFA
* Cisco usNIC
* Cray uGNI
* Cornelis Networks Omni-Path
* HPE Slingshot 11

In general, the OFI-based components in Open MPI will auto-select
themselves as appropriate at run time.

That being said, additional questions are available in this FAQ
section to provide more information about specific OFI-based network
types and support.

/////////////////////////////////////////////////////////////////////////

What are the Libfabric (OFI) components in Open MPI?
----------------------------------------------------

Open MPI has three main components for Libfabric (a.k.a., OFI)
communications:

#. ``ofi`` MTL: Available since Open MPI v1.10, this component is used
   with the ``cm`` PML and is used for two-sided MPI communication
   (e.g., ``MPI_SEND`` and ``MPI_RECV``).

  The ``ofi`` MTL requires that the Libfabric provider support
  reliable datagrams with ordered tagged messaging (specifically:
  ``FI_EP_RDM`` endpoints, ``FI_TAGGED`` capabilities, and
  ``FI_ORDER_SAS`` ordering).

#. ``ofi`` BTL: Available since Open MPI v4.0.0, this component is primarily
   intended for one-sided MPI communications (e.g., ``MPI_PUT``). It
   can also support BTL send/recv operations.
   ``ofi`` BTL requires that the Libfabric provider support reliable
   datagrams, RMA and atomic operations, and remote atomic completion
   notifications (specifically: ``FI_EP_RDM`` endpoints, ``FI_RMA``
   and ``FI_ATOMIC`` capabilities, and ``FI_DELIVERY_COMPLETE`` op
   flags).

#. ``usnic`` BTL: This BTL is used exclusively with Cisco usNIC-based
   networks.  It will auto-select itself over the other OFI-based
   components when run with Cisco usNIC-based networks.

See each Lifabric provider man page (e.g., fi_sockets(7)) to understand which
provider will work for each of the above-listed Open MPI components. Some
providers may require to be used with one of the Libfabric utility providers;
for example, the verbs provider needs to be paired with utility provider
``ofi_rxm`` to provide reliable datagram endpoint support (``verbs;ofi_rxm``).

Both components have MCA parameters to specify the Libfabric provider(s) that
will be included/excluded in the selection process. For example:

.. code-block::

   shell$ mpirun --mca pml cm --mca mtl ofi --mca mtl_ofi_provider_include psm2 mpi_hello

In addition, each component has specific parameters for each one; see
``ompi_info --param <framework> <component> -level 9`` for a full
list. For example:

.. code-block::

   shell$ ompi_info --param mtl ofi --level 9

.. important:: When using the HPE CXI provider and ``mpirun`` as the job launcher,
          it is recommended that the PRTE ``ras_base_launch_orted_on_hn`` MCA parameter be set to 1.
          This can be done by adding ``--prtemca ras_base_launch_orted_on_hn 1`` to the job launch
          command line.  This ensures that MPI processes launched on the first node of
          an allocation are able to use the CXI provider.
          
For more information refer to the `Libfabric web site
<https://libfabric.org/>`_.

/////////////////////////////////////////////////////////////////////////

Omni-Path: How can the multi-rail settings be adjusted if multiple HFI (Host Fabric Interface) cards are installed on the system?
---------------------------------------------------------------------------------------------------------------------------------

Multi-Rail feature allows a process to use multiple HFIs to transfer a message
to improve message bandwidth. The PSM2 library handles the support for multi-rail
which is off by default. The multi-rail settings can be modified using the
following environment variables:

* ``PSM2_MULTIRAIL=[0,1,2] ]``: 0=Disabled, 1=Enable across all HFIs in the
  system, 2=Enable multi-rail within a NUMA node.
* ``PSM2_MULTIRAIL_MAP=unit:port,unit:port...``

The variables above may be included in the ``mpirun`` command line or in
the environment. For example:

.. code-block::

   shell$ mpirun -mca mtl [psm2|ofi] -x PSM2_MULTIRAIL=1 -n 2 -H host1,host2 ./a.out

.. note:: When using the OFI MTL, please ensure that the PSM2 OFI
          provider is used for communication with OPA devices.

/////////////////////////////////////////////////////////////////////////

Omni-Path: What is Multi-HFI support in PSM2 and how does it differ from multi-rail?
------------------------------------------------------------------------------------

Multi-HFI support is intended to describe the use of multiple HFIs in
a system among MPI ranks local to a node in order to load-balance the
hardware resources. It differs from the Multi-Rail feature, which is
intended to allow a single process to use all HFIs in the system. For
an MPI job with multiple processes on a single node, the default PSM2
behavior depends on the affinity settings of the MPI process. The PSM2
library defaults to using the HFI (Host Fabric Interface) that is in
the same NUMA node as that of the MPI process.

Users can restrict access to a single HFI using the environment variable:

* ``HFI_UNIT=N``: valid values of N are 0,1,2 and 3

More details can be found on the PSM2 Programmer's Guide and the Omni-Path
Fabric Performance Tuning Guide.

Please see the `Cornelis Networks Customer Center <https://customercenter.cornelisnetworks.com/>`_
for more details.
