Using Open MPI with HPE Slingshot
=================================

Open MPI supports Slingshot through the ``cxi`` provider in the `Libfabric
<https://libfabric.org/>`_ library. The ``cxi`` provider maps Open MPI
communication operations to the Slingshot Cassini network interface card and
Rosetta switch.


Requirements
------------

The system administrator must install the Slingshot software stack, including
the Cassini driver, libCXI, and a Libfabric installation that includes the
``cxi`` provider. Open MPI must be built with that Libfabric installation. For
example, when Libfabric is not in the default compiler search paths:

.. code-block:: sh

   shell$ ./configure --with-libfabric=/path/to/libfabric \
                      <other configure options>
   shell$ make all install

A job launcher normally supplies the authorization environment required by the
``cxi`` provider for an allocated job. The site launcher configuration
determines whether users need
to set ``SLINGSHOT_VNIS``, ``SLINGSHOT_SVC_IDS``, or
``SLINGSHOT_DEVICES``. These settings select the CXI service and Job IDs.
A Job ID isolates one job's network traffic from another job's traffic.
Incorrect values can prevent processes from communicating.

Verify the installation
-----------------------

On a Slingshot compute node, use the Libfabric ``fi_info`` utility to confirm
that the installed Libfabric can find the ``cxi`` provider:

.. code-block:: sh

   shell$ fi_info -p cxi

Use the Open MPI ``ompi_info`` utility to confirm that the OpenFabrics
Interfaces Message Transport Layer (MTL) component was built:

.. code-block:: sh

   shell$ ompi_info --param mtl ofi --level 9

If either command does not report the expected component or provider, consult
the system administrator. A login node may not have access to CXI devices, so
perform these checks within an allocation when necessary.

Launching Slingshot jobs
------------------------

When running in an allocation from Slurm, a workload manager, Open MPI
recommends using ``mpirun``. It uses the allocation information supplied by
Slurm; no host list or process count is needed when the allocation already
defines them:

.. code-block:: sh

   shell$ salloc -N 2 -n 64
   shell$ mpirun --prtemca ras_base_launch_orted_on_hn 1 ./mpi_application

The ``ras_base_launch_orted_on_hn`` setting is recommended when using the
``cxi`` provider with ``mpirun``. It ensures that MPI processes launched on the
first node of the allocation can use this provider.

Slurm direct launch is also available when Slurm was built with its Process
Management Interface for Exascale (PMIx) plugin:

.. code-block:: sh

   shell$ srun --mpi=pmix ./mpi_application

See :doc:`../../launching-apps/slurm` for general Slurm launch requirements and
behavior.

Selecting the ``cxi`` provider
------------------------------

Open MPI normally selects its OpenFabrics Interfaces components automatically.
To request the tagged-message path explicitly, select the ``cm``
Point-to-Point Management Layer (PML) and ``ofi`` Message Transport Layer
(MTL), and include the ``cxi`` provider. When the selected provider supports
hardware tag matching, message matching can be offloaded from the host
processor to the network interface card. The CXI provider supports this
capability. Tagged messages carry a numeric label that Open MPI uses to match
a send operation to the correct receive operation:

.. code-block:: sh

   shell$ mpirun --prtemca ras_base_launch_orted_on_hn 1 \
          --mca pml cm --mca mtl ofi \
          --mca mtl_ofi_provider_include cxi \
          ./mpi_application

This path uses the ``ofi`` Message Transport Layer for Message Passing
Interface point-to-point communication and the ``cxi`` provider for Slingshot
communication. The provider supports reliable datagram communication and the
tagged-message capability that this transport layer requires.

For jobs that use graphics processing unit (GPU) memory, use the same launch
pattern when the site Libfabric and ``cxi`` provider support the accelerator memory
in use. Consult the site configuration documentation for the supported GPU
runtime and provider settings; these capabilities are supplied by Libfabric and
the ``cxi`` provider, rather than by a Message Passing Interface application
setting.

Troubleshooting and tuning
--------------------------

To diagnose component or provider selection, add Open MPI verbosity only for
the relevant component:

.. code-block:: sh

   shell$ mpirun --mca mtl_ofi_verbose 1 \
          --mca mtl_ofi_provider_include cxi ./mpi_application

Check that the command is run within a Slingshot allocation and that every
compute node exposes the ``cxi`` device and matching Libfabric installation.
Provider errors involving a service identifier or virtual network identifier
usually indicate a job-launcher or fabric-authorization configuration issue;
contact the system administrator rather than reusing settings from another job.

When reporting an Open MPI issue, include the output of the relevant
``ompi_info`` command, ``fi_info -p cxi``, the Open MPI and Libfabric versions,
and the complete command line. Do not include credentials, allocation tokens,
or other site-sensitive environment values.

Open MPI controls component selection through Modular Component Architecture
(MCA) parameters. ``cxi`` transport settings, including provider-specific
performance tuning, are owned by Libfabric. Refer to the installed
``fi_cxi(7)`` manual page for the supported ``FI_CXI_*`` parameters and their
version-specific defaults. See :doc:`ofi` for general Open MPI OpenFabrics
Interfaces component information.
