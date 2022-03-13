Network Support
===============

Main network support models
---------------------------

There are multiple MPI network models available in this release:

* ``ob1`` supports a variety of networks using BTL ("Byte Transfer
  Layer") plugins that can be used in
  combination with each other:

  * ``self``: Loopback (send-to-self)
  * ``sm``: Shared memory, including single-copy technologies:
    XPMEM, Linux CMA, as Linux KNEM, as well as traditional
    copy-in/copy-out shared memory.
  * ``tcp``: TCP
  * ``smcuda``: CUDA-enabled shared memory
  * ``usnic``: Cisco usNIC
  * ``ugni``: uGNI (Cray Gemini, Aries)

* ``cm`` supports a smaller number of networks (and they cannot be
  used together), but may provide better overall MPI performance by
  utilizing MTL ("Matching Transport Layer") plugins:

  * OpenFabrics Interfaces ("libfabric" tag matching)
  * Intel Omni-Path PSM2 (version 11.2.173 or later)
  * Intel True Scale PSM (QLogic InfiniPath)
  * Portals 4

* ``ucx`` uses the `Unified Communication X (UCX) communication
  library <https://www.openucx.org/>`_.  This is an open-source
  project developed in collaboration between industry, laboratories,
  and academia to create an open-source production grade
  communication framework for data centric and high-performance
  applications.  The UCX library can be downloaded from repositories
  (e.g., Fedora/RedHat yum repositories).  The UCX library is also
  part of Mellanox OFED and Mellanox HPC-X binary distributions.

  UCX currently supports:

  * OpenFabrics Verbs (including InfiniBand and RoCE)
  * Cray's uGNI
  * TCP
  * Shared memory
  * NVIDIA CUDA drivers

While users can manually select any of the above transports at run
time, Open MPI will select a default transport as follows:

#. If InfiniBand devices are available, use the UCX PML.
#. If PSM, PSM2, or other tag-matching-supporting Libfabric
   transport devices are available (e.g., Cray uGNI), use the ``cm``
   PML and a single appropriate corresponding ``mtl`` module.
#. Otherwise, use the ``ob1`` PML and one or more appropriate ``btl``
   modules.

Users can override Open MPI's default selection algorithms and force
the use of a specific transport if desired by setting the ``pml`` MCA
parameter (and potentially the ``btl`` and/or ``mtl`` MCA parameters) at
run-time:

.. code-block:: sh

   shell$ mpirun --mca pml ob1 --mca btl [comma-delimted-BTLs] ...
   # or
   shell$ mpirun --mca pml cm --mca mtl [MTL] ...
   # or
   shell$ mpirun --mca pml ucx ...

There is a known issue when using UCX with very old Mellanox
Infiniband HCAs, in particular HCAs preceding the introduction of
the ConnectX product line, which can result in Open MPI crashing in
MPI_Finalize.  This issue is addressed by UCX release 1.9.0 and
newer.

Miscellaneous network notes
---------------------------

* The main OpenSHMEM network model is ``ucx``; it interfaces directly
  with UCX.

* In prior versions of Open MPI, InfiniBand and RoCE support was
  provided through the ``openib`` BTL and ``ob1`` PML plugins.  Starting
  with Open MPI 4.0.0, InfiniBand support through the ``openib`` plugin
  is both deprecated and superseded by the ``ucx`` PML component.  The
  ``openib`` BTL was removed in Open MPI v5.0.0.

  While the ``openib`` BTL depended on ``libibverbs``, the UCX PML depends
  on the UCX library.

  Once installed, Open MPI can be built with UCX support by adding
  ``--with-ucx`` to the Open MPI configure command. Once Open MPI is
  configured to use UCX, the runtime will automatically select the
  ``ucx`` PML if one of the supported networks is detected (e.g.,
  InfiniBand).  It's possible to force using UCX in the ``mpirun`` or
  ``oshrun`` command lines by specifying any or all of the following mca
  parameters: ``--mca pml ucx`` for MPI point-to-point operations,
  ``--mca spml ucx`` for OpenSHMEM support, and ``--mca osc ucx`` for MPI
  RMA (one-sided) operations.

* The ``usnic`` BTL is support for Cisco's usNIC device ("userspace NIC")
  on Cisco UCS servers with the Virtualized Interface Card (VIC).
  Although the usNIC is accessed via the OpenFabrics Libfabric API
  stack, this BTL is specific to Cisco usNIC devices.

* uGNI is a Cray library for communicating over the Gemini and Aries
  interconnects.

* Linux ``knem`` support is used when the ``sm`` (shared memory) BTL is
  compiled with knem support (see the ``--with-knem`` configure option)
  and the ``knem`` Linux module is loaded in the running kernel.  If the
  ``knem`` Linux kernel module is not loaded, the ``knem`` support is (by
  default) silently deactivated during Open MPI jobs.

  See https://knem.gitlabpages.inria.fr/ for details on Knem.

* Linux Cross-Memory Attach (CMA) or XPMEM is used by the ``sm`` shared
  memory BTL when the CMA/XPMEM libraries are installed,
  respectively.  Linux CMA and XPMEM are similar (but different)
  mechanisms for Open MPI to utilize single-copy semantics for shared
  memory.

* The OFI MTL does not support sending messages larger than the active
  Libfabric provider's ``max_msg_size``.  If you receive an error
  message about sending too large of a message when using the OFI MTL,
  please reach out to your networking vendor to ask them to support a
  larger ``max_msg_size`` for tagged messages.
