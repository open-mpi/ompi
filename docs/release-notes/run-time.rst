General Run-Time Support Notes
==============================

* The Open MPI installation must be in your ``PATH`` on all nodes (and
  potentially ``LD_LIBRARY_PATH`` or ``DYLD_LIBRARY_PATH``, if
  ``libmpi``/``libshmem`` is a shared library), unless using the
  ``--prefix`` or ``--enable-mpirun-prefix-by-default`` functionality (see
  below).

* Open MPI's run-time behavior can be customized via Modular Component
  Architecture (MCA) parameters (see :ref:`this section
  <label-running-setting-mca-param-values>`) for more information on
  how to get/set MCA parameter values).  Some MCA parameters can be
  set in a way that renders Open MPI inoperable.  In particular, some
  parameters have required options that must be included:

  * If specified, the ``btl`` parameter must include the ``self``
    component, or Open MPI will not be able to deliver messages to the
    same rank as the sender.  For example: ``mpirun --mca btl tcp,self
    ...``
  * If specified, the ``btl_tcp_if_exclude`` parameter must include the
    loopback device (``lo`` on many Linux platforms), or Open MPI will
    not be able to route MPI messages using the TCP BTL.  For example:
    ``mpirun --mca btl_tcp_if_exclude lo,eth1 ...``

* Running on nodes with different endian and/or different datatype
  sizes within a single parallel job is supported in this release.
  However, Open MPI does not resize data when datatypes differ in size
  (for example, sending a 4 byte ``MPI_DOUBLE`` and receiving an 8 byte
  ``MPI_DOUBLE`` will fail).
