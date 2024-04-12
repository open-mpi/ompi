General Run-Time Support Notes
==============================

* The PMIx installation must be in your ``PATH`` on all nodes (and
  potentially ``LD_LIBRARY_PATH`` or ``DYLD_LIBRARY_PATH``, if
  ``libpmix`` is a shared library).

* PMIx's run-time behavior can be customized via Modular Component
  Architecture (MCA) parameters (see the :ref:`MCA section <label-mca>`
  for more information on how to get/set MCA parameter values).

* If specified, we recommend that the ``ptl_tcp_if_include`` parameter
  include the loopback device (``lo`` on many Linux platforms) to ensure
  that at least one intra-node transport can be found between a local
  client and its server. For example:
  ``export PMIX_MCA_ptl_tcp_if_include lo,eth1 ...``

* Running on nodes with different endian and/or different datatype
  sizes within a single PMIx-based application is supported in this release,
  at least from a PMIx perspective (i.e., we will pack/unpack to
  handle the situation). However, PMIx does not resize data when the
  same datatype differs in size (for example, sending a 4 byte ``double``
  and receiving an 8 byte ``double`` will fail).
