.. _ft-data-reliability-label:

End-to-end data reliability for MPI messages
============================================

Current Open MPI releases have no support for end-to-end data
reliability, at least not more than currently provided by the
underlying network.

Previous releases included a data reliability PML component (``dr``) that
assumed that the underlying
network is unreliable.  It could drop / restart connections, retransmit
corrupted or lost data, etc.  The end effect was that data sent through MPI API
functions would be guaranteed to be reliable.

For example, if you're using TCP as a message transport, chances of
data corruption are fairly low.  However, other interconnects do *not*
guarantee that data will be uncorrupted when traveling across the
network.  Additionally, there are non-zero possibilities that data can
be corrupted while traversing PCI buses, etc. (some corruption errors
at this level can be caught/fixed, others cannot).  Such errors are
not uncommon at high altitudes (!).

Note that such added reliability does incur a performance cost |mdash|
latency and bandwidth suffer when Open MPI performs the consistency
checks that are necessary to provide such guarantees.

Most clusters/networks do not need data reliability.  But some do
(e.g., those operating at high altitudes).  The ``dr`` PML was intended for
these rare environments where reliability was an issue; and users were
willing to tolerate slightly slower applications in order to guarantee
that their job does not crash (or worse, produce wrong answers).

.. note:: The ``dr`` component has been removed from current releases
          of Open MPI, but is still available in older releases.
