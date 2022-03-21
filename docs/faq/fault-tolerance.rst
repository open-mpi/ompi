Fault Tolerance
===============

What is "fault tolerance"?
--------------------------

The phrase "fault tolerance" means many things to many
people.  Typical definitions include user processes dumping vital
state to disk periodically, checkpoint/restart of running processes,
elaborate recreate-process-state-from-incremental-pieces schemes,
and many others.

In the scope of Open MPI, we typically define "fault tolerance" to
mean the ability to recover from one or more component failures in a
well defined manner with either a transparent or application-directed
mechanism.  Component failures may exhibit themselves as a corrupted
transmission over a faulty network interface or the failure of one or
more serial or parallel processes due to a processor or node failure.
Open MPI strives to provide the application with a consistent system
view while still providing a production quality, high performance
implementation.

Yes, that's pretty much as all-inclusive as possible |mdash| intentionally
so!  Remember that in addition to being a production-quality MPI
implementation, Open MPI is also a vehicle for research.  So while
some forms of "fault tolerance" are more widely accepted and used,
others are certainly of valid academic interest.

/////////////////////////////////////////////////////////////////////////

What fault tolerance techniques has / does / will Open MPI support?
-------------------------------------------------------------------

Open MPI was a vehicle for research in fault tolerance and over the years provided
support for a wide range of resilience techniques:

* Current
    * User Level Fault Mitigation techniques similar to
      those implemented in FT-MPI.

* Deprecated / no longer available
    * Coordinated and uncoordinated process checkpoint and
      restart. Similar to those implemented in LAM/MPI and MPICH-V,
      respectively.
    * <strike>Message logging techniques. Similar to those implemented in
      MPICH-V</strike>
    * <strike>Data Reliability and network fault tolerance. Similar to those
      implemented in LA-MPI</strike>

The Open MPI team will not limit their fault tolerance techniques to
those mentioned above, but intend on extending beyond them in the
future.

/////////////////////////////////////////////////////////////////////////

Does Open MPI support checkpoint and restart of parallel jobs (similar to LAM/MPI)?
-----------------------------------------------------------------------------------

Old versions of OMPI (starting from v1.3 series) had support for
the transparent, coordinated checkpointing and restarting of MPI
processes (similar to LAM/MPI).

Open MPI supported both the the `BLCR <http://ftg.lbl.gov/checkpoint/>`_
checkpoint/restart system and a "self" checkpointer that allows
applications to perform their own checkpoint/restart functionality while taking
advantage of the Open MPI checkpoint/restart infrastructure.
For both of these, Open MPI provides a coordinated checkpoint/restart protocol
and integration with a variety of network interconnects including shared memory,
Ethernet, and InfiniBand.

The implementation introduces a series of new frameworks and
components designed to support a variety of checkpoint and restart
techniques. This allows us to support the methods described above
(application-directed, BLCR, etc.) as well as other kinds of
checkpoint/restart systems (e.g., Condor, libckpt) and protocols
(e.g., uncoordinated, message induced).

.. note:: The
   checkpoint/restart support was last released as part of the v1.6
   series.

/////////////////////////////////////////////////////////////////////////

Where can I find the fault tolerance development work?
------------------------------------------------------

The only active work in resilience in Open MPI
targets the User Level Fault Mitigation (ULFM) approach, a
technique discussed in the context of the MPI standardization
body.

For information on the Fault Tolerant MPI prototype in Open MPI see the
links below:

* `MPI Forum's Fault Tolerance Working Group <https://github.com/mpiwg-ft/ft-issues/wiki>`_
* Fault Tolerant MPI Prototype:
    * `Development / code <https://bitbucket.org/icldistcomp/ulfm2>`_
    * `Information and support <https://fault-tolerance.org/>`_

Support for other types of resilience (e.g., :ref:`data reliability <faq-ft-data-reliability-label>`,
checkpoint) has been deprecated over the years
due to lack of adoption and lack of maintenance. If you are interested
in doing some archeological work, traces are still available on the main
repository.

/////////////////////////////////////////////////////////////////////////

.. _faq-ft-data-reliability-label:

Does Open MPI support end-to-end data reliability in MPI message passing?
-------------------------------------------------------------------------

Current Open MPI releases have no support for end-to-end data
reliability, at least not more than currently provided by the
underlying network.

The data reliability PML component (``dr``, available
on some past releases has been deprecated), assumed that the
underlying network is unreliable.  It could drop / restart connections,
retransmit corrupted or lost data, etc.  The end effect is that data
sent through MPI API functions will be guaranteed to be reliable.

For example, if you're using TCP as a message transport, chances of
data corruption are fairly low.  However, other interconnects do *not*
guarantee that data will be uncorrupted when traveling across the
network.  Additionally, there are nonzero possibilities that data can
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
