.. _ft-checkpoint-restart-label:

Checkpoint and restart of parallel jobs
=======================================

Old versions of Open MPI (starting from v1.3 series) had support for
the transparent, coordinated checkpointing and restarting of MPI
processes (similar to LAM/MPI).

Open MPI supported both the the `BLCR <http://ftg.lbl.gov/checkpoint/>`_
checkpoint/restart system and a "self" checkpointer that allows
applications to perform their own checkpoint/restart functionality while taking
advantage of the Open MPI checkpoint/restart infrastructure.
For both of these, Open MPI provides a coordinated checkpoint/restart protocol
and integration with a variety of network interconnects including shared memory,
Ethernet, and InfiniBand.

The implementation introduced a series of new frameworks and
components designed to support a variety of checkpoint and restart
techniques. This enabled support for the methods described above
(application-directed, BLCR, etc.) as well as other kinds of
checkpoint/restart systems (e.g., Condor, libckpt) and protocols
(e.g., uncoordinated, message induced).

.. note:: The checkpoint/restart support was last included as part of
          the v1.6 series.  It was removed from Open MPI after failing
          to gain adoption and lack of maintenance.
