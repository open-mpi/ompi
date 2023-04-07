Fault tolerance
===============

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


.. toctree::
   :maxdepth: 1

   supported
   checkpoint-restart
   data-reliability
