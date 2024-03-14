History of Open MPI
===================

Open MPI represents the merger of three prior MPI implementations:

#. LAM/MPI: originally from the Ohio State University supercomputing
   center and later migrated to the University of Notre Dame.
#. LA-MPI: from the US Department of Energy Los Alamos National
   Laboratory.
#. FT-MPI: from the University of Tennassee at Knoxville.  One of the
   UTK developers moved back to the University of Stuttgart in late
   2004, which effectively added their team into the project.

The lead developers of these projects kept bumping into each other at
various HPC conferences in 2003.  At each conference, our
lunch/dinner-table conversations got more and more technically
involved when it finally dawned on us that we are doing a *lot* of the
same things in each of our respective implementations.  Although each
MPI implementation focused on different areas of excellence, we all
shared the same common core values:

* A full MPI implementation
* Production quality code |mdash| it has to "just work"
* A desire to explore lots of things that an MPI implementation can do
  that we've never had the time/resources to investigate because we
  are bound to bug fixing, etc.

Hence, we decided to collaborate and pool our resources.  At SC2003,
we decided to start an entire new code base |mdash| leaving all the cruft
and legacy code of our prior implementations behind.  Take the best,
leave the rest.  The source tree's first commit was on November 22,
2003; development work started in earnest on January 5, 2004.  Since
then, we have met together as a group once a month (for at least a
week) to meet our goal of a world-class MPI implementation, bar none.


Goals of the Open MPI Project
-----------------------------

We have several top-level goals:

#. Create a free, open source, peer-reviewed, production-quality
   complete MPI implementation.
#. Provide extremely high, competitive performance (latency,
   bandwidth, ...pick your favorite metric).
#. Directly involve the HPC community with external development
   and feedback (vendors, 3rd party researchers, users, etc.).
#. Provide a stable platform for 3rd party research and commercial
   development.
#. Support a wide variety of HPC platforms and environments.

In short, we want to work *with* and *for* the HPC community to make a
world-class MPI implementation that can be used on a huge number and
kind of systems.


Community
---------

Bringing together smart researchers and developers to work on a common
product is not only a good idea, it's the open source model.  The Open
MPI project started by multiple MPI implementation teams, and that
proved to work *extremely* well; extending this concept to the HPC
open source community is the next logical step.

The component architecture upon which Open MPI is founded is designed
to foster 3rd party collaboration by enabling independent developers
to use Open MPI as a production quality research platform.  Although
Open MPI is a relatively large code base, it is not necessary to learn
the entirety of it; it may be sufficient to learn the interfaces for
the component type which you are implementing and some of the
surrounding infrastructure.  Specifically, the component architecture
was designed to allow small, discrete implementations of major
portions of MPI functionality (e.g., point-to-point messaging,
collective communications, run-time environment support, etc.).
