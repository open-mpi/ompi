Supported systems
=================

.. TODO How can I create a TOC just for this page here at the top?

/////////////////////////////////////////////////////////////////////////

What operating systems does Open MPI support?
---------------------------------------------

We primarily develop Open MPI on Linux and MacOS.

Other operating systems are supported, however.  The exact list of
operating systems supported has changed over time (e.g., native
Microsoft Windows support was added in v1.3.3, and although it was
removed prior to v1.8, is still supported through Cygwin).  :ref:`See
the Platform Notes section <platform-notes-section-label>` for a
listing of the OSes that that version supports.

Open MPI is fairly POSIX-neutral, so it will run without *too* many
modifications on most POSIX-like systems.  Hence, if we haven't listed
your favorite operating system here, it may not be difficult to get
Open MPI to compile and run properly.  The biggest obstacle is
typically the assembly language, but that's fairly modular and we're
happy to provide information about how to port it to new platforms.

It should be noted that we are quite open to accepting patches for
operating systems that we do not currently support.  If we do not have
systems to test these on, we probably will only claim to
"unofficially" support those systems.


/////////////////////////////////////////////////////////////////////////

What hardware platforms does Open MPI support?
----------------------------------------------

Essentially all the common platforms that the operating
systems listed in the previous question support.

For example, Linux runs on a *wide* variety of platforms, and we
certainly don't claim to test all of them.  Open MPI includes
Linux-compiler-based assembly for support of Intel, AMD, ARM, and
PowerPC chips, for example.


/////////////////////////////////////////////////////////////////////////

What network interconnects does Open MPI support?
-------------------------------------------------

:ref:`See the Platform Notes section <platform-notes-section-label>`
for a list of networks supported in this specific release of Open MPI.

The set of commonly-available HPC-class network interconnects has
evolved and changed over time.

Reflecting that evolution, each release of Open MPI supports a
specific set of such network interconnects.  You will need to check
the documentation of the version of your Open MPI installation to see
which interconnects it supports.  A general rule of thumb is that a
given Open MPI version tends to support the popular HPC-class
interconnects at the time of its release.

This, unfortunately, does mean that Open MPI removes support for
networks that are no longer commonly-used in HPC environments.  If you
still have one of these older interconnects, not all new version of
Open MPI may support your interconnect -- sorry!  This simply reflects
the reality of limited development, testing, and maintenance
resources.

That being said, :doc:`contributions are always welcome!
</contributing>`.


/////////////////////////////////////////////////////////////////////////

How does Open MPI interface to back-end run-time systems?
---------------------------------------------------------

Prior versions of Open MPI used to be layered on top of the Open
Run-Time Environment (ORTE).  ORTE originally started as a small
portion of the Open MPI code base, but over time, ORTE effectively
spun off into its own sub-project.  ORTE ultimately evolved into the
`Process Management Interface Exascale (PMIx) standard and
corresponding OpenPMIx software project <https://openpmix.org/>`_.

The OpenPMIx project then evolved its own `PMIx Reference Run-Time
Environment (PRRTE) <https://github.com/openpmix/prrte>`_ project.

PRRTE has effectively replaced ORTE in the Open MPI implementation.


/////////////////////////////////////////////////////////////////////////

What run-time environments does Open MPI support?
-------------------------------------------------

:ref:`See the Platform Notes section <platform-notes-section-label>`
for a list of run-time environments supported in this specific release
of Open MPI.

Since Open MPI uses `PRRTE <https://github.com/openpmix/prrte>`_ as
its back-end run-time system, Open MPI supports whatever run-time
systems PRRTE supports.

Each version of Open MPI supports a specific set of versions of
PRRTE.  Those versions therefore determine which run-time systems that
that release of Open MPI supports.


/////////////////////////////////////////////////////////////////////////

.. _faq_supported_systems_mpi_compliance_label:

How much MPI does Open MPI support?
-----------------------------------

* Open MPI 1.2 supports all of MPI-2.0.

* Open MPI 1.3 supports all of MPI-2.1.

* Open MPI 1.8 supports all of MPI-3.0.

* Starting with v2.0, Open MPI supports all of MPI-3.1.


/////////////////////////////////////////////////////////////////////////

Is Open MPI thread safe?
------------------------

Support for ``MPI_THREAD_MULTIPLE`` (i.e., multiple threads
executing within the MPI library) and asynchronous message passing
progress (i.e., continuing message passing operations even while no
user threads are in the MPI library) has been designed into Open MPI
from its first planning meetings.

Support for ``MPI_THREAD_MULTIPLE`` was included in the first version of
Open MPI, but it only became robust around v3.0.0.  Subsequent
releases continually improve reliability and performance of
multi-threaded MPI applications.


/////////////////////////////////////////////////////////////////////////

Does Open MPI support 32 bit environments?
------------------------------------------

As far as we know, yes.  64 bit architectures have effectively taken
over the world, though, so 32-bit is not tested nearly as much as
64-bit.

Specifically, most of the Open MPI developers only have 64-bit
machines, and therefore only test 32-bit in emulation mode.


/////////////////////////////////////////////////////////////////////////

Does Open MPI support 64 bit environments?
------------------------------------------

Yes, Open MPI is 64 bit clean. You should be able to use Open MPI on
64 bit architectures and operating systems with no difficulty.


/////////////////////////////////////////////////////////////////////////

Does Open MPI support execution in heterogeneous environments?
--------------------------------------------------------------

Heterogeneous support (specifically: supporting different sized and/or
represented data types in a single MPI application run) within a
single MPI job is technically required by the MPI standard.

However, there are both theoretical and practical problems with
supporting true data heterogeneity at run time.

Indeed, it is quite uncommon for production HPC environments to be
data-heterogeneous (e.g., natively support little endian on some nodes
and big endian on other nodes in the same MPI application job).

As such, supporting data heterogeneity is a feature that has fallen
into disrepair: it is currently known to be broken in this release of
Open MPI.

:doc:`Contributions to fix it would be welcome! </contributing>`

/////////////////////////////////////////////////////////////////////////

Does Open MPI support parallel debuggers?
-----------------------------------------

Yes.  Open MPI supports the TotalView API for parallel process
attaching, which several parallel debuggers support (e.g., DDT, fx2).
As part of v1.2.4 (released in September 2007), Open MPI also supports
the TotalView API for viewing message queues in running MPI processes.

:ref:`See this FAQ entry <faq-running-mpi-apps-totalview-label>` for
details on how to run Open MPI jobs under TotalView, and :ref:`this
FAQ entry <faq-running-mpi-apps-ddt-label>` for details on how to run
Open MPI jobs under DDT.

.. note:: The integration of Open MPI message queue support is
   problematic with 64 bit versions of TotalView prior to v8.3:

   * The message queues views will be truncated.
   * Both the communicators and requests list will be incomplete.
   * Both the communicators and requests list may be filled with wrong
     values (such as an ``MPI_Send`` to the destination
     ``MPI_ANY_SOURCE``).

   There are two workarounds:

   * Use a 32 bit version of TotalView
   * Upgrade to TotalView v8.3
