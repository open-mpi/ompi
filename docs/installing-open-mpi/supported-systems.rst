Supported Systems
=================

Operating systems, hardware, and network requirements for this version of
Open MPI.

Supported Operating Systems
~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Supported Hardware Platforms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Essentially all the common platforms that the operating
systems listed in the previous section support.

For example, Linux runs on a *wide* variety of platforms, and we
certainly don't claim to test all of them.  Open MPI includes
Linux-compiler-based assembly for support of Intel, AMD, ARM, and
PowerPC chips, for example.


Supported Network Interconnects
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
Open MPI may support your interconnect |mdash| sorry!  This simply reflects
the reality of limited development, testing, and maintenance
resources.

That being said, :doc:`contributions are always welcome!
</contributing>`.
