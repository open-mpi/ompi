OpenPMIx |opmix_ver|
====================

The charter of the PMIx community is to:

#. Develop an open source (non-copy-left licensed) and independent
   (i.e., not affiliated with any specific programming model code
   base) standalone library to support application interactions with
   Resource Managers (RMs).

#. Retain transparent compatibility with the existing PMI-1 and PMI-2
   definitions, and any future PMI releases.

#. Support the *Instant On* initiative for rapid startup of
   applications at exascale and beyond.

#. Work with the HPC community to define and implement new APIs that
   support evolving programming model requirements for application-RM
   interactions.

  .. note:: You will see ``OpenPMIx`` frequently referred to
            as just ``PMIx``. While there is a separate PMIx Standard, there
            are (as of this writing) no alternative implementations of that
            Standard. In fact, the Standard post-dates the library by several
            years, and often lags behind the library in terms of new definitions.
            Thus, it is customary to refer to the library as just ``PMIx`` and
            drop the longer name - at least, until some other implementation
            arises (which many consider unlikely).

PMIx is designed to be particularly easy for resource managers to
adopt, thus facilitating a rapid uptake into that community for
application portability. Both client and server libraries are
included, along with reference examples of client usage and
server-side integration.

PMIx targets support for the Linux operating system.  A reasonable
effort is made to support all major, modern Linux distributions;
however, validation is limited to the most recent 2-3 releases of
RedHat Enterprise Linux (RHEL), Fedora, CentOS, and SUSE Linux
Enterprise Server (SLES). Support for vendor-specific operating
systems is included as provided by the vendor.

Table of contents
=================

.. toctree::
   :maxdepth: 2
   :numbered:

   quickstart
   getting-help
   release-notes/index
   exceptions
   installing-pmix/index
   how-things-work/index
   release-notes
   history
   versions
   mca
   building-apps/index
   developers/index
   contributing
   license
   security
   news/index
   man/index
