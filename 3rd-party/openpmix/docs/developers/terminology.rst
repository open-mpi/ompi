OpenPMIx terminology
====================

OpenPMIx is a medium-sized project containing a number of different sub-systems and
a relatively big code base.  Let's first cover some fundamental
terminology in order to make the rest of the discussion easier.

First, note that you will see ``OpenPMIx`` frequently referred to
as just ``PMIx``. While there is a separate PMIx Standard, there
are (as of this writing) no alternative implementations of that
Standard. In fact, the Standard post-dates the library by several
years, and often lags behind the library in terms of new definitions.
Thus, it is customary to refer to the library as just ``PMIx`` and
drop the longer name - at least, until some other implementation
arises (which many consider unlikely).

Modular Component Architecture (MCA)
------------------------------------

:ref:`See this section <label-mca>` for a discussion of the Modular
Component Architecture (MCA).  Seriously.  Go read it now.  From
reading that section, you should understand the following terms before
continuing reading these docs:

* Framework
* Component
* Module
* Parameters (variables)

Frameworks, components, and modules can be dynamic or static. That is,
they can be available as plugins or they may be compiled statically
into libraries (e.g., ``libpmix``).

In PMIx, ``configure`` defaults to:

* Building ``libpmix`` as a dynamic library
* Linking all components directly into the ``libpmix`` libraries
  (vs. compiling them as independent DSOs)

These defaults can be modified by :doc:`command line
arguments to configure
</installing-pmix/configure-cli-options/index>`.

Required 3rd party libraries
----------------------------

Note that PMIx uses two third-party libraries for core
functionality:

* Libevent or Libev
* Hardware Locality ("hwloc")

These are discussed in detail in the :ref:`required support libraries
section <label-install-required-support-libraries>`.
