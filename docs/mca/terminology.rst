Terminology
===========

The Modular Component Architecture (MCA) is the backbone for much of
Open MPI's functionality.  It is a series of *projects*, *frameworks*,
*components*, and *modules* that are assembled at run-time to create
an MPI implementation.

MCA *parameters* (also known as MCA *variables*) are used to customize
Open MPI's behavior at run-time.

Each of these entities are described below.

Projects
--------

A *project* is essentially the highest abstraction layer division in
the Open MPI code base.

.. note:: The word "project" is unfortunately overloaded.  It can be
          used to mean the code/resources/people in the greater Open
          MPI community associated with the development of a
          particular software package, but it can also be used to mean
          a major, top-level section of code within the Open MPI code
          base.

          For the purposes of this documentation, "project" means the
          latter: a major, top-level section of code within the Open
          MPI code base.

The following *projects* exist in Open MPI |ompi_ver|:

* **Open Portability Access Layer (OPAL):** Low-level, operating
  system and architecture portability code.
* **Open MPI (OMPI):** The MPI API and supporting infrastructure.
* **OpenSHMEM (OSHMEM):** The OpenSHMEM API and supporting
  infrastructure.

.. note:: Prior versions of Open MPI also included an Open MPI
          Runtime Environment (ORTE) project.  ORTE essentially
          evolved into the standalone `PMIx Runtime Reference
          Environment (PRRTE) <https://github.com/openpmix/prrte>`_
          and is now considered a 3rd-party dependency of Open MPI
          |mdash| not one of its included projects.

          See :ref:`the role of PMIx and PRRTE
          <label-running-role-of-pmix-and-prte>` for more information.

Frameworks
----------

An MCA framework manages zero or more components at run-time and is
targeted at a specific task (e.g., providing MPI collective operation
functionality).  Although each MCA framework supports only a single
type of component, it may support multiple components of that type.

Some of the more common frameworks that users may want or need to
customize include the following:

* ``btl``: Byte Transport Layer; these components are exclusively used
  as the underlying transports for the ``ob1`` PML component.
* ``coll``: MPI collective algorithms
* ``io``: MPI I/O
* ``mtl``: MPI Matching Transport Layer (MTL); these components are
  exclusively used as the underlying transports for the ``cm`` PML
  component.
* ``pml``: Point-to-point Messaging Layer (PML).  These components are
  used to implement MPI point-to-point messaging functionality.

There are many frameworks within Open MPI; the exact set varies
between different versions of Open MPI.  You can use the
:ref:`ompi_info(1) <man1-ompi_info>` command to see the full list of
frameworks that are included in Open MPI |ompi_ver|.

Components
----------

An MCA component is an implementation of a framework's formal
interface.  It is a standalone collection of code that can be bundled
into a plugin that can be inserted into the Open MPI code base, either
at run-time and/or compile-time.

.. note:: Good synonyms for Open MPI's "component" concept are
          "plugin", or "add-on".

The exact set of components varies between different versions of Open
MPI.  Open MPI's code base includes support for many components, but
not all of them may be present or available on your system.  You can
use the :ref:`ompi_info(1) <man1-ompi_info>` command to see what
components are included in Open MPI |ompi_ver| on your system.

Modules
-------

An MCA module is an instance of a component (in the C++ sense of the
word "instance"; an MCA component is analogous to a C++ class).  For
example, if a node running an Open MPI application has two Ethernet
NICs, the Open MPI application will contain one TCP MPI point-to-point
*component*, but two TCP point-to-point *modules*.

.. _label-mca-terminology-parameters:

Parameters (variables)
----------------------

MCA *parameters* (sometimes called MCA *variables*) are the basic unit
of run-time tuning for Open MPI.  They are simple "key = value" pairs
that are used extensively throughout Open MPI.  The general rules of
thumb that the developers use are:

#. Instead of using a constant for an important value, make it an MCA
   parameter.
#. If a task can be implemented in multiple, user-discernible ways,
   implement as many as possible, and use an an MCA parameter to
   choose between them at run-time.

For example, an easy MCA parameter to describe is the boundary between
short and long messages in TCP wire-line transmissions.  "Short"
messages are sent eagerly whereas "long" messages use a rendezvous
protocol.  The decision point between these two protocols is the
overall size of the message (in bytes).  By making this value an MCA
parameter, it can be changed at run-time by the user or system
administrator to use a sensible value for a particular environment or
set of hardware (e.g., a value suitable for 1Gpbs Ethernet is probably
not suitable for 100 Gigabit Ethernet, and may require even a third
different value for 25 Gigabit Ethernet).
