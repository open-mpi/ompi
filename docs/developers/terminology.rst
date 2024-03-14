Open MPI terminology
====================

Open MPI is a large project containing many different sub-systems and
a relatively large code base.  Let's first cover some fundamental
terminology in order to make the rest of the discussion easier.

Modular Component Architecture (MCA)
------------------------------------

:ref:`See this section <label-mca>` for a discussion of the Modular
Component Architecture (MCA).  Seriously.  Go read it now.  From
reading that section, you should understand the following terms before
continuing reading these docs:

* Project
* Framework
* Component
* Module
* Parameters (variables)

Notes on projects
-----------------

Projects are strict abstraction barriers in the code.  That is, they
are compiled into separate libraries: ``liboshmem``, ``libmpi``,
``libopen-pal`` with a strict dependency order: OSHMEM depends on
OMPI, OMPI depends on OPAL.  For example, MPI executables are linked
with:

.. code-block:: sh

   shell$ mpicc myapp.c -o myapp
   # This actually turns into:
   shell$ cc myapp.c -o myapp -lmpi ...

More system-level libraries may listed after ``-lmpi``, but you get
the idea.  ``libmpi`` will implicitly pull ``libopen-pal`` into the
overall link step.

Strictly speaking, these are not "layers" in the classic software
engineering sense (even though it is convenient to refer to them as
such).  They are listed above in dependency order, but that does not
mean that, for example, the OMPI code must go through the OPAL code in
order to reach the operating system or a network interface.

As such, this code organization more reflects abstractions and
software engineering, not a strict hierarchy of functions that must be
traversed in order to reach a lower layer.  For example, OMPI can
directly call the operating system as necessary (and not go through
OPAL).  Indeed, many top-level MPI API functions are quite performance
sensitive; it would not make sense to force them to traverse an
arbitrarily deep call stack just to move some bytes across a network.

Frameworks, components, and modules can be dynamic or static. That is,
they can be available as plugins or they may be compiled statically
into libraries (e.g., ``libmpi``).

In Open MPI |ompi_ver|, ``configure`` defaults to:

* Building projects as dynamic libraries
* Linking all components into their parent project libraries
  (vs. compiling them as independent DSOs)

Although these defaults can be modified by :doc:`command line
arguments to configure
</installing-open-mpi/configure-cli-options/index>`.

Required 3rd party libraries
----------------------------

Note that Open MPI also uses some third-party libraries for core
functionality:

* PMIx
* PRRTE
* Libevent
* Hardware Locality ("hwloc")

These are discussed in detail in the :ref:`required support libraries
section <label-install-required-support-libraries>`.
