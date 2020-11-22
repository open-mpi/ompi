Open MPI terminology
====================

Open MPI is a large project containing many different
sub-systems and a relatively large code base.  Let's first cover some
fundamental terminology in order to make the rest of the discussion
easier.

Open MPI has multiple main sections of code:

* *OSHMEM:* The OpenSHMEM API and supporting logic
* *OMPI:* The MPI API and supporting logic
* *OPAL:* The Open Portable Access Layer (utility and "glue" code)

There are strict abstraction barriers in the code between these
sections.  That is, they are compiled into separate libraries:
``liboshmem``, ``libmpi``, ``libopen-pal`` with a strict dependency order:
OSHMEM depends on OMPI, OMPI depends on OPAL.  For example, MPI
executables are linked with:

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
mean that, for example, the OMPI code must go through the
OPAL code in order to reach the operating system or a network
interface.

As such, this code organization more reflects abstractions and
software engineering, not a strict hierarchy of functions that must be
traversed in order to reach a lower layer.  For example, OMPI can
directly call the operating system as necessary (and not go through
OPAL).  Indeed, many top-level MPI API functions are quite performance
sensitive; it would not make sense to force them to traverse an
arbitrarily deep call stack just to move some bytes across a network.

Note that Open MPI also uses some third-party libraries for core
functionality:

* PMIx
* PRRTE
* Libevent
* Hardware Locality ("hwloc")

These are discussed in detail in the :ref:`required support libraries
section <label-install-required-support-libraries>`.

Here's a list of terms that are frequently used in discussions about
the Open MPI code base:

* *MCA:* The Modular Component Architecture (MCA) is the foundation
  upon which the entire Open MPI project is built.  It provides all the
  component architecture services that the rest of the system uses.
  Although it is the fundamental heart of the system, its
  implementation is actually quite small and lightweight |mdash| it is
  nothing like CORBA, COM, JINI, or many other well-known component
  architectures.  It was designed for HPC |mdash| meaning that it is small,
  fast, and reasonably efficient |mdash| and therefore offers few services
  other than finding, loading, and unloading components.

* *Framework:* An MCA *framework* is a construct that is created for a
  single, targeted purpose.  It provides a public interface that is
  used by external code, but it also has its own internal services.
  :ref:`See the list of Open MPI frameworks in this version of Open
  MPI <label-frameworks>`.  An MCA framework uses the MCA's services
  to find and load *components* at run-time |mdash| implementations of
  the framework's interface.  An easy example framework to discuss is
  the MPI framework named ``btl``, or the Byte Transfer Layer.  It is
  used to send and receive data on different kinds of networks.
  Hence, Open MPI has ``btl`` components for shared memory,
  OpenFabrics interfaces, various protocols over Ethernet, etc.

* *Component:* An MCA *component* is an implementation of a
  framework's interface.  Another common word for component is
  "plugin". It is a standalone collection of code that can be bundled
  into a unit that can be inserted into the Open MPI code base, either
  at run-time and/or compile-time.

* *Module:* An MCA *module* is an instance of a component (in the C++
  sense of the word "instance"; an MCA component is analogous to a C++
  class, and an MCA module is analogous to a C++ object). For example,
  if a node running an Open MPI application has two Ethernet NICs, the
  Open MPI application will contain one TCP ``btl`` component, but two
  TCP ``btl`` modules.  This difference between components and modules
  is important because modules have private state; components do not.

Frameworks, components, and modules can be dynamic or static. That is,
they can be available as plugins or they may be compiled statically
into libraries (e.g., ``libmpi``).
