Source code tree layout
=======================

There are a few notable top-level directories in the source
tree:

* The main sub-projects:

    * ``oshmem``: Top-level OpenSHMEM code base
    * ``ompi``: The Open MPI code base
    * ``opal``: The OPAL code base

* ``config``: M4 scripts supporting the top-level ``configure`` script
  ``mpi.h``
* ``etc``: Some miscellaneous text files
* ``docs``: Source code for Open MPI documentation
* ``examples``: Trivial MPI / OpenSHMEM example programs
* ``3rd-party``: Included copies of required core libraries (either
  via Git submodules in Git clones or via binary tarballs).

  .. note:: While it may be considered unusual, we include binary
     tarballs (instead of Git submodules) for 3rd party projects that
     are:

     #. Needed by Open MPI for correct operation, and
     #. Not universally included in OS distributions, and
     #. Rarely updated.

Each of the three main source directories (``oshmem``, ``ompi``, and
``opal``) generate at least a top-level library named ``liboshmem``,
``libmpi``, and ``libopen-pal``, respectively.  They can be built as
either static or shared libraries.  Executables are also produced in
subdirectories of some of the trees.

Each of the sub-project source directories have similar (but not
identical) directory structures under them:

* ``class``: C++-like "classes" (using the OPAL class system)
  specific to this project
* ``include``: Top-level include files specific to this project
* ``mca``: MCA frameworks and components specific to this project
* ``runtime``: Startup and shutdown of this project at runtime
* ``tools``: Executables specific to this project (currently none in
  OPAL)
* ``util``: Random utility code

There are other top-level directories in each of the sub-projects,
each having to do with specific logic and code for that project.  For
example, the MPI API implementations can be found under
``ompi/mpi/LANGUAGE``, where ``LANGUAGE`` is ``c`` or ``fortran``.

The layout of the ``mca`` trees are strictly defined.  They are of the
form:

.. code-block:: text

    PROJECT/mca/FRAMEWORK/COMPONENT

To be explicit: it is forbidden to have a directory under the ``mca``
trees that does not meet this template (with the exception of ``base``
directories, explained below).  Hence, only framework and component
code can be in the ``mca`` trees.

That is, framework and component names must be valid directory names
(and C variables; more on that later).  For example, the TCP BTL
component is located in ``opal/mca/btl/tcp/``.

The name ``base`` is reserved; there cannot be a framework or component
named ``base``. Directories named ``base`` are reserved for the
implementation of the MCA and frameworks.  Here are a few examples (as
of the |ompi_series| source tree):

.. code-block:: sh

    # Main implementation of the MCA
    opal/mca/base

    # Implementation of the btl framework
    opal/mca/btl/base

    # Implementation of the sysv framework
    oshmem/mcs/sshmem/sysv

    # Implementation of the pml framework
    ompi/mca/pml/base

Under these mandated directories, frameworks and/or components may have
arbitrary directory structures, however.
