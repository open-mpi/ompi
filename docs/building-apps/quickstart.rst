.. _label-quickstart-building-apps:

Quick start: Building MPI applications
======================================

Although this section skips many details, it offers examples that will
probably work in many environments.

.. caution:: Note that this section is a "Quick start" |mdash| it does
   not attempt to be comprehensive or describe how to build Open MPI
   in all supported environments.  The examples below may therefore
   not work exactly as shown in your environment.

   Please consult the other sections in this chapter for more details,
   if necessary.

Open MPI provides "wrapper" compilers that should be used for
compiling MPI and OpenSHMEM applications:

+---------+-----------------------------------+
| C       | :ref:`mpicc(1) <man1-mpicc>`,     |
|         | :ref:`oshcc(1) <man1-oshcc>`      |
+---------+-----------------------------------+
| C++     | :ref:`mpic++(1) <man1-mpic++>`,   |
|         | :ref:`oshc++(1) <man1-oshc++>`    |
|         | (and :ref:`mpiCC(1) <man1-mpiCC>` |
|         | and :ref:`oshCC(1) <man1-oshCC>`  |
|         | if you filesystem is              |
|         | case-sensitive)                   |
+---------+-----------------------------------+
| Fortran | :ref:`mpifort(1) <man1-mpifort>`, |
|         | :ref:`oshfort(1) <man1-oshfort>`  |
+---------+-----------------------------------+

.. caution:: The legacy names ``mpif77`` and ``mpif90`` still exist,
             and are simply symbolic links to the ``mpifort`` wrapper
             compiler.  Users are strongly encouraged to update all
             build scripts to use ``mpifort`` instead of ``mpif77``
             and ``mpif90``.

The intent is that users can simply invoke the Open MPI wrapper
compiler instead of their usual language compiler.  For example,
instead of invoking your usual C compiler to build your MPI C
appliance, use ``mpicc``:

.. code-block:: sh

   shell$ mpicc hello_world_mpi.c -o hello_world_mpi -g
   shell$

For OpenSHMEM applications:

.. code-block:: sh

   shell$ oshcc hello_shmem.c -o hello_shmem -g
   shell$

All the wrapper compilers do is add a variety of compiler and linker
flags to the command line and then invoke a back-end compiler.  To be
specific: the wrapper compilers do not parse source code at all; they
are solely command-line manipulators, and have nothing to do with the
actual compilation or linking of programs.  The end result is an MPI
executable that is properly linked to all the relevant libraries.

.. caution:: It is *absolutely not sufficient* to simply add ``-lmpi``
             to your link line and assume that you will obtain a valid
             Open MPI executable.
