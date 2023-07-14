Building MPI applications
=========================

The simplest way to compile and link MPI applications is to use the
Open MPI "wrapper" compilers.

.. toctree::
   :maxdepth: 1

   quickstart
   customizing-wrappers
   extracting-wrapper-flags
   abi-compatibility
   removed-mpi-constructs
   deprecation-warnings
   building-static-apps

.. warning:: Open MPI does not attempt to interoperate with other MPI
             implementations, nor executables that were compiled for
             them.  MPI applications need to be compiled and linked
             with Open MPI in order to run under Open MPI.

             See the :doc:`section on Open MPI's version numbering
             scheme </version-numbering>` for information about binary
             compatibility between different versions of Open MPI for
             applications compiled with another version.
