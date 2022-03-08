.. _label-quickstart-building-open-mpi:

Quick start: Installing Open MPI
================================

Although this section skips many details, it offers examples that will
probably work in many environments.

.. caution:: Note that this section is a "Quick start" |mdash| it does
   not attempt to be comprehensive or describe how to build Open MPI
   in all supported environments.  The examples below may therefore
   not work exactly as shown in your environment.

   Please consult the other sections in this chapter for more details,
   if necessary.

.. important:: If you have checked out a *developer's copy* of Open MPI
   (i.e., you cloned from Git), you really need to read :doc:`the
   Developer's Guide </developers/index>` before attempting to build Open
   MPI. Really.

Open MPI uses a traditional ``configure`` script paired with ``make``
to build.  Typical installs can be of the pattern:

.. code-block:: sh

   shell$ tar xf openmpi-<version>.tar.bz2
   shell$ cd openmpi-<version>
   shell$ ./configure --prefix=<path> [...options...] 2>&1 | tee config.out
   <... lots of output ...>

   # Use an integer value of N for parallel builds
   shell$ make [-j N] all 2>&1 | tee make.out

   # ...lots of output...

   # Depending on the <prefix> chosen above, you may need root access
   # for the following:
   shell$ make install 2>&1 | tee install.out

   # ...lots of output...

Note that VPATH builds are fully supported.  For example:

.. code-block:: sh

   shell$ tar xf openmpi-<version>.tar.bz2
   shell$ cd openmpi-<version>
   shell$ mkdir build
   shell$ cd build
   shell$ ../configure --prefix=<path> 2>&1 | tee config.out
   # ...etc.

The above patterns can be used in many environments.

Note that there are many, many configuration options available in the
``./configure`` step.  Some of them may be needed for your particular
HPC network interconnect type and/or computing environmnet; see the
rest of this chapter for desciptions of the available options.
