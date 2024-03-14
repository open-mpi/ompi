.. _building-open-mpi-vpath-label:

VPATH builds
============

VPATH build are fully supported. For example:

.. code-block:: sh

   shell$ tar xf openmpi-VERSION.tar.bz2
   shell$ cd openmpi-VERSION
   shell$ mkdir build
   shell$ cd build
   shell$ ../configure [...options...]
   <... lots of output ...>
   shell$ make -j 4
