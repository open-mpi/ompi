MPI Collectives
===============

* The ``cuda`` coll component provides CUDA-aware support for the
  reduction type collectives with GPU buffers. This component is only
  compiled into the library when the library has been configured with
  CUDA-aware support.  It intercepts calls to the reduction
  collectives, copies the data to staging buffers if GPU buffers, then
  calls underlying collectives to do the work.
