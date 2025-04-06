.. _mpi_file_set_atomicity:


MPI_File_set_atomicity
======================

.. include_body

:ref:`MPI_File_set_atomicity` |mdash| Sets consistency semantics for data-access
operations (collective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_set_atomicity.rst

INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``flag``: **true** to enable atomic mode, **false** to enable nonatomic mode (boolean).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The consistency semantics for data-access operations using the set of
file handles created by one collective :ref:`MPI_File_open` is set by
collectively calling :ref:`MPI_File_set_atomicity`. All processes in the group
must pass identical values for *fh* and *flag.* If *flag* is *true,*
atomic mode is set; if *flag* is *false,* nonatomic mode is set.

The default value on a call to :ref:`MPI_File_open` in Open MPI is *true* for
jobs running on more than one node, *false* for jobs running on a single
SMP. For more information, see the MPI-2 standard.


ERRORS
------

.. include:: ./ERRORS.rst
