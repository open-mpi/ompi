.. _mpi_initialized:


MPI_Initialized
===============

.. include_body

:ref:`MPI_Initialized` |mdash| Checks whether MPI has been initialized

.. The following file was automatically generated
.. include:: ./bindings/mpi_initialized.rst

OUTPUT PARAMETERS
-----------------
* ``flag``: True if MPI has been initialized, and false otherwise (logical).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This routine may be used to determine whether MPI has been initialized.
It is one of a small number of routines that may be called before MPI is
initialized and after MPI has been finalized (:ref:`MPI_Finalized` is another).


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Init`
   * :ref:`MPI_Init_thread`
   * :ref:`MPI_Finalize`
   * :ref:`MPI_Finalized`
