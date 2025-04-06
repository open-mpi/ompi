.. _mpi_finalized:

MPI_Finalized
=============

.. include_body

:ref:`MPI_Finalized` |mdash| Checks whether MPI has been finalized

.. The following file was automatically generated
.. include:: ./bindings/mpi_finalized.rst

OUTPUT PARAMETER
----------------

* ``flag`` : True if MPI was finalized, and false otherwise (logical).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

This routine may be used to determine whether MPI has been finalized. It
is one of a small number of routines that may be called before MPI is
initialized and after MPI has been finalized (:ref:`MPI_Initialized` is
another).

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Init`
