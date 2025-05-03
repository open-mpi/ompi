.. _mpi_free_mem:

MPI_Free_mem
============

.. include_body

:ref:`MPI_Free_mem` |mdash| Frees memory that has been allocated using :ref:`MPI_Alloc_mem`.

.. The following file was automatically generated
.. include:: ./bindings/mpi_free_mem.rst

INPUT PARAMETER
---------------

* ``base`` : Initial address of memory segment allocated by :ref:`MPI_Alloc_mem`
   (choice).

OUTPUT PARAMETER
----------------

* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Free_mem` frees memory that has been allocated by :ref:`MPI_Alloc_mem`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Alloc_mem`
