.. _mpi_op_free:


MPI_Op_free
===========

.. include_body

:ref:`MPI_Op_free` |mdash| Frees a user-defined combination function handle.

.. The following file was automatically generated
.. include:: ./bindings/mpi_op_free.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``op``: Operation (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Marks a user-defined reduction operation for deallocation and sets *op*
to MPI_OP_NULL.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Op_create`
   * :ref:`MPI_Reduce`
   * :ref:`MPI_Allreduce`
   * :ref:`MPI_Reduce_scatter`
   * :ref:`MPI_Scan`
