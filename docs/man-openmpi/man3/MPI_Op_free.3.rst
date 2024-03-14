.. _mpi_op_free:


MPI_Op_free
===========

.. include_body

:ref:`MPI_Op_free` |mdash| Frees a user-defined combination function handle.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Op_free(MPI_Op *op)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_OP_FREE(OP, IERROR)
   	INTEGER	OP, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Op_free(op, ierror)
   	TYPE(MPI_Op), INTENT(INOUT) :: op
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


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
