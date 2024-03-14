.. _mpi_op_commutative:


MPI_Op_commutative
==================

.. include_body

:ref:`MPI_Op_commutative` |mdash| Query of commutativity of reduction operation.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Op_commutative(MPI_Op op, int *commute)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_OP_COMMUTATIVE(OP, COMMUTE, IERROR)
   	LOGICAL	COMMUTE
   	INTEGER	OP, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Op_commutative(op, commute, ierror)
   	TYPE(MPI_Op), INTENT(IN) :: op
   	INTEGER, INTENT(OUT) :: commute
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``op``: Operation (handle).

OUTPUT PARAMETERS
-----------------
* ``commute``: True if op is commutative, false otherwise (logical).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Reduction operations can be queried for their commutativity.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Op_create`
