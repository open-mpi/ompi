.. _mpi_op_commutative:


MPI_Op_commutative
==================

.. include_body

:ref:`MPI_Op_commutative` |mdash| Query of commutativity of reduction operation.

.. The following file was automatically generated
.. include:: ./bindings/mpi_op_commutative.rst

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
