.. _mpi_f_sync_reg:


MPI_F_sync_reg
==============

.. include_body

:ref:`MPI_F_sync_reg` |mdash| Prevent invalid register optimization of a Fortran buffer

.. The following file was automatically generated
.. include:: ./bindings/mpi_f_sync_reg.rst

INPUT/OUTPUT PARAMETERS
-----------------------
* ``buf``: Initial address of the buffer (choice).

DESCRIPTION
-----------

:ref:`MPI_F_sync_reg` has no executable statements; it exists only to prevent
a Fortran compiler from making invalid assumptions about the contents of a
buffer across an operation that the compiler cannot see. Passing *buf* to this
routine forces the compiler, when necessary, to flush a cached register copy
of the buffer back to memory, or to invalidate a cached register copy so that
the buffer is reloaded from memory on its next use.

This is needed in Fortran code that aggressively optimizes register usage
around nonblocking or one-sided operations, whose completion |mdash| and
therefore whose effect on the buffer |mdash| is not visible to the compiler
from the surrounding code.


NOTES
-----

This routine is provided only in the Fortran bindings; it has no C binding
because it would serve no purpose in C. It also has no *ierror* argument
because there is no operation that can fail.

For example, after an :ref:`MPI_Wait` that completes a nonblocking receive
into *buf*, a call to ``MPI_F_sync_reg(buf)`` ensures that the compiler
reloads *buf* from memory rather than reusing a stale register copy that
predates the receive.


.. seealso::
   * :ref:`MPI_Wait`
