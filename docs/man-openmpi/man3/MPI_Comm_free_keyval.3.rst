.. _mpi_comm_free_keyval:


MPI_Comm_free_keyval
====================

.. include_body

:ref:`MPI_Comm_free_keyval` |mdash| Frees attribute key for communicator cache
attribute.

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_free_keyval.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``comm_keyval``:

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_free_keyval` frees an extant attribute key. This function sets
the value of *keyval* to MPI_KEYVAL_INVALID. Note that it is not
erroneous to free an attribute key that is in use, because the actual
free does not transpire until after all references (in other
communicators on the process) to the key have been freed. These
references need to be explicitly freed by the program, either via calls
to :ref:`MPI_Comm_delete_attr` that free one attribute instance, or by calls to
:ref:`MPI_Comm_free` that free all attribute instances associated with the
freed communicator.

This call is identical to the call :ref:`MPI_Keyval_free` but is needed to
match the communicator-specific creation function introduced in the
MPI-2 standard. The use of :ref:`MPI_Keyval_free` is deprecated.


NOTES
-----

Key values are global (they can be used with any and all communicators).


ERRORS
------
.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_create_keyval`
