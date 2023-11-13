.. _mpi_comm_get_attr:


MPI_Comm_get_attr
=================

.. include_body

:ref:`MPI_Comm_get_attr` |mdash| Retrieves attribute value by key.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_get_attr(MPI_Comm comm, int comm_keyval,
   	void *attribute_val, int *flag)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_GET_ATTR(COMM, COMM_KEYVAL, ATTRIBUTE_VAL, FLAG, IERROR)
   	INTEGER	COMM, COMM_KEYVAL, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) ATTRIBUTE_VAL
   	LOGICAL FLAG


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_get_attr(comm, comm_keyval, attribute_val, flag, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, INTENT(IN) :: comm_keyval
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val
   	LOGICAL, INTENT(OUT) :: flag
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``comm``: Communicator to which the attribute is attached (handle).
* ``comm_keyval``: Key value (integer).

OUTPUT PARAMETER
----------------
* ``attribute_val``: Attribute value, unless f\ *lag* = false.
* ``flag``: False if no attribute is associated with the key (logical).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_get_attr` retrieves an attribute value by key. The call is
erroneous if there is no key with value *keyval*. On the other hand, the
call is correct if the key value exists, but no attribute is attached on
*comm* for that key; in that case, the call returns *flag* = false. In
particular, ``MPI_KEYVAL_INVALID`` is an erroneous key value.

This function replaces :ref:`MPI_Attr_get`, the use of which is deprecated. The
C binding is identical. The Fortran binding differs in that
*attribute_val* is an address-sized integer.


ERRORS
------

.. include:: ./ERRORS.rst
