.. _mpi_comm_set_attr:


MPI_Comm_set_attr
=================

.. include_body

:ref:`MPI_Comm_set_attr` |mdash| Stores attribute value associated with a key.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_set_attr(MPI_Comm comm, int comm_keyval, void *attribute_val)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_SET_ATTR(COMM, COMM_KEYVAL, ATTRIBUTE_VAL, IERROR)
   	INTEGER	COMM, COMM_KEYVAL, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) ATTRIBUTE_VAL


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_set_attr(comm, comm_keyval, attribute_val, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, INTENT(IN) :: comm_keyval
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``comm``: Communicator from which attribute will be attached (handle).

INPUT PARAMETERS
----------------
* ``comm_keyval``: Key value (integer).
* ``attribute_val``: Attribute value.

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_set_attr` stores the stipulated attribute value *attribute_val*
for subsequent retrieval by :ref:`MPI_Comm_get_attr`. If the value is already
present, then the outcome is as if :ref:`MPI_Comm_delete_attr` was first called
to delete the previous value (and the callback function ``delete_fn`` was
executed), and a new value was next stored. The call is erroneous if
there is no key with value *comm_keyval*; in particular
``MPI_KEYVAL_INVALID`` is an erroneous key value. The call will fail if the
delete_fn function returned an error code other than ``MPI_SUCCESS``.

This function replaces :ref:`MPI_Attr_put`, the use of which is deprecated. The
C binding is identical. The Fortran binding differs in that
*attribute_val* is an address-sized integer.


NOTES
-----

Values of the permanent attributes ``MPI_TAG_UB``, ``MPI_HOST``, ``MPI_IO``, and
``MPI_WTIME_IS_GLOBAL`` may not be changed.

The type of the attribute value depends on whether C or Fortran is being
used. In C, an attribute value is a pointer (``void *``); in Fortran, it is
a single, address-size integer system for which a pointer does not fit
in an integer.

If an attribute is already present, the delete function (specified when
the corresponding keyval was created) will be called.


ERRORS
------

.. include:: ./ERRORS.rst
