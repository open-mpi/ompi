.. _mpi_type_set_attr:


MPI_Type_set_attr
=================

.. include_body

:ref:`MPI_Type_set_attr` |mdash| Sets a key value/attribute pair to a data type.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_set_attr(MPI_Datatype type, int type_keyval,
   	void *attribute_val)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_SET_ATTR(TYPE, TYPE_KEYVAL, ATTRIBUTE_VAL, IERROR)
   	INTEGER	TYPE, TYPE_KEYVAL, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) ATTRIBUTE_VAL


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_set_attr(datatype, type_keyval, attribute_val, ierror)
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	INTEGER, INTENT(IN) :: type_keyval
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``type``: Data type to which attribute will be attached (handle).

INPUT PARAMETERS
----------------
* ``type_keyval``: Key value (integer).
* ``attribute_val``: Attribute value.

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

For the given data type, :ref:`MPI_Type_set_attr` sets the key value to the
value of the specified attribute.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_get_attr`
