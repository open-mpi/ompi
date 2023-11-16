.. _mpi_type_set_name:


MPI_Type_set_name
=================

.. include_body

:ref:`MPI_Type_set_name` |mdash| Sets the name of a data type.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_set_name(MPI_Datatype type, const char *type_name)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_SET_NAME(TYPE, TYPE_NAME, IERROR)
   	INTEGER	TYPE, IERROR
   	CHARACTER*(*) TYPE_NAME


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_set_name(datatype, type_name, ierror)
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	CHARACTER(LEN=*), INTENT(IN) :: type_name
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``type``: Data type for which the identifier is to be set (handle).

INPUT PARAMETER
---------------
* ``type_name``: The character string remembered as the name (string).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_set_name` associates a printable identifier with an MPI data
type.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_get_name`
