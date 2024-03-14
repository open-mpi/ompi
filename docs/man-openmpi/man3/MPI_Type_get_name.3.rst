.. _mpi_type_get_name:


MPI_Type_get_name
=================

.. include_body

:ref:`MPI_Type_get_name` |mdash| Gets the name of a data type.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_get_name(MPI_Datatype type, char *type_name,
   	int *resultlen)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_GET_NAME(TYPE, TYPE_NAME, RESULTLEN, IERROR)
   	INTEGER	TYPE, RESULTLEN, IERROR
   	CHARACTER*(*) TYPE_NAME


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_get_name(datatype, type_name, resultlen, ierror)
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	CHARACTER(LEN=MPI_MAX_OBJECT_NAME), INTENT(OUT) :: type_name
   	INTEGER, INTENT(OUT) :: resultlen
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``type``: Data type whose name is to be returned (handle).

OUTPUT PARAMETERS
-----------------
* ``type_name``: The name previously stored on the data type, or an empty string if not such name exists (string).
* ``resultlen``: Length of returned name (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_get_name` returns the printable identifier associated with an
MPI data type.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_set_name`
