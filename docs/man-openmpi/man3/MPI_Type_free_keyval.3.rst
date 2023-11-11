.. _mpi_type_free_keyval:


MPI_Type_free_keyval
====================

.. include_body

:ref:`MPI_Type_free_keyval` |mdash| Frees a previously created type key value.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_free_keyval(int *type_keyval)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_FREE_KEYVAL(TYPE_KEYVAL, IERROR)
   	INTEGER	TYPE_KEYVAL, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_free_keyval(type_keyval, ierror)
   	INTEGER, INTENT(INOUT) :: type_keyval
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``type_keyval``: Key value to free (integer).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_create_keyval`
