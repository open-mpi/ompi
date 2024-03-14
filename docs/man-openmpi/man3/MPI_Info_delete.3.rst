.. _mpi_info_delete:


MPI_Info_delete
===============

.. include_body

:ref:`MPI_Info_delete` |mdash| Deletes a key/value pair from *info*.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Info_delete(MPI_Info info, const char *key)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_INFO_DELETE(INFO, KEY, IERROR)
   	INTEGER		INFO, IERROR
   	CHARACTER*(*)	KEY


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Info_delete(info, key, ierror)
   	TYPE(MPI_Info), INTENT(IN) :: info
   	CHARACTER(LEN=*), INTENT(IN) :: key
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``info``: Info object (handle).

INPUT PARAMETER
---------------
* ``key``: Key (string).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Info_delete` deletes a (key,value) pair from *info*. If *key* is not
defined in *info*, the call raises an error of class MPI_ERR_INFO_NOKEY.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_create`
   * :ref:`MPI_Info_dup`
   * :ref:`MPI_Info_free`
   * :ref:`MPI_Info_get`
   * :ref:`MPI_Info_set`
