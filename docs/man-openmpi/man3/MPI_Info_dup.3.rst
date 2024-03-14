.. _mpi_info_dup:


MPI_Info_dup
============

.. include_body

:ref:`MPI_Info_dup` |mdash| Duplicates an info object.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Info_dup(MPI_Info info, MPI_Info *newinfo)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_INFO_DUP(INFO, NEWINFO, IERROR)
   	INTEGER		INFO, NEWINFO, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Info_dup(info, newinfo, ierror)
   	TYPE(MPI_Info), INTENT(IN) :: info
   	TYPE(MPI_Info), INTENT(OUT) :: newinfo
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``info``: Info object (handle).

OUTPUT PARAMETERS
-----------------
* ``newinfo``: Info object (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Info_dup` duplicates an existing info object, creating a new object,
with the same (key,value) pairs and the same ordering of keys.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_create`
   * :ref:`MPI_Info_delete`
   * :ref:`MPI_Info_free`
   * :ref:`MPI_Info_get`
   * :ref:`MPI_Info_set`
