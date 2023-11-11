.. _mpi_info_get_nkeys:


MPI_Info_get_nkeys
==================

.. include_body

:ref:`MPI_Info_get_nkeys` |mdash| Gets the number of keys currently defined in an
info object.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Info_get_nkeys(MPI_Info info, int *nkeys)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_INFO_GET_NKEYS(INFO, NKEYS, IERROR)
   	INTEGER		INFO, NKEYS, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Info_get_nkeys(info, nkeys, ierror)
   	TYPE(MPI_Info), INTENT(IN) :: info
   	INTEGER, INTENT(OUT) :: nkeys
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``info``: Info object (handle).

OUTPUT PARAMETERS
-----------------
* ``nkeys``: Number of defined keys (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Info_get_nkeys` returns the number of currently defined keys in
*info*.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_get`
   * :ref:`MPI_Info_get_nthkey`
   * :ref:`MPI_Info_get_valuelen`
