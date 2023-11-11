.. _mpi_info_get_valuelen:


MPI_Info_get_valuelen
=====================

.. include_body

:ref:`MPI_Info_get_valuelen` |mdash| Retrieves the length of the key value
associated with an info object.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Info_get_valuelen(MPI_Info info, const char *key,
   	int *valuelen, int *flag)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_INFO_GET_VALUELEN(INFO, KEY, VALUELEN, FLAG, IERROR)
   	INTEGER		INFO, VALUELEN, IERROR
   	LOGICAL		FLAG
   	CHARACTER*(*)	KEY


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Info_get_valuelen(info, key, valuelen, flag, ierror)
   	TYPE(MPI_Info), INTENT(IN) :: info
   	CHARACTER(LEN=*), INTENT(IN) :: key
   	INTEGER, INTENT(OUT) :: valuelen
   	LOGICAL, INTENT(OUT) :: flag
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``info``: Info object (handle).
* ``key``: Key (string).

OUTPUT PARAMETERS
-----------------
* ``valuelen``: Length of value arg (integer).
* ``flag``: Returns true if key defined, false if not (boolean).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Info_get_valuelen` retrieves the length of the *value* associated
with *key*. If *key* is defined, *valuelen* is set to the length of its
associated value and *flag* is set to true. If *key* is not defined,
*valuelen* is not touched and *flag* is set to false. The length
returned in C does not include the end-of-string character.

If *key* is larger than MPI_MAX_INFO_KEY, the call is erroneous.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_get`
   * :ref:`MPI_Info_get_nkeys`
   * :ref:`MPI_Info_get_nthkey`
