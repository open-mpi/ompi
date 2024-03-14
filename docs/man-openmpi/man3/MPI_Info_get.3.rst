.. _mpi_info_get:


MPI_Info_get
============

.. include_body

:ref:`MPI_Info_get` |mdash| Retrieves the value associated with a key in an info
object.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Info_get(MPI_Info info, const char *key, int valuelen, char *value, int *flag)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_INFO_GET(INFO, KEY, VALUELEN, VALUE, FLAG, IERROR)
   	INTEGER	INFO, VALUELEN, IERROR
   	CHARACTER*(*) KEY, VALUE
   	LOGICAL FLAG


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Info_get(info, key, valuelen, value, flag, ierror)
   	TYPE(MPI_Info), INTENT(IN) :: info
   	CHARACTER(LEN=*), INTENT(IN) :: key
   	INTEGER, INTENT(IN) :: valuelen
   	CHARACTER(LEN=valuelen), INTENT(OUT) :: value
   	LOGICAL, INTENT(OUT) :: flag
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``info``: Info object (handle).
* ``key``: Key (string).
* ``valuelen``: Length of value arg (integer).

OUTPUT PARAMETER
----------------
* ``value``: Value (string).
* ``flag``: Returns true if key defined, false if not (boolean).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Info_get` retrieves the value associated with *key* in a previous
call to :ref:`MPI_Info_set`. If such a key exists, it sets *flag* to true and
returns the value in *value*; otherwise it sets *flag* to false and
leaves *value* unchanged. *valuelen* is the number of characters
available in value. If it is less than the actual size of the value, the
returned value is truncated. In C, *valuelen* should be one less than
the amount of allocated space to allow for the null terminator.

If *key* is larger than MPI_MAX_INFO_KEY, the call is erroneous.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_create`
   * :ref:`MPI_Info_delete`
   * :ref:`MPI_Info_dup`
   * :ref:`MPI_Info_free`
   * :ref:`MPI_Info_get_valuelen`
   * :ref:`MPI_Info_get_nkeys`
   * :ref:`MPI_Info_get_nthkey`
   * :ref:`MPI_Info_set`
