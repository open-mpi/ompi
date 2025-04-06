.. _mpi_info_get:


MPI_Info_get
============

.. include_body

:ref:`MPI_Info_get` |mdash| Retrieves the value associated with a key in an info
object.

.. The following file was automatically generated
.. include:: ./bindings/mpi_info_get.rst

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
