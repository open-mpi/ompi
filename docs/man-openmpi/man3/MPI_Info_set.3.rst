.. _mpi_info_set:


MPI_Info_set
============

.. include_body

:ref:`MPI_Info_set` |mdash| Adds a key/value pair to *info*.

.. The following file was automatically generated
.. include:: ./bindings/mpi_info_set.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``info``: Info object (handle).

INPUT PARAMETERS
----------------
* ``key``: Key (string).
* ``value``: Value (string).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Info_set` adds the (key,value) pair to *info* and overrides the value
if a value for the same key was previously set. The *key* and *value*
parameters are null-terminated strings in C. In Fortran, leading and
trailing spaces in *key* and *value* are stripped. If either *key* or
*value* is larger than the allowed maximums, the error MPI_ERR_INFO_KEY
or MPI_ERR_INFO_VALUE is raised, respectively.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_create`
   * :ref:`MPI_Info_delete`
   * :ref:`MPI_Info_dup`
   * :ref:`MPI_Info_free`
   * :ref:`MPI_Info_set`
