.. _mpi_info_get_valuelen:


MPI_Info_get_valuelen
=====================

.. include_body

:ref:`MPI_Info_get_valuelen` |mdash| Retrieves the length of the key value
associated with an info object.

.. The following file was automatically generated
.. include:: ./bindings/mpi_info_get_valuelen.rst

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
