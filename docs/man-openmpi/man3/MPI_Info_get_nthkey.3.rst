.. _mpi_info_get_nthkey:


MPI_Info_get_nthkey
===================

.. include_body

:ref:`MPI_Info_get_nthkey` |mdash| Returns the *n*\ th defined key in *info*.

.. The following file was automatically generated
.. include:: ./bindings/mpi_info_get_nthkey.rst

INPUT PARAMETERS
----------------
* ``info``: Info object (handle).
* ``n``: Key number (integer).

OUTPUT PARAMETERS
-----------------
* ``key``: Key (string).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Info_get_nthkey` returns the *n*\ th defined key in *info*. Keys are
numbered 0...\ *N* - 1 where *N* is the value returned by
:ref:`MPI_Info_get_nkeys`. All keys between 0 and *N* - 1 are guaranteed to be
defined. The number of a given key does not change as long as *info* is
not modified with :ref:`MPI_Info_set` or :ref:`MPI_Info_delete`.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_get`
   * :ref:`MPI_Info_get_nkeys`
   * :ref:`MPI_Info_get_valuelen`
