.. _mpi_abi_get_fortran_booleans:


MPI_Abi_get_fortran_booleans
============================

.. include_body

:ref:`MPI_Abi_get_fortran_booleans` |mdash| Retrieves Fortran ABI details for the provided Fortran logical of size *logical_size*.

.. The following file was automatically generated
.. include:: ./bindings/mpi_abi_get_fortran_booleans.rst

INPUT PARAMETERS
----------------
* ``logical_size``: the size of Fortran logical in bytes

OUTPUT PARAMETERS
-----------------
* ``logical_true``: the Fortran literal value TRUE
* ``logical_false``: the Fortran literal value FALSE
* ``is_set``: flag to indicate whether the logical boolean values were set previously (logical)
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Abi_get_fortran_booleans` returns the values of TRUE and FALSE for a given Fortran
logical size.  *is_set* is set to true if these values have been previously set either
by a call to :ref:`MPI_Abi_set_fortran_booleans` or internally by MPI.

*logical_size* is the size in bytes of a Fortran logical and must be a power of two;
otherwise ``MPI_ERR_ARG`` is returned.

When Open MPI is built with Fortran logical support, this routine
returns Open MPI's configured Fortran ``.TRUE.`` and ``.FALSE.`` values
for recognized logical sizes and sets ``is_set`` to true.  For a
power-of-two logical size that Open MPI cannot map to a configured
Fortran logical representation, the routine returns ``MPI_SUCCESS`` and
sets ``is_set`` to false.

When Open MPI is built without Fortran logical support, this routine
returns values previously supplied with
:ref:`MPI_Abi_set_fortran_booleans`, if any.  If no values have been
supplied for ``logical_size``, the routine returns ``MPI_SUCCESS`` and
sets ``is_set`` to false.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Abi_set_fortran_info`
   * :ref:`MPI_Abi_get_fortran_info`
   * :ref:`MPI_Abi_set_fortran_booleans`
