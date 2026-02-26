.. _mpi_abi_set_fortran_booleans:


MPI_Abi_set_fortran_booleans
============================

.. include_body

:ref:`MPI_Abi_set_fortran_booleans` |mdash| allows the application
to inform the implementation of the literal values of the Fortran booleans.

.. The following file was automatically generated
.. include:: ./bindings/mpi_abi_set_fortran_booleans.rst

INPUT PARAMETERS
----------------
* ``logical_size``: the size of Fortran logical in bytes
* ``logical_true``: the Fortran literal value TRUE
* ``logical_false``: the Fortran literal value FALSE

OUTPUT PARAMETERS
-----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Abi_set_fortran_booleans` allows the application to inform the implementation 
of the literal values of the Fortran booleans.  Prior to setting this information, the application should
check if the logical values are already set using :ref:`MPI_Abi_get_fortran_booleans`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Abi_set_fortran_info`
   * :ref:`MPI_Abi_get_fortran_info`
   * :ref:`MPI_Abi_get_fortran_booleans`
