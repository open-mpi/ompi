.. _mpi_attr_delete:


MPI_Attr_delete
===============

.. include_body

:ref:`MPI_Attr_delete` |mdash| Deletes attribute value associated with a key |mdash| |deprecated_favor| :ref:`MPI_Comm_delete_attr`.

.. The following file was automatically generated
.. include:: ./bindings/mpi_attr_delete.rst


INPUT PARAMETERS
----------------
* ``comm``: Communicator to which attribute is attached (handle).
* ``keyval``: The key value of the deleted attribute (integer).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated* as of MPI-2, and was
*deleted* in MPI-3. Please use :ref:`MPI_Comm_delete_attr`. This function does
not have a mpi_f08 binding.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_delete_attr`
