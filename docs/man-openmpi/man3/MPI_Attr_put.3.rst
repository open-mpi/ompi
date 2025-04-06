.. _mpi_attr_put:


MPI_Attr_put
============

.. include_body

:ref:`MPI_Attr_put` |mdash| Stores attribute value associated with a key |mdash| |deprecated_favor| :ref:`MPI_Comm_set_attr`.


.. The following file was automatically generated
.. include:: ./bindings/mpi_attr_put.rst

INPUT PARAMETERS
----------------
* ``comm``: Communicator to which attribute will be attached (handle).
* ``keyval``: Key value, as returned by MPI_KEYVAL_CREATE (integer).
* ``attribute_val``: Attribute value.

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated as of MPI-2, and* was
deleted in MPI-3. Please use :ref:`MPI_Comm_set_attr`. This function does not
have a mpi_f08 binding.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_set_attr`
