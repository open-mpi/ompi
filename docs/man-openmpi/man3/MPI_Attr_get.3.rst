.. _mpi_attr_get:


MPI_Attr_get
============

.. include_body

:ref:`MPI_Attr_get` |mdash| Retrieves attribute value by key |mdash| |deprecated_favor| :ref:`MPI_Comm_get_attr`.

.. The following file was automatically generated
.. include:: ./bindings/mpi_attr_get.rst

INPUT PARAMETERS
----------------
* ``comm``: Communicator to which attribute is attached (handle).
* ``keyval``: Key value (integer).

OUTPUT PARAMETERS
-----------------
* ``attribute_val``: Attribute value, unless flag = false.
* ``flag``: True if an attribute value was extracted; false if no attribute is associated with the key.
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated* as of MPI-2, and was
*deleted* in MPI-3. Please use :ref:`MPI_Comm_get_attr`. This function does not
have a mpi_f08 binding.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_get_attr`
