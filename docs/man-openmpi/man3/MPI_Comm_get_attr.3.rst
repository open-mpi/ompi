.. _mpi_comm_get_attr:


MPI_Comm_get_attr
=================

.. include_body

:ref:`MPI_Comm_get_attr` |mdash| Retrieves attribute value by key.

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_get_attr.rst

INPUT PARAMETERS
----------------
* ``comm``: Communicator to which the attribute is attached (handle).
* ``comm_keyval``: Key value (integer).

OUTPUT PARAMETER
----------------
* ``attribute_val``: Attribute value, unless f\ *lag* = false.
* ``flag``: False if no attribute is associated with the key (logical).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_get_attr` retrieves an attribute value by key. The call is
erroneous if there is no key with value *keyval*. On the other hand, the
call is correct if the key value exists, but no attribute is attached on
*comm* for that key; in that case, the call returns *flag* = false. In
particular, ``MPI_KEYVAL_INVALID`` is an erroneous key value.

This function replaces :ref:`MPI_Attr_get`, the use of which is deprecated. The
C binding is identical. The Fortran binding differs in that
*attribute_val* is an address-sized integer.


ERRORS
------

.. include:: ./ERRORS.rst
