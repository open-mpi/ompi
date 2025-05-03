.. _mpi_type_get_attr:


MPI_Type_get_attr
=================

.. include_body

:ref:`MPI_Type_get_attr` |mdash| Returns the attribute associated with a data
type.

.. The following file was automatically generated
.. include:: ./bindings/mpi_type_get_attr.rst

INPUT PARAMETERS
----------------
* ``type``: Data type to which the attribute is attached (handle).
* ``type_keyval``: Key value (integer).

OUTPUT PARAMETERS
-----------------
* ``attribute_val``: Attribute value, unless *flag* = false
* ``flag``: "false" if no attribute is associated with the key (logical).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

For the given data type, :ref:`MPI_Type_get_attr` returns an attribute value
that corresponds to the specified key value.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_set_attr`
