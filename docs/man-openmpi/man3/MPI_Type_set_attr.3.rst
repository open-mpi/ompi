.. _mpi_type_set_attr:


MPI_Type_set_attr
=================

.. include_body

:ref:`MPI_Type_set_attr` |mdash| Sets a key value/attribute pair to a data type.

.. The following file was automatically generated
.. include:: ./bindings/mpi_type_set_attr.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``type``: Data type to which attribute will be attached (handle).

INPUT PARAMETERS
----------------
* ``type_keyval``: Key value (integer).
* ``attribute_val``: Attribute value.

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

For the given data type, :ref:`MPI_Type_set_attr` sets the key value to the
value of the specified attribute.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_get_attr`
