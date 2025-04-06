.. _mpi_type_delete_attr:


MPI_Type_delete_attr
====================

.. include_body

:ref:`MPI_Type_delete_attr` |mdash| Deletes a datatype-caching attribute value
associated with a key.

.. The following file was automatically generated
.. include:: ./bindings/mpi_type_delete_attr.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``type``: Data type from which the attribute is deleted (handle).n

INPUT PARAMETER
---------------
* ``type_keyval``: Key value (integer).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_delete_attr` deletes a datatype-caching attribute value
associated with a key. This routines partially replaces :ref:`MPI_Attr_delete`,
which is now deprecated.


NOTES
-----

Note that it is not defined by the MPI standard what happens if the
delete_fn callback invokes other MPI functions. In Open MPI, it is not
valid for delete_fn callbacks (or any of their children) to add or
delete attributes on the same object on which the delete_fn callback is
being invoked.


ERRORS
------

.. include:: ./ERRORS.rst
