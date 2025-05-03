.. _mpi_type_dup:


MPI_Type_dup
============

.. include_body

:ref:`MPI_Type_dup` |mdash| Duplicates a data type with associated key values.

.. The following file was automatically generated
.. include:: ./bindings/mpi_type_dup.rst

INPUT PARAMETER
---------------
* ``type``: Data type (handle).

OUTPUT PARAMETERS
-----------------
* ``newtype``: Copy of *type* (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_dup` is a type constructor that duplicates the existing type
with associated key values. For each key value, the respective copy
callback function determines the attribute value associated with this
key in the new communicator. One particular action that a copy callback
may take is to delete the attribute from the new data type. Returns in
*newtype* a new data type with exactly the same properties as *type*, as
well as any copied cached information. The new data type has identical
upper bound and lower bound and yields the same net result when fully
decoded with the functions described in Section 8.6 of the MPI-2
standard. *newtype* has the same committed state as the old *type*.


NOTES
-----

Note that it is not defined by the MPI standard what happens if the
attribute copy callback invokes other MPI functions. In Open MPI, it is
not valid for attribute copy callbacks (or any of their children) to add
or delete attributes on the same object on which the attribute copy
callback is being invoked.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_create_keyval`
