.. _mpi_type_delete_attr:


MPI_Type_delete_attr
====================

.. include_body

:ref:`MPI_Type_delete_attr` |mdash| Deletes a datatype-caching attribute value
associated with a key.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_delete_attr(MPI_Datatype type, int type_keyval)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_DELETE_ATTR(TYPE, TYPE_KEYVAL, IERROR)
   	INTEGER	TYPE, TYPE_KEYVAL, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_delete_attr(datatype, type_keyval, ierror)
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	INTEGER, INTENT(IN) :: type_keyval
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


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
