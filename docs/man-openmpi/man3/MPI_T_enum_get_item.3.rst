.. _mpi_t_enum_get_item:


MPI_T_enum_get_item
===================

.. include_body

:ref:`MPI_T_enum_get_item` |mdash| Query information about an enumerator


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_enum_get_item(MPI_T_enum enumtype, int index, int *value, char *name,
                           int *name_len)


INPUT PARAMETERS
----------------
* ``enumtype``: Enumeration to be queried.
* ``index``: Number of the value to be queried in this enumeration.

INPUT/OUTPUT PARAMETERS
-----------------------
* ``name_len``: Length of the string and/or buffer for name.

OUTPUT PARAMETERS
-----------------
* ``value``: Variable value.
* ``name``: Buffer to return the string containing the name of the category.

DESCRIPTION
-----------

:ref:`MPI_T_enum_get_item` can be used to query information about an item in an
enumerator. This function returns the enumeration value in the *value*
parameter.


NOTES
-----

This MPI tool interface function returns the name of the item as a
string. This function takes two arguments for the string: a buffer to
store the string, and a length which must initially specify the size
of the buffer. If the length passed is n then this function will copy
at most n - 1 characters of the string into the buffer and sets the
length to the number of characters copied - 1. If the length argument
is NULL or the value specified in the length is 0 the string buffer is
ignored and the string is not returned. For more information see MPI-3
section 14.3.3.


ERRORS
------

:ref:`MPI_T_enum_get_item` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_INDEX``: The enumeration is invalid or has been deleted
