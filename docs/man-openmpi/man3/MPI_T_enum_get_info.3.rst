.. _mpi_t_enum_get_info:


MPI_T_enum_get_info
===================

.. include_body

:ref:`MPI_T_enum_get_info` |mdash| Query information about an enumerator


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_enum_get_info(MPI_T_enum enumtype, int *num, char *name, int *name_len)


INPUT PARAMETERS
----------------
* ``enumtype``: Enumerator to be queried.

INPUT/OUTPUT PARAMETERS
-----------------------
* ``name_len``: Length of the string and/or buffer for name.

OUTPUT PARAMETERS
-----------------
* ``num``: number of discrete values represented by this enumeration.
* ``name``: Buffer to return the string containing the name of the category.

DESCRIPTION
-----------

:ref:`MPI_T_enum_get_info` can be used to query information about an
enumerator. The function returns the number of discrete values
represented by this enumerator in the *num* parameter.


NOTES
-----

This MPI tool interface function returns the name of the enumeration as
a string. This function takes two argument for the string: *name* which
specifies a buffer where the name of the should be stored, and
*name_len* which must initially specify the size of the buffer pointed
to by *name*. This function will copy at most *name_len* - 1 characters
of the name and sets *name_len* to the number of characters returned +
1. If *name_len* is NULL or the value specified in *name_len* is 0 the
*name* buffer is ignored and the name of the enumeration is not
returned.


ERRORS
------

:ref:`MPI_T_enum_get_info` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_INDEX``: The enumeration is invalid or has been deleted
