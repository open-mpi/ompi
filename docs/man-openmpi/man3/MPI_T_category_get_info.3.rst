.. _mpi_t_category_get_info:


MPI_T_category_get_info
=======================

.. include_body

:ref:`MPI_T_category_get_info` |mdash| Query information from a category


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_category_get_info(int cat_index, char *name, int *name_len,
   char *desc, int *desc_len, int *num_cvars, int *num_pvars,
   int *num_categories)


INPUT PARAMETERS
----------------
* ``cat_index``: Index of the category to be queried.

INPUT/OUTPUT PARAMETERS
-----------------------
* ``name_len``: Length of the string and/or buffer for name.
* ``desc_len``: Length of the string and/or buffer for desc.

OUTPUT PARAMETERS
-----------------
* ``name``: Buffer to return the string containing the name of the category.
* ``desc``: Buffer to return the string containing the description of the category.
* ``num_cvars``: Number of control variables in the category.
* ``num_pvars``: Number of performance variables in the category.
* ``num_categories``: Number of categories contained in the category.

DESCRIPTION
-----------

:ref:`MPI_T_category_get_info` can be used to query information from a
category. The function returns the number of control variables,
performance variables, and sub-categories in the queried category in the
arguments *num_cvars*, *num_pvars*, and *num_categories*, respectively.


NOTES
-----

This MPI tool interface function returns two strings. This function
takes two argument for each string: a buffer to store the string, and a
length which must initially specify the size of the buffer. If the
length passed is n then this function will copy at most n - 1 characters
of the string into the corresponding buffer and set the length to the
number of characters copied - 1. If the length argument is NULL or the
value specified in the length is 0 the corresponding string buffer is
ignored and the string is not returned.


ERRORS
------

:ref:`MPI_T_category_get_info` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_INDEX``: The category index is invalid
