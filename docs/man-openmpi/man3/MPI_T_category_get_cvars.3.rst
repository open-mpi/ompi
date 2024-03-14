.. _mpi_t_category_get_cvars:


MPI_T_category_get_cvars
========================

.. include_body

:ref:`MPI_T_category_get_cvars` |mdash| Query which control variables are in a
category


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_category_get_cvars(int cat_index, int len, int indices[])


INPUT PARAMETERS
----------------
* ``cat_index``: Index of the category to be queried.
* ``len``: The length of the indices array.

OUTPUT PARAMETERS
-----------------
* ``indices``: An integer array of size len, indicating control variable indices.

DESCRIPTION
-----------

:ref:`MPI_T_category_get_cvars` can be used to query which control variables
are contained in a particular category.


ERRORS
------

:ref:`MPI_T_category_get_cvars` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_INDEX``: The category index is invalid
