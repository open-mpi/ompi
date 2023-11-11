.. _mpi_t_category_get_num:


MPI_T_category_get_num
======================

.. include_body

:ref:`MPI_T_category_get_num` |mdash| Query the number of categories


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_category_get_num(int *num_cat)


OUTPUT PARAMETERS
-----------------
* ``num_cat``: Current number of categories

DESCRIPTION
-----------

:ref:`MPI_T_category_get_num` can be used to query the current number of
categories.


ERRORS
------

:ref:`MPI_T_category_get_num` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized
