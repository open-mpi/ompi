.. _mpi_t_category_get_num_events:


MPI_T_category_get_num_events
=============================

.. include_body

:ref:`MPI_T_category_get_num_events` |mdash| Query returns the number of event types contained
in the queried category.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_category_get_num_events(int cat_index, int *num_events)


INPUT PARAMETERS
----------------
* ``cat_index``: Index of the category to be queried

OUTPUT PARAMETERS
-----------------
* ``num_events``: Number of event types in the category

DESCRIPTION
-----------

:ref:`MPI_T_category_get_num_events` can be used to query the number of events
contained in the category.


ERRORS
------

:ref:`MPI_T_category_get_num_events` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized
