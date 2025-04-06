.. _mpi_t_category_get_events:


MPI_T_category_get_events
=========================

.. include_body

:ref:`MPI_T_category_get_events` |mdash| Query which events are in a
category

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_category_get_events.rst

INPUT PARAMETERS
----------------
* ``cat_index``: Index of the category to be queried.
* ``len``: The length of the indices array.

OUTPUT PARAMETERS
-----------------
* ``indices``: An integer array of size len, indicating event indices.

DESCRIPTION
-----------

:ref:`MPI_T_category_get_events` can be used to query which events
are contained in a particular category.


ERRORS
------

:ref:`MPI_T_category_get_events` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_INDEX``: The category index is invalid
