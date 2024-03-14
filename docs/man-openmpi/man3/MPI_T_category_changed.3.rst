.. _mpi_t_category_changed:


MPI_T_category_changed
======================

.. include_body

:ref:`MPI_T_category_changed` |mdash| Get a timestamp for the categories


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_category_changed(int *stamp)


INPUT PARAMETERS
----------------
* ``stamp``: A virtual time stamp to indicate the last change to the categories.

DESCRIPTION
-----------

If two subsequent calls to this routine return the same timestamp, it is
guaranteed that no categories have been changed or added. If the
timestamp from the second call is higher than some categories have been
added or changed.


ERRORS
------

:ref:`MPI_T_category_changed` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized
