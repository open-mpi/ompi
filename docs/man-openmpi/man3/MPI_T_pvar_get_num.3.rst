.. _mpi_t_pvar_get_num:


MPI_T_pvar_get_num
==================

.. include_body

:ref:`MPI_T_pvar_get_num` |mdash| Query the number of performance variables


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_pvar_get_num(int *num_pvar)


OUTPUT PARAMETERS
-----------------
* ``num_pvar``: Current number of performance variables.

DESCRIPTION
-----------

:ref:`MPI_T_pvar_get_num` can be used to query the current number of
performance variables. The number of performance variables may increase
throughout the execution of the process but will never decrease.


ERRORS
------

:ref:`MPI_T_pvar_get_num` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized
