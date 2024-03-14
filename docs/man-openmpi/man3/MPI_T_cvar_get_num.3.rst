.. _mpi_t_cvar_get_num:


MPI_T_cvar_get_num
==================

.. include_body

:ref:`MPI_T_cvar_get_num` |mdash| Query the number of control variables


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_cvar_get_num(int *num_cvar)


OUTPUT PARAMETERS
-----------------
* ``num_cvar``: Current number of control variables.

DESCRIPTION
-----------

:ref:`MPI_T_cvar_get_num` can be used to query the current number of control
variables. The number of control variables may increase throughout the
execution of the process but will never decrease.


ERRORS
------

:ref:`MPI_T_cvar_get_num` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized
