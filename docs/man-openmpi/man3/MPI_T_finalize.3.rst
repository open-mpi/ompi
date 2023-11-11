.. _mpi_t_finalize:


MPI_T_finalize
==============

.. include_body

:ref:`MPI_T_finalize` |mdash| Finalize the MPI tool information interface


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_finalize(void)


DESCRIPTION
-----------

MPI_T_finalize() finalizes the MPI tool information interface and must
be called the same number of times as MPI_T_init_thread() by the end of
execution. Calls to MPI tool functions are allowed at any point in
execution as long as MPI_T_init_thread() has been called at least once
and the number of calls to MPI_T_init_thread() is greater than the
number of calls to MPI_T_finalize(). If at any point in execution the
number of calls to MPI_T_finalize() equals the number of calls to
MPI_T_init_thread() the MPI tool interface will no longer be available
until another call to MPI_T_init_thread().


NOTES
-----

Before the end of execution the number of calls to MPI_T_init_thread()
and :ref:`MPI_T_finalize` must be the same.


ERRORS
------

:ref:`MPI_T_finalize` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized


.. seealso::
   * :ref:`MPI_T_init_thread`
