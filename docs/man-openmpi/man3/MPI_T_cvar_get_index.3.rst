.. _mpi_t_cvar_get_index:


MPI_T_cvar_get_index
====================

.. include_body

:ref:`MPI_T_cvar_get_index` |mdash| Query the index of a control variable from its name

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_cvar_get_index.rst

INPUT PARAMETERS
----------------
* ``name``: Name of the control variable to query.

OUTPUT PARAMETERS
-----------------
* ``cvar_index``: Index of the control variable.

DESCRIPTION
-----------

:ref:`MPI_T_cvar_get_index` can be used to retrieve the index of a control
variable given its name. The *name* argument is provided by the caller as a
null-terminated string, and the matching index is returned in *cvar_index*.
The returned index can then be passed to other MPI tool information
interface routines, such as :ref:`MPI_T_cvar_get_info`. Control variables in
Open MPI are the same as MCA parameters.

This routine allows a tool to look up a control variable by name without
iterating over the entire set of control variables. Because the number of
control variables exposed by the implementation can change over time, this is
both more convenient and lower overhead than enumerating all of the variables
to find a particular one.


NOTES
-----

Control variable names are implementation-specific. Looking a variable up by
name is therefore not portable across MPI implementations, but may be the
preferred approach for a tool that targets Open MPI specifically.


ERRORS
------

:ref:`MPI_T_cvar_get_index` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface is not initialized.

* ``MPI_T_ERR_INVALID``: ``name`` or ``cvar_index`` is ``NULL``.

* ``MPI_T_ERR_INVALID_NAME``: ``name`` does not match the name of any control
  variable provided by the implementation at the time of the call.


.. seealso::
   * :ref:`MPI_T_cvar_get_info`
   * :ref:`MPI_T_cvar_get_num`
