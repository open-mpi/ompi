.. _mpi_t_pvar_get_index:


MPI_T_pvar_get_index
====================

.. include_body

:ref:`MPI_T_pvar_get_index` |mdash| Query the index of a performance variable from its name

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_pvar_get_index.rst

INPUT PARAMETERS
----------------
* ``name``: Name of the performance variable to query.
* ``var_class``: Class of the performance variable to query.

OUTPUT PARAMETERS
-----------------
* ``pvar_index``: Index of the performance variable.

DESCRIPTION
-----------

:ref:`MPI_T_pvar_get_index` can be used to retrieve the index of a
performance variable given its name and class. The *name* and *var_class*
arguments are provided by the caller |mdash| *name* as a null-terminated
string |mdash| and the matching index is returned in *pvar_index*. A
performance variable is identified by the pair (*name*, *var_class*), so both
must be supplied. The returned index can then be passed to other MPI tool
information interface routines, such as :ref:`MPI_T_pvar_get_info`.

This routine allows a tool to look up a performance variable by name without
iterating over the entire set of performance variables. Because the number of
performance variables exposed by the implementation can change over time, this
is both more convenient and lower overhead than enumerating all of the
variables to find a particular one.


NOTES
-----

Performance variable names are implementation-specific. Looking a variable up
by name is therefore not portable across MPI implementations, but may be the
preferred approach for a tool that targets Open MPI specifically.


ERRORS
------

:ref:`MPI_T_pvar_get_index` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface is not initialized.

* ``MPI_T_ERR_INVALID``: ``name`` or ``pvar_index`` is ``NULL``.

* ``MPI_T_ERR_INVALID_NAME``: ``name`` does not match the name of any
  performance variable of the specified *var_class* provided by the
  implementation at the time of the call.


.. seealso::
   * :ref:`MPI_T_pvar_get_info`
   * :ref:`MPI_T_pvar_get_num`
