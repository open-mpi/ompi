.. _mpi_session_get_pset_info:

MPI_Session_get_pset_info
=========================

.. include_body

:ref:`MPI_Session_get_pset_info` |mdash| Returns an info object containing properties
of a specific process set

.. The following file was automatically generated
.. include:: ./bindings/mpi_session_get_pset_info.rst

INPUT PARAMETERS
----------------

* ``session`` : session (handle)
* ``pset_name`` : name of process set (string)

OUTPUT PARAMETERS
-----------------

* ``info`` : info object (handle)
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Session_get_pset_info` is used to query properties of a specific
process set. The returned info object can be queried with existing MPI
info object query functions. One key/value pair must be defined,
"mpi_size". The value of the "mpi_size" key specifies the number of MPI
processes in the process set.

NOTES
-----

The user is responsible for freeing the returned info object via
:ref:`MPI_Info_free`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Session_init`
