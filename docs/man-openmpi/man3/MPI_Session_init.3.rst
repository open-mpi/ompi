.. _mpi_session_init:

MPI_Session_init
================

.. include_body

:ref:`MPI_Session_init` |mdash| Creates a new session handle

.. The following file was automatically generated
.. include:: ./bindings/mpi_session_init.rst

INPUT PARAMETERS
----------------

* ``info`` : info object (handle)
* ``errhandler`` : error handler to be attached to the returned session
   (handle)

OUTPUT PARAMETERS
-----------------

* ``session`` : New session (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Session_init` is used to instantiate an MPI Session. The
returned session handle can be used to query the runtime system about
characteristics of the job within which the process is running, as
well as other system resources.  Other MPI communications can also be
initiated in the context of an MPI session handle.  All sessions must
be finalized via :ref:`MPI_Session_finalize` before the MPI process
terminates.

Multiple sessions can be created and destroyed during the lifetime of
an MPI process.  This is different than MPI world model, which can be
initialized at most exactly once (and then subsequently finalized)
during the lifetime of an MPI process.


NOTES
-----

The *info* argument is used to request MPI functionality requirements
and possible MPI implementation specific capabilities.

The *errhandler* argument specifies an error handler to invoke in the
event that the Session instantiation call encounters an error.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Init`
   * :ref:`MPI_Initialized`
   * :ref:`MPI_Init_thread`
   * :ref:`MPI_Finalize`
   * :ref:`MPI_Finalized`
   * :ref:`MPI_Group_from_session_pset`
   * :ref:`MPI_Session_c2f`
   * :ref:`MPI_Session_call_errhandler`
   * :ref:`MPI_Session_create_errhandler`
   * :ref:`MPI_Session_f2c`
   * :ref:`MPI_Session_finalize`
   * :ref:`MPI_Session_get_errhandler`
   * :ref:`MPI_Session_get_info`
   * :ref:`MPI_Session_get_nth_pset`
   * :ref:`MPI_Session_get_num_psets`
   * :ref:`MPI_Session_get_pset_info`
   * :ref:`MPI_Session_init`
   * :ref:`MPI_Session_set_errhandler`
   * :ref:`MPI_T_pvar_session_create`
   * :ref:`MPI_T_pvar_session_free`
