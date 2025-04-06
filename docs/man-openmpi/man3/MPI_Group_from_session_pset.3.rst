.. _mpi_group_from_session_pset:

MPI_Group_from_session_pset
===========================

.. include_body

:ref:`MPI_Group_from_session_pset` |mdash| Creates a group using a provided session
handle and process set.

.. The following file was automatically generated
.. include:: ./bindings/mpi_group_from_session_pset.rst

INPUT PARAMETERS
----------------

* ``session`` : Session (handle).
* ``pset_name`` : name of process set to use to create the new group
   (string)

OUTPUT PARAMETERS
-----------------

* ``newgroup`` : New group derived from supplied session and process set
   (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

The function :ref:`MPI_Group_from_session_pset` creates a group newgroup using
the provided session handle and process set. The process set name must
be one returned from an invocation of :ref:`MPI_Session_get_nth_pset` using the
supplied session handle. If the pset_name does not exist, MPI_GROUP_NULL
will be returned in the newgroup argument.

NOTE
----

As with other group constructors, :ref:`MPI_Group_from_session_pset` is a local
function.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Session_init`
