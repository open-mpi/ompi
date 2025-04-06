.. _mpi_session_get_nth_pset:

MPI_Session_get_nth_pset
========================

.. include_body

:ref:`MPI_Session_get_nth_pset` |mdash| Query runtime for name of the nth process set

.. The following file was automatically generated
.. include:: ./bindings/mpi_session_get_nth_pset.rst

INPUT PARAMETERS
----------------

* ``session`` : session (handle)
* ``info`` : info object (handle)
* ``n`` : index of the desired process set name (integer)

INPUT/OUTPUT PARAMETER
^^^^^^^^^^^^^^^^^^^^^^

* ``pset_len`` : length of the pset_name argument (integer)

OUTPUT PARAMETERS
-----------------

* ``pset_name`` : name of the nth process set (string)
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Session_get_nth_pset` returns the name of the nth process set in the
supplied pset_name buffer. pset_len is the size of the buffer needed to
store the nth process set name. If the pset_len passed into the function
is less than the actual buffer size needed for the process set name,
then the string value returned in pset_name is truncated. If pset_len is
set to 0, pset_name is not changed. On return, the value of pset_len
will be set to the required buffer size to hold the process set name. In
C, pset_len includes the required space for the null terminator. In C,
this function returns a null terminated string in all cases where the
pset_len input value is greater than 0.

NOTES
-----

Process set names have an implementation-defined maximum length of
MPI_MAX_PSET_NAME_LEN characters. MPI_MAX_PSET_NAME_LEN shall have a
value of at least 63.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Session_init`
