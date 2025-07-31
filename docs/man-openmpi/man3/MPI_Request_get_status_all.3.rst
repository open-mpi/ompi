.. _mpi_request_get_status_all:


MPI_Request_get_status_all
==========================

.. include_body

:ref:`MPI_Request_get_status_all` |mdash| Access information associated with an
array of requests without freeing the requests.

.. The following file was automatically generated
.. include:: ./bindings/mpi_request_get_status_all.rst

INPUT PARAMETERS
----------------
* ``count``: List length (non-negative integer)
* ``array_of_requests``: Array of requests (array of handles).

OUTPUT PARAMETERS
-----------------
* ``flag``: Boolean flag, same as from :ref:`MPI_Test` (logical).
* ``array_of_statuses``: Array of ``MPI_Status`` objects if flag is true (array of status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Request_get_status_all` sets ``flag = true`` if all
operations associated with *active* handles in the array have completed.
In this case, each status entry that corresponds to an active request
is set to the status of the corresponding operation. It
does not deallocate or deactivate the request; a subsequent call to
any of the MPI test, wait, or free routines should be executed with each
of those requests.

Each status entry that corresponds to a null or inactive handle is set
to empty.  Otherwise, ``flag = false`` is returned and the values of the
status entries are undefined.

If your application does not need to examine the *status* field, you can
save resources by using the predefined constant ``MPI_STATUSES_IGNORE`` as a
special value for the ``array_of_statuses`` argument.


ERRORS
------

.. include:: ./ERRORS.rst
