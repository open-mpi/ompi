.. _mpi_request_get_status_any:


MPI_Request_get_status_any
==========================

.. include_body

:ref:`MPI_Request_get_status_any` |mdash| Access information associated with an
array of requests without freeing the requests.

.. The following file was automatically generated
.. include:: ./bindings/mpi_request_get_status_any.rst

INPUT PARAMETERS
----------------
* ``count``: List length (non-negative integer)
* ``array_of_requests``: Array of requests (array of handles).

OUTPUT PARAMETERS
-----------------
* ``index``: Index of operation that completed (integer).
* ``flag``: Boolean flag, same as from :ref:`MPI_Test` (logical).
* ``status``: ``MPI_Status`` object if flag is true (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Request_get_status_any` sets ``flag = true`` if either one
of the operations associated with active handles has completed. In
this case it returns in ``index`` the index of this request in the
array and the status of the operation in ``status``.  It does not
deallocate or deactivate the request; a subsequent call to any of the MPI
test, wait, or free routines should be executed with that request.

If no operation completed, it returns ``flag = false`` and a value of
``MPI_UNDEFINED`` in ``index``. ``status`` is undefined in this
scenario.

If ``array_of_requests`` contains no active handles then the call
returns immediately with ``flag = true``, ``index = MPI_UNDEFINED``,
and an empty status.

If your application does not need to examine the *status* field, you can
save resources by using the predefined constant ``MPI_STATUS_IGNORE`` as a
special value for the ``status`` argument.


ERRORS
------

.. include:: ./ERRORS.rst
