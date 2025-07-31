.. _mpi_request_get_status_some:


MPI_Request_get_status_some
===========================

.. include_body

:ref:`MPI_Request_get_status_some` |mdash| Access information associated with an
array of requests without freeing the requests.

.. The following file was automatically generated
.. include:: ./bindings/mpi_request_get_status_some.rst

INPUT PARAMETERS
----------------
* ``incount``: List length (non-negative integer).
* ``array_of_requests``: Array of requests (array of handles).

OUTPUT PARAMETERS
-----------------
* ``outcount``: Number of completed requests (integer).
* ``array_of_indices``: Array of indices of operations that completed (array of integers).
* ``array_of_statuses``: Array of ``MPI_Status`` objects for operations that completed (array of status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Request_get_status_some` returns in outcount the number of
requests from the list ``array_of_requests`` that have completed. The
first ``outcount`` locations of the array ``array_of_indices`` and
``array_of_statuses`` will contain the indices of the operations
within the array ``array_of_requests`` and the status of these
operations respectively. The array is indexed from zero in C and from
one in Fortran. It does not deallocate or deactivate the request; a
subsequent call to any of the MPI test, wait, or free routines should be
executed with each completed request.

If no operation in ``array_of_requests`` is complete, it returns
``outcount = 0``. If all operations in ``array_of_requests`` are either
``MPI_REQUEST_NULL`` or inactive, ``outcount`` will be set to ``MPI_UNDEFINED``.

If your application does not need to examine the *status* field, you can
save resources by using the predefined constant ``MPI_STATUSES_IGNORE`` as a
special value for the ``array_of_statuses`` argument.


ERRORS
------

.. include:: ./ERRORS.rst
