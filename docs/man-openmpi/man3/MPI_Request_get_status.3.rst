.. _mpi_request_get_status:


MPI_Request_get_status
======================

.. include_body

:ref:`MPI_Request_get_status` |mdash| Access information associated with a
request without freeing the request.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Request_get_status(MPI_Request request, int *flag, MPI_Status *status)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_REQUEST_GET_STATUS(REQUEST, FLAG, STATUS, IERROR)
   	INTEGER	REQUEST, STATUS(MPI_STATUS_SIZE), IERROR
   	LOGICAL	FLAG


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Request_get_status(request, flag, status, ierror)
   	TYPE(MPI_Request), INTENT(IN) :: request
   	LOGICAL, INTENT(OUT) :: flag
   	TYPE(MPI_Status) :: status
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``request``: Communication request (handle).

OUTPUT PARAMETERS
-----------------
* ``flag``: Boolean flag, same as from :ref:`MPI_Test` (logical).
* ``status``: ``MPI_Status`` object if flag is true (status).

DESCRIPTION
-----------

:ref:`MPI_Request_get_status` sets ``flag = true`` if the operation is
complete or sets ``flag = false`` if it is not complete. If the
operation is complete, it returns in *status* the request status. It
does not deallocate or deactivate the request; a subsequent call to
test, wait, or free should be executed with that request.

If your application does not need to examine the *status* field, you can
save resources by using the predefined constant ``MPI_STATUS_IGNORE`` as a
special value for the *status* argument.


ERRORS
------

.. include:: ./ERRORS.rst
