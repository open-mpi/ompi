.. _mpi_status_set_cancelled:


MPI_Status_set_cancelled
========================

.. include_body

:ref:`MPI_Status_set_cancelled` |mdash| Sets *status* to indicate a request has
been canceled.

.. The following file was automatically generated
.. include:: ./bindings/mpi_status_set_cancelled.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``status``: Status with which to associate cancel flag (status).

INPUT PARAMETER
---------------
* ``flag``: If true, indicates request was canceled (logical).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

If *flag* is set to true, then a subsequent call to
MPI_Test_cancelled(status, flag*) will also return *flag* = true;
otherwise it will return false.


NOTES
-----

Users are advised not to reuse the status fields for values other than
those for which they were intended. Doing so may lead to unexpected
results when using the status object. For example, calling
:ref:`MPI_Get_elements` may cause an error if the value is out of range, or it
may be impossible to detect such an error. The *extra_state* argument
provided with a generalized request can be used to return information
that does not logically belong in *status*. Furthermore, modifying the
values in a status set internally by MPI, such as :ref:`MPI_Recv`, may lead to
unpredictable results and is strongly discouraged.


ERRORS
------

.. include:: ./ERRORS.rst
