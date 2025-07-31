.. _mpi_status_set_elements:


MPI_Status_set_elements
=======================

.. include_body

:ref:`MPI_Status_set_elements`, :ref:`MPI_Status_set_elements_x` - Modifies
opaque part of *status* to allow :ref:`MPI_Get_elements` to return *count*.

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Status_set_elements, MPI_Status_set_elements_x

.. The following file was automatically generated
.. include:: ./bindings/mpi_status_set_elements.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``status``: Status to associate with *count (status).*

INPUT PARAMETERS
----------------
* ``datatype``: Data type associated with *count (handle).*
* ``count``: Number of elements to associate with *status (integer).*

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Status_set_elements` modifies the opaque part of *status* so that a
call to :ref:`MPI_Get_elements` or :ref:`MPI_Get_elements_x` will return count.
:ref:`MPI_Get_count` will return a compatible value.*

A subsequent call to ``MPI_Get_count(status, datatype, count)``, to
``MPI_Get_elements(status, datatype, count)``, or to
``MPI_Get_elements_x(status, datatype, count)`` must use a data-type
argument that has the same type signature as the data-type argument that
was used in the call to :ref:`MPI_Status_set_elements`.


NOTES
-----

Users are advised not to reuse the status fields for values other than
those for which they were intended. Doing so may lead to unexpected
results when using the status object. For example, calling
:ref:`MPI_Get_elements` may cause an error if the value is out of range, or it
may be impossible to detect such an error. The *extra_state* argument
provided with a generalized request can be used to return information
that does not logically belong in status. Furthermore, modifying the
values in a status set internally by MPI, such as :ref:`MPI_Recv`, may lead to
unpredictable results and is strongly discouraged.

Note that :ref:`MPI_Status_set_elements_x` is  *deprecated* as of MPI-4.1. Please use
the big count version of :ref:`MPI_Status_set_elements` instead.

ERRORS
------

.. include:: ./ERRORS.rst
