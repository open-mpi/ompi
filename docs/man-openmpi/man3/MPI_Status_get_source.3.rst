.. _mpi_status_get_source:


MPI_Status_get_source
========================

.. include_body

:ref:`MPI_Status_get_source` |mdash| Retrieves the ``MPI_SOURCE`` field from ``status``.

.. The following file was automatically generated
.. include:: ./bindings/mpi_status_get_source.rst

INPUT PARAMETER
---------------
* ``status``: Status from which to retrieve the source rank (status).

OUTPUT PARAMETER
----------------
* ``source``: rank set in the ``MPI_SOURCE`` field (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Returns in source the ``MPI_SOURCE`` field from the ``status`` object.

While the ``status`` object members ``MPI_SOURCE``, ``MPI_TAG``, and
``MPI_ERROR`` are directly accessible in C and Fortran, for
convenience in other contexts (e.g., when using alternate MPI bindings
in languages that do not directly translate the ``status`` object),
users can also access these values via procedure calls such as this
one.


ERRORS
------

.. include:: ./ERRORS.rst


.. seealso::
   * :ref:`MPI_Get_count`
   * :ref:`MPI_Get_elements`
   * :ref:`MPI_Get_elements_x`
   * :ref:`MPI_Status_get_error`
   * :ref:`MPI_Status_get_tag`
   * :ref:`MPI_Status_set_cancelled`
   * :ref:`MPI_Status_set_elements`
   * :ref:`MPI_Status_set_elements_x`
   * :ref:`MPI_Status_set_error`
   * :ref:`MPI_Status_set_source`
   * :ref:`MPI_Status_set_tag`
   * :ref:`MPI_Test_cancelled`
