.. _mpi_status_set_source:


MPI_Status_set_source
========================

.. include_body

:ref:`MPI_Status_set_source` |mdash| Sets the ``MPI_SOURCE`` field on ``status``.

.. The following file was automatically generated
.. include:: ./bindings/mpi_status_set_source.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``status``: Status with which to associate source rank (status).

INPUT PARAMETER
---------------
* ``source``: rank to set in the ``MPI_SOURCE`` field (integer).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Set the ``MPI_SOURCE`` field in the ``status`` object to the provided
source argument.

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
   * :ref:`MPI_Status_get_source`
   * :ref:`MPI_Status_get_tag`
   * :ref:`MPI_Status_set_cancelled`
   * :ref:`MPI_Status_set_elements`
   * :ref:`MPI_Status_set_elements_x`
   * :ref:`MPI_Status_set_error`
   * :ref:`MPI_Status_set_tag`
   * :ref:`MPI_Test_cancelled`
