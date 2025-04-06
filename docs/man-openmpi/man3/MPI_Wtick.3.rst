.. _mpi_wtick:


MPI_Wtick
=========

.. include_body

:ref:`MPI_Wtick` |mdash| Returns the resolution of :ref:`MPI_Wtime`.

.. The following file was automatically generated
.. include:: ./bindings/mpi_wtick.rst

RETURN VALUE
------------

Time in seconds of resolution of :ref:`MPI_Wtime`.


DESCRIPTION
-----------

:ref:`MPI_Wtick` returns the resolution of :ref:`MPI_Wtime` in seconds. That is, it
returns, as a double-precision value, the number of seconds between
successive clock ticks. For example, if the clock is implemented by the
hardware as a counter that is incremented every millisecond, the value
returned by :ref:`MPI_Wtick` should be 10^-3.


NOTE
----

This function does not return an error value. Consequently, the result
of calling it before :ref:`MPI_Init` or after :ref:`MPI_Finalize` is undefined.


.. seealso::
   * :ref:`MPI_Wtime`
