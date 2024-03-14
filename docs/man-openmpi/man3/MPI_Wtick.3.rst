.. _mpi_wtick:


MPI_Wtick
=========

.. include_body

:ref:`MPI_Wtick` |mdash| Returns the resolution of :ref:`MPI_Wtime`.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   double MPI_Wtick()


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   DOUBLE PRECISION MPI_WTICK()


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   DOUBLE PRECISION MPI_WTICK()


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
