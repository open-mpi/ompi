Using Valgrind to Find Open MPI Application Errors
==================================================

The `Valgrind <https://valgrind.org/>`_ utilities may be useful in identifying application
logic errors. However, there are many situations where Open MPI purposefully does
not initialize and subsequently communicate memory, e.g., by calling ``writev(2)``.
Furthermore, several cases are known where memory is not properly freed when
MPI is finalized.

This certainly does not help distinguishing real errors from false positives.
Valgrind provides functionality to suppress errors and warnings from certain
function contexts.

In an attempt to ease debugging using Valgrind, Open MPI provides a
Valgrind suppression file, that can be passed on the command
line:

.. code-block:: sh

   shell$ mpirun -n 2 valgrind --suppressions=$PREFIX/share/openmpi/openmpi-valgrind.supp

More information on suppression files and how to generate them can be
found in `Valgrind's documentation
<https://valgrind.org/docs/manual/manual-core.html#manual-core.suppress>`_.
