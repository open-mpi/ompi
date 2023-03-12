Calling fork(), system(), or popen() in MPI processes
=====================================================

It may be possible to call ``fork()``, ``system()``, ``popen()``, etc. calls
from an MPI process, but it depends on a lot of factors, including (but not
limited to):

* The operating system
* The underlying compute hardware
* The network stack
* Interactions with other middleware in the MPI process

In some cases, Open MPI will determine that it is not safe to
``fork()``.  In these cases, Open MPI will register a
``pthread_atfork()`` callback to print a warning when the process
forks.

This warning is helpful for legacy MPI applications where the current
maintainers are unaware that ``system()`` or ``popen()`` is being invoked from
an obscure subroutine nestled deep in millions of lines of Fortran code
(we've seen this kind of scenario many times).

However, this atfork handler can be dangerous because there is no way
to *unregister* an atfork handler.  Hence, packages that
dynamically open Open MPI's libraries (e.g., Python bindings for Open
MPI) may fail if they finalize and unload libmpi, but later call
fork.  The atfork system will try to invoke Open MPI's atfork handler;
nothing good can come of that.

For such scenarios, or if you simply want to disable printing the
warning, Open MPI can be set to never register the atfork handler with
the ``mpi_warn_on_fork`` MCA parameter.  For example:

.. code-block:: sh

   shell$ mpirun --mca mpi_warn_on_fork 0 ...

Of course, systems that ``dlopen("libmpi.so", ...)`` may not use Open
MPI's ``mpirun``, and therefore may need to use a different mechanism to
:ref:`set MCA parameters <label-running-setting-mca-param-values>`.

