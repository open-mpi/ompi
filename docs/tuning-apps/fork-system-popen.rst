Calling fork(), system(), or popen() in MPI processes
=====================================================

It may be possible to call ``fork()``, ``system()``, ``popen()``, etc. calls
from an MPI process, but it depends on a lot of factors, including (but not
limited to):

* The operating system
* The underlying compute hardware
* The network stack
* Interactions with other middleware in the MPI process

Users are encouraged to avoid invoking these operations in their MPI applications
as they may cause issues with Open MPI's operation or the operation of underlying
components, such as `UCX <https://openucx.org/>`_
and `Libfabric <https://libfabric.org/>`_.
