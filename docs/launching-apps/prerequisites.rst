Prerequisites
=============

In general, successful launch of Open MPI jobs requires the ability to
find Open MPI's executables and shared libraries on all nodes at run
time.  If these can be via in system-default search paths (e.g.,
without the user needing to set or modify ``PATH`` or
``LD_LIBRARY_PATH``), nothing additional needs to be done.  For
example:

* If Open MPI is installed in ``/usr/bin`` and ``/usr/lib``), that is
  usually sufficient, and the user does not need to do anything extra.
* If Open MPI is installed in a location that is not searched by
  default, users may need to add ``$prefix/bin`` to their ``PATH`` and
  ``$libdir`` (which defaults to ``$prefix/lib``) to their
  ``LD_LIBRARY_PATH``.

  .. caution:: In scheduled environments, ensuring Open MPI's
               executables and libraries can be found on the node that
               executes :ref:`mpirun(1) <man1-mpirun>` may be
               sufficient.

               In non-scheduled environments, users may need to set
               the ``PATH`` and ``LD_LIBRARY_PATH`` environment
               variables in their shell setup files (e.g.,
               ``$HOME/.bashrc``) so that non-interactive
               ``ssh``-based logins will be able to find the Open MPI
               executables and libraries.

If users are unable to add the relevant directories to ``PATH`` and
``LD_LIBRARY_PATH``,the :ref:`mpirun(1) <man1-mpirun>` ``--prefix``
option *may* be sufficient.
