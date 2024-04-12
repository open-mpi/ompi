.. This file is included by building-pmix.rst

Run-time system support
^^^^^^^^^^^^^^^^^^^^^^^

The following are command line options for various runtime systems that
can be used with ``configure``:

* ``--with-alps``:
  Force the building of for the Cray Alps run-time environment.  If
  Alps support cannot be found, configure will abort.

* ``--with-lsf=DIR``:
  Specify the directory where the LSF libraries and header files are
  located.  This option is generally only necessary if the LSF headers
  and libraries are not in default compiler/linker search paths.

  LSF is a resource manager system, frequently used as a batch
  scheduler in HPC systems.

* ``--with-lsf-libdir=DIR``:
  Look in directory for the LSF libraries.  By default, Open MPI will
  look in ``DIR/lib`` and ``DIR/lib64``, which covers most cases.  This
  option is only needed for special configurations.

* ``--with-slurm``:
  Force the building of Slurm scheduler support.

* ``--with-sge``:
  Specify to build support for the Oracle Grid Engine (OGE) resource
  manager and/or the Open Grid Engine.  OGE support is disabled by
  default; this option must be specified to build OMPI's OGE support.

  The Oracle Grid Engine (OGE) and open Grid Engine packages are
  resource manager systems, frequently used as a batch scheduler in
  HPC systems.  It used to be called the "Sun Grid Engine", which is
  why the option is still named ``--with-sge``.

* ``--with-tm=DIR``:
  Specify the directory where the TM libraries and header files are
  located.  This option is generally only necessary if the TM headers
  and libraries are not in default compiler/linker search paths.

  TM is the support library for the Torque and PBS Pro resource
  manager systems, both of which are frequently used as a batch
  scheduler in HPC systems.
