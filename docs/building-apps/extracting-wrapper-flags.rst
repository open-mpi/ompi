Extracting flags from the wrapper compilers
===========================================

If you cannot use the wrapper compilers for some reason, there are
multiple supported ways to extract the compiler/linker flags that you
will need.

Using the ``--showme`` option
-----------------------------

The wrapper compilers all support a ``--showme`` command line option
that will show what commands would have been invoked.

.. code-block:: sh

   # Show the flags necessary to compile MPI C applications
   shell$ mpicc --showme:compile

   # Show the flags necessary to link MPI C applications
   shell$ mpicc --showme:link

   # Show all the flags necessary to build MPI C applications
   shell$ mpicc --showme

.. note:: If you pass ``--showme`` *and additional command line
   parameters* to the wrapper compiler, be sure to *also* pass in a
   filename.  Otherwise, the ``--showme`` functionality will not
   display output as expected.

.. warning:: It is almost never a good idea to hard-code these results
   in a ``Makefile`` (or other build system).  It is almost always
   best to run (for example) ``mpicc --showme:compile`` in a dynamic
   fashion to find out what you need.  For example, GNU Make allows
   running commands and assigning their results to variables:

   .. code-block:: make

      MPI_COMPILE_FLAGS = $(shell mpicc --showme:compile)
      MPI_LINK_FLAGS = $(shell mpicc --showme:link)

      my_app: my_app.c
              $(CC) $(MPI_COMPILE_FLAGS) my_app.c $(MPI_LINK_FLAGS) -o my_app

Using ``pkg-config``
--------------------

Alternatively, Open MPI also installs ``pkg-config(1)`` configuration
files under ``$libdir/pkgconfig``.  If ``pkg-config`` is configured to
find these files (e.g., if you add ``$libdir/pkgconfig`` |mdash| which
is usually ``$prefix/lib/pkgconfig`` |mdash| to the
``PKG_CONFIG_PATH`` environment variable), then compiling / linking
Open MPI programs can be performed like this:

.. code-block:: sh

   shell$ export PKG_CONFIG_PATH=/opt/openmpi/lib/pkgconfig
   shell$ gcc hello_world_mpi.c -o hello_world_mpi -g \
               `pkg-config ompi-c --cflags --libs`
   shell$

Open MPI supplies multiple ``pkg-config`` configuration files; one for
each different wrapper compiler (language):

* ``ompi``: Synonym for ``ompi-c``; Open MPI applications using the C
  MPI bindings
* ``ompi-c``: Open MPI applications using the C MPI bindings
* ``ompi-cxx``: Open MPI applications using the C MPI bindings
* ``ompi-fort``: Open MPI applications using the Fortran MPI bindings

.. note:: Open MPI's ``pkg-config`` files *work properly*, but they
          probably aren't *technically correct*.

          Specifically: Open MPI will list all of its dependent
          libraries that are necessary to link an MPI application,
          even if a given dependency has a ``.pc`` file and should
          therefore be listed as a ``Requires`` and/or
          ``Requires.private`` in Open MPI's ``.pc`` files.

          For example, Open MPI lists ``-lpmix`` in both ``Libs`` and
          ``Libs.private``.  But since PMIx provides its own
          ``pmix.pc`` file, it would be more correct for Open MPI to
          *not* list ``-lpmix`` in ``Libs`` / ``Libs.private``, and
          instead include:

          .. code-block::

             Requires: pmix
             Requires.private: pmix

          The end result is likely immaterial, but we document this
          just in case it ever becomes an issue someday.


Using ``ompi_info``
-------------------

This method is not directly suitable for getting all the compiler /
linker flags needed to compile MPI applications because it does not
include the relevant flags to find Open MPI's headers and libraries.
But it does show a breakdown of all other flags.

.. code-block::

   shell$ ompi_info --all | grep -i wrapper
      Wrapper extra CFLAGS:
    Wrapper extra CXXFLAGS:
      Wrapper extra FFLAGS:
     Wrapper extra FCFLAGS:
     Wrapper extra LDFLAGS:
        Wrapper extra LIBS: -lutil -lnsl -ldl -Wl,--export-dynamic -lm

This installation is *only* adding options in the ``xLIBS`` areas of the
wrapper compilers; all other values are blank (remember: the ``-I``'s
and ``-L``'s are implicit).

Note that the ``--parsable`` option can be used to obtain
machine-parsable versions of this output.  For example:

.. code-block::

   shell$ ompi_info --all --parsable | grep wrapper:extra
   option:wrapper:extra_cflags:
   option:wrapper:extra_cxxflags:
   option:wrapper:extra_fflags:
   option:wrapper:extra_fcflags:
   option:wrapper:extra_ldflags:
   option:wrapper:extra_libs:-lutil -lnsl  -ldl  -Wl,--export-dynamic -lm
