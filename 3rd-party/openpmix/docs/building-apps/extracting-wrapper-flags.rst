Extracting flags from the wrapper compilers
===========================================

If you cannot use the wrapper compiler for some reason, there are
multiple supported ways to extract the compiler/linker flags that you
will need.

Using the ``--showme`` option
-----------------------------

The wrapper compiler supports a ``--showme`` command line option
that will show what commands would have been invoked.

.. note:: If you pass ``--showme`` *and additional command line
   parameters* to the wrapper compiler, be sure to *also* pass in a
   filename.  Otherwise, the ``--showme`` functionality will not
   display output as expected.

.. warning:: It is almost never a good idea to hard-code these results
   in a ``Makefile`` (or other build system).  It is almost always
   best to run (for example) ``pmixcc --showme:compile`` in a dynamic
   fashion to find out what you need.  For example, GNU Make allows
   running commands and assigning their results to variables:

   .. code-block:: make

      PMIX_COMPILE_FLAGS = $(shell pmixcc --showme:compile)
      PMIX_LINK_FLAGS = $(shell pmixcc --showme:link)

      my_app: my_app.c
              $(CC) $(PMIX_COMPILE_FLAGS) my_app.c $(PMIX_LINK_FLAGS) -o my_app

Using ``pkg-config``
--------------------

Alternatively, PMIx also installs ``pkg-config(1)`` configuration
files under ``$libdir/pkgconfig``.  If ``pkg-config`` is configured to
find these files (e.g., if you add ``$libdir/pkgconfig`` |mdash| which
is usually ``$prefix/lib/pkgconfig`` |mdash| to the
``PKG_CONFIG_PATH`` environment variable), then compiling / linking
PMIx programs can be performed like this:

.. code-block:: sh

   shell$ export PKG_CONFIG_PATH=/opt/pmix/lib/pkgconfig
   shell$ gcc hello_world.c -o hello_world -g \
               `pkg-config pmix --cflags --libs`
   shell$

.. note:: PMIx's ``pkg-config`` file *works properly*, but it
          probably isn't *technically correct*.

          Specifically: PMIx will list all of its dependent
          libraries that are necessary to link PMIx-based application,
          even if a given dependency has a ``.pc`` file and should
          therefore be listed as a ``Requires`` and/or
          ``Requires.private`` in PMIx's ``.pc`` files.

          For example, PMIx lists ``-lhwloc`` in both ``Libs`` and
          ``Libs.private``.  But since HWLOC provides its own
          ``pmix.pc`` file, it would be more correct for PMIx to
          *not* list ``-lhwloc`` in ``Libs`` / ``Libs.private``, and
          instead include:

          .. code-block::

             Requires: hwloc
             Requires.private: hwloc

          The end result is likely immaterial, but we document this
          just in case it ever becomes an issue someday.


Using ``pmix_info``
-------------------

This method is not directly suitable for getting all the compiler /
linker flags needed to compile PMIx-based applications because it does not
include the relevant flags to find PMIx's headers and libraries.
But it does show a breakdown of all other flags.

.. code-block::

   shell$ pmix_info --all | grep Build
            Build CFLAGS:
           Build LDFLAGS:
              Build LIBS: -ldl -levent_core -levent_pthreads -lhwloc

This installation is *only* adding options in the ``xLIBS`` areas of the
wrapper compilers; all other values are blank (remember: the ``-I``'s
and ``-L``'s are implicit).

Note that the ``--parsable`` option can be used to obtain
machine-parsable versions of this output.  For example:

.. code-block::

   shell$ ompi_info --all --parsable | grep option:build
        option:build:cflags:
        option:build:ldflags:
        option:build:libs:-ldl -levent_core -levent_pthreads -lhwloc
