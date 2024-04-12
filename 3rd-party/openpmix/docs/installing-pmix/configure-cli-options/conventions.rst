.. _building-pmix-cli-options-conventions-label:

``configure`` CLI option conventions
------------------------------------

``configure`` will, by default, search for header files and/or
libraries for various optional features (e.g., various network
support libraries).  If the relevant files are found, PMIx
will built support for that feature.  If they are not found, PMIx
will skip building support for that feature.

However, if you specify ``--with-FOO`` (where ``FOO`` is the
corresponding CLI option name for the feature) on the ``configure``
command line and PMIx is unable to find relevant support for
``FOO``, ``configure`` will assume that it was unable to provide a
feature that was specifically requested, and will therefore abort so
that a human can resolve out the issue.

.. note:: Using ``--with-FOO`` to force PMIx's ``configure``
          script to abort it if can't find support for a given feature
          may be preferable to unexpectedly discovering at run-time
          that PMIx is missing support for a critical feature.

Additionally, if a search directory is specified for ``FOO`` in the
form ``--with-FOO=DIR``, PMIx will:

#. Search for ``FOO``'s header files in ``DIR/include``.
#. Search for ``FOO``'s library files:

   #. If ``--with-FOO-libdir=LIBDIR`` was specified, search in
      ``LIBDIR``.
   #. Otherwise, search in ``DIR/lib64``, and if they are not found
      there, search again in ``DIR/lib``.

#. If both the relevant header files and libraries are found:

   #. PMIx will build support for ``FOO``.
   #. If the root path where the FOO libraries are found is neither
      ``/usr`` nor ``/usr/local``, PMIx will compile itself with
      RPATH flags pointing to the directory where ``FOO``'s libraries
      are located.

      .. important:: PMIx does not RPATH ``/usr/lib[64]`` and
                     ``/usr/local/lib[64]`` because many systems
                     already search these directories for run-time
                     libraries by default; adding RPATH for them could
                     have unintended consequences for the search path
                     ordering.

.. caution:: The ``--with-FOO-libdir=LIBDIR`` options are not usually
   needed; they are typically only needed when ``FOO``'s libraries are
   installed in an "unexpected" location.

   Also note the difference between ``--with-FOO=DIR`` and
   ``--with-FOO-libdir=LIBDIR``: the former is a directory to which
   suffixes such as ``/include`` and ``/lib`` are added, whereas the
   latter is assumed to be a full library directory name (e.g.,
   ``/opt/some_library/lib``).
