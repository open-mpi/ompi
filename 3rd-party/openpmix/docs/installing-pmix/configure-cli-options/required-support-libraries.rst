.. _label-building-pmix-cli-options-required-support-libraries:

CLI Options for required support libraries
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following ``configure`` command line options are for PMIx's
:ref:`required support libraries
<label-install-required-support-libraries>`

* ``--with-hwloc[=VALUE]``:
* ``--with-libevent[=VALUE]``:
* ``--with-libev[=VALUE]``:

These  options specify where to find
  the headers and libraries for the `Hwloc
  <https://www.open-mpi.org/projects/hwloc/>`_, `Libevent
  <https://libevent.org/>`_, and `libev <https://metacpan.org/dist/EV/view/libev/ev.pod/>`_ libraries,
  respectively.

.. note:: Either ``libevent`` *or* ``libev`` must be provided.
          PMIx's ``configure`` script will error out if both
          are specified. In the absence of user direction,
          PMIx will default to using ``libevent``.

  The following ``VALUE``\s are permitted:

  * ``DIR``: Specify the location of a specific installation to use.
    ``configure`` will abort if it cannot find suitable header files
    and libraries under ``DIR``.

* ``--with-hwloc-libdir=LIBDIR``:
* ``--with-libevent-libdir=LIBDIR``:
* ``--with-libev-libdir=LIBDIR``:
  :ref:`See the configure CLI
  options conventions <building-pmix-cli-options-conventions-label>`
  for a description of these options.
