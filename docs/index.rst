.. image:: openmpi_logo.png
   :align: right

Open MPI |ompi_ver|
===================

`The Open MPI Project <https://www.open-mpi.org/>`_ is an open source
implementation of the `Message Passing Interface (MPI) specification
<https://www.mpi-forum.org/docs/>`_ that is developed and maintained
by a consortium of academic, research, and industry partners.  Open
MPI is therefore able to combine the expertise, technologies, and
resources from all across the High Performance Computing community in
order to build the best MPI library available.  Open MPI offers
advantages for system and software vendors, application developers and
computer science researchers.

Documentation locations
=======================

Documentation for Open MPI can be found in the following locations:

.. list-table::
   :header-rows: 1

   * - Open MPI version
     - Documentation location

   * - v5.0.0 and later
     - Web: https://docs.open-mpi.org/

       Included in tarball: ``docs/html/index.html``

       Built in source tree (if Sphinx available): ``docs/_build/html/index.html``

       Installed: ``$docdir/html/index.html``

       (which defaults to: ``$prefix/share/doc/openmpi/html/index.html``)

   * - v4.1.x and earlier
     - See the `legacy Open MPI FAQ <https://www.open-mpi.org/faq/>`_
       and the README file in the source tarball.

       For example:

       * `v4.1.x README file <https://github.com/open-mpi/ompi/blob/v4.1.x/README>`_
       * `v4.0.x README file <https://github.com/open-mpi/ompi/blob/v4.0.x/README>`_

Release announcements
=====================

The best way to hear about new Open MPI releases is via the
`low-volume announcement mailing list
<https://lists.open-mpi.org/mailman/listinfo/announce>`_.

Additionally, if you are a downstream packager of Open MPI (e.g., you
package Open MPI for an operating system distribution), you may wish
to sign up for the `low-volume packagers list
<https://lists.open-mpi.org/mailman/listinfo/ompi-packagers>`_.

Table of contents
=================

.. toctree::
   :maxdepth: 2
   :numbered:

   quickstart
   getting-help
   release-notes/index
   installing-open-mpi/index
   features/index
   validate
   version-numbering
   mca
   building-apps/index
   launching-apps/index
   tuning-apps/index
   app-debug/index
   developers/index
   contributing
   license/index
   history
   man-openmpi/index
   man-openshmem/index
