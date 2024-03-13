
.. _building-open-mpi-downloading-label:

Downloading Open MPI
====================

Open MPI is generally available two ways:

#. As source code.

   * The best place to get an official Open MPI source code
     distribution is from `the main Open MPI web site
     <https://www.open-mpi.org/>`_.

   * Downstream Open MPI packagers (e.g., Linux distributions)
     sometimes also provide source code distributions.  They may
     include additional patches or modifications.

     Consult your favorite downstream packager for more details.
     
   .. caution:: Do **not** download an Open MPI source code tarball
               from GitHub.com.  The tarballs automatically generated
               by GitHub.com are incomplete and will not build
               properly.

               GitHub.com-generated tarballs are **not** official Open
               MPI releases.

#. As binary packages.

   * The Open MPI community does not provide binary packages on `the
     main Open MPI web site <https://www.open-mpi.org/>`_.

   * Various downstream packagers (e.g., Linux distributions,
     Homebrew, MacPorts, etc.) *do* provide pre-built, binary packages
     for Open MPI.

     Consult your favorite downstream packager for more details.

The majority of the rest of this section deals with installing Open
MPI from source.
