Main updates (not on release branches yet)
==========================================

This file generally contains all the updates for Open MPI that have
not yet appeared on a release branch.  It reflects active development,
and is therefore a "loose" listing of features and changes.  It is not
considered definitive.

Open MPI version main
---------------------

.. admonition:: MPIR API has been removed
   :class: warning

   As was announced in summer 2017, Open MPI has removed support of
   MPIR-based tools beginning with the release of Open MPI v5.0.0.

   The new PRRTE based runtime environment supports PMIx-tools API
   instead of the legacy MPIR API for debugging parallel jobs.

   see https://github.com/openpmix/mpir-to-pmix-guide for more
   information


.. admonition:: zlib is suggested for better user experience
   :class: note

   PMIx will optionally use zlib to compress large data streams.
   This may result in shorter-than-normal startup times and
   smaller memory footprints.  It is recommended to install zlib
   and zlib-devel for a better user experience.
