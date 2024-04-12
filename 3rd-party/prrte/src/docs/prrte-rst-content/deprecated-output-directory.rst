.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Redirect output from application processes into
``filename/job/rank/std[out,err,diag]``. A relative path value will be
converted to an absolute path. The directory name may include a colon
followed by a comma-delimited list of optional case-insensitive
directives. Supported directives currently include ``NOJOBID`` (do not
include a job-id directory level) and ``NOCOPY`` (do not copy the
output to the stdout/err streams).

.. admonition:: Deprecated
   :class: warning

   This option is deprecated.  Please use ``--output dir=<path>``.
