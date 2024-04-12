.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

The ``output`` command line directive must be accompanied by a
comma-delimited list of case-insensitive options that control how
output is generated. The full directive need not be provided |mdash| only
enough characters are required to uniquely identify the directive. For
example, ``MERGE`` is sufficient to represent the
``MERGE-STDERR-TO-STDOUT`` directive |mdash| while ``TAG`` can not be
used to represent ``TAG-DETAILED`` (though ``TAG-D`` would suffice).

Supported values include:

* ``TAG`` marks each output line with the ``[job,rank]<stream>:`` of
  the process that generated it

* ``TAG-DETAILED`` marks each output line with a detailed annotation
  containing ``[namespace,rank][hostname:pid]<stream>:`` of the
  process that generated it

* ``TAG-FULLNAME`` marks each output line with the
  ``[namespace,rank]<stream>:`` of the process that generated it

* ``TAG-FULLNAME`` marks each output line with the
  ``[namespace,rank]<stream>:`` of the process that generated it

* ``TIMESTAMP`` prefixes each output line with a ``[datetime]<stream>:``
  stamp. Note that the timestamp will be the time when the line is
  output by the DVM and not the time when the source output it

* ``XML`` provides all output in a pseudo-XML format
  ``MERGE-STDERR-TO-STDOUT`` merges stderr into stdout

* ``DIR=DIRNAME`` redirects output from application processes into
  ``DIRNAME/job/rank/std[out,err,diag]``. The provided name will be
  converted to an absolute path

* ``FILE=FILENAME`` redirects output from application processes into
  ``filename.rank.`` The provided name will be converted to an absolute
  path

Supported qualifiers include ``NOCOPY`` (do not copy the output to the
stdout/err streams), and ``RAW`` (do not buffer the output into complete
lines, but instead output it as it is received).
