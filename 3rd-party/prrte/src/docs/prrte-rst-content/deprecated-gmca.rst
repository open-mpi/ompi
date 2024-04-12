.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Syntax: ``--gmca <key> <value>``, where ``key`` is the parameter name
and ``value`` is the parameter value. The ``g`` prefix indicates that
this parameter is "global", and to be applied to *all* application
contexts |mdash| not just the one in which the directive appears.

Pass generic MCA parameters |mdash| i.e., parameters whose project
affiliation must be determined by PRRTE based on matching the name of
the parameter with defined values from various projects that PRRTE
knows about.

.. admonition:: Deprecated
   :class: warning

   This translation can be incomplete (e.g., if known project adds or
   changes parameters) |mdash| thus, it is strongly recommended that
   users use project-specific parameters such as ``--gprtemca`` or
   ``--gpmixmca``.
