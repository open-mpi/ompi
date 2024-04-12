.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Comma-delimited list of one or more files containing PRRTE and PMIx
MCA params for tuning DVM and/or application operations. Parameters in
the file will be treated as *generic* parameters and subject to the
translation rules/uncertainties.  See ``--help mca`` for more
information.

Syntax in the file is:

.. code::

   param = value

with one parameter and its associated value per line. Empty lines and
lines beginning with the ``#`` character are ignored.
