.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Disable automatic ``--prefix`` behavior. PRRTE automatically sets the
prefix for remote daemons if it was either configured with the
``--enable-prte-prefix-by-default`` option OR prte itself was executed
with an absolute path to the ``prte`` command. This option disables
that behavior.
