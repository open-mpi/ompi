.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Definition of "processor element"
=================================

By default, PRRTE defines that a "processing element" is a processor
core.  However, if ``--use-hwthread-cpus`` is specified on the command
line, then a "processing element" is a hardware thread.

