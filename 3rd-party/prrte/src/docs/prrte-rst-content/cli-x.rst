.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Export an environment variable, optionally specifying a value. For
example:

* ``-x foo`` exports the environment variable ``foo`` and takes its
  value from the current environment.
* ``-x foo=bar`` exports the environment variable name ``foo`` and
  sets its value to ``bar`` in the started processes.
* ``-x foo*`` exports all current environmental variables starting
  with ``foo``.
