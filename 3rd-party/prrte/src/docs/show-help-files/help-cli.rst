.. Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
   Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

   This is the US/English general help file for PRTE CLI options.

[bogus section]

This section is not used by PRTE code.  But we have to put a RST
section title in this file somewhere, or Sphinx gets unhappy.  So we
put it in a section that is ignored by PRTE code.

Hello, world
------------

[help]

Help can be requested in three ways:

#. A directive to the cmd. For example, the command

   .. code::

      "%s --help" or "%s -h"

   will provide the high-level help output giving a general
   description of each available option.

#. An option to the help directive. For example, the command

   .. code::

      "%s --help foo" or "%s -h foo"

   will provide detailed help about the ``foo`` command line
   directive.

#. Specifying "help" as an option to the directive of interest.
   For example, the command

   .. code::

      "%s --foo --help" or "%s --foo -h"

   will provide detailed help about the "foo" command line
   directive. Note that the "help" and "h" options do not require the
   dash prefixes |mdash| i.e., the following commands

   .. code::

      "%s --foo help" or "%s --foo h"

   will also output detailed help for ``foo``. Thus, directives are
   precluded from defining options of ``help`` and ``h``.

Note that the last two methods are functionally equivalent
and will return the same output.

[unknown-option]

Help was requested for an unknown option:

.. code::

   Option: %s

Please use the ``%s --help`` command to obtain a list of all
supported options.

[unrecognized-option]

The help system for %s does not recognize the following:

.. code::

   Option: %s

This might require help from the developers.

[version]

Print version and exit

[verbose]

Enable debug options - number of times specified sets level of verbosity
(e.g., ``-vvv`` => 3rd level of debug information)

[quiet]

Suppress helpful messages

[parsable]

When used in conjunction with other parameters, output information
(e.g., help messages) is displayed in a machine-parsable format

[parseable]

When used in conjunction with other parameters, the output information
(e.g., help messages) is displayed in a machine-parsable format

[short-no-long]

A short option was provided to %s and recognized by the parser,
but has no matching long option:

.. code::

   Short option: %s

This might require help from the developers.

[unregistered-option]

An unrecognized option was included on the %s command line:

.. code::

   Option: %s

Please use the ``%s --help`` command to obtain a list of all
supported options.
