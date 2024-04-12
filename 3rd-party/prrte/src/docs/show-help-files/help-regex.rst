.. Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
                           University Research and Technology
                           Corporation.  All rights reserved.
   Copyright (c) 2004-2005 The University of Tennessee and The University
                           of Tennessee Research Foundation.  All rights
                           reserved.
   Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
                           University of Stuttgart.  All rights reserved.
   Copyright (c) 2004-2005 The Regents of the University of California.
                           All rights reserved.
   Copyright (c) 2014      Research Organization for Information Science
                           and Technology (RIST). All rights reserved.
   Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
   Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
   Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
   Copyright (c) 2024      Nanook Consulting  All rights reserved.
   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

   This is the US/English general help file for the regex utils.

[bogus section]

This section is not used by PRTE code.  But we have to put a RST
section title in this file somewhere, or Sphinx gets unhappy.  So we
put it in a section that is ignored by PRTE code.

Hello, world
------------

[regex:special-char]

While trying to parse a regular expression to extract a list
of node names, the regex parser encountered a special character
at the beginning of the string:

.. code::

   regexp: %s

We do not know how to parse this character |mdash| please contact the
PRTE help list for assistance.

[regex:end-range-missing]

While trying to parse a regular expression to extract a list
of node names, the regex parser could not find the end of a range:

.. code::

   regexp: %s

A range must end with a ``]`` |mdash| please contact the PRTE help
list for assistance.

[regex:bad-value]

While trying to parse a regular expression to extract a list
of node names, the regex parser encountered a value it does
not know how to parse:

.. code::

   regexp: %s

Please contact the PRTE help list for assistance.

[regex:bad-ppn]

While trying to parse a regular expression to extract the number
of processes on each node, the regex parser encountered a value
it does not know how to parse:

.. code::

   regexp: %s

Please contact the PRTE help list for assistance.

[regex:num-digits-missing]

While trying to parse a regular expression to extract the node
names, the regex parser was unable to determine the number of
digits in the names:

.. code::

   regexp: %s

Please contact the PRTE help list for assistance.

[regex:invalid-name]

While trying to create a regular expression of the node names
used in this application, the regex parser has detected the
presence of an illegal character in the following node name:

.. code::

   node:  %s

Node names must be composed of a combination of ascii letters,
digits, dots, and the hyphen (``-``) character. See the following
for an explanation:

  https://en.wikipedia.org/wiki/Hostname

Please correct the error and try again.
