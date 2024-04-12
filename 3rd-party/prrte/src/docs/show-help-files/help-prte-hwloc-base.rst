.. Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
   Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

   This is the US/English help file for PRTE's hwloc base support

[bogus section]

This section is not used by PRTE code.  But we have to put a RST
section title in this file somewhere, or Sphinx gets unhappy.  So we
put it in a section that is ignored by PRTE code.

Hello, world
------------

[mbind failure]

PRTE failed to bind internal memory to a specific NUMA node.  This
message will only be reported at most once per process.

.. code::

   Local host: %s
   PID:        %d
   File:       %s:%d
   Message:    %s
   Severity:   %s

[invalid binding_policy]

The specified %s policy is not recognized:

.. code::

   Policy: %s

Please check for a typo or ensure that the option is a supported
one.

[redefining-policy]

Conflicting directives for binding policy are causing the policy
to be redefined:

.. code::

   New policy:   %s
   Prior policy:  %s

Please check that only one policy is defined.

[deprecated]

The following command line option and corresponding MCA parameter have
been deprecated and replaced as follows:

.. code::

   Command line option:
     Deprecated:  %s
     Replacement: %s

   Equivalent MCA parameter:
     Deprecated:  %s
     Replacement: %s

The deprecated forms *will* disappear in a future version of PRTE.
Please update to the new syntax.

[obj-idx-failed]

PRTE failed to find a cache of a specified type.  This is a highly
unusual error; it may indicate a system configuration error.  This
additional information may be of help:

.. code::

   Message:     %s
   Cache level: %d

[missing-cpulist]

The following binding modifier was given but lacks a required value:

.. code::

   Modifier: %s

Please check for a typo or ensure that the option is a supported
one.

[bad-processor-type]

The default CPU list contains an invalid modifier:

.. code::

   Default CPU list:  %s
   Modifier:  %s

The modifier should indicate the type of CPU being used in the
list |mdash| the only valid values are HWTCPUS (for use of hwthreads
as independent CPUs) or CORECPUS (for use of cores as independent
CPUs). Please correct the input.

[unrecognized-modifier]

The binding request contains an unrecognized modifier:

.. code::

   Request: %s

Please check your request and try again.

[bind-upwards]

Binding is performed to the first available specified object type
within the object where the process was mapped. In other words,
binding can only be done to the mapped object or to a resource
located beneath that object.

The specified binding lies above the mapping object type:

.. code::

   Mapping level: %s
   Binding level: %s

Please correct the map/bind directives and try again.
