.. Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
   Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

   This is the US/English general help file for PRTE.

[bogus section]

This section is not used by PRTE code.  But we have to put a RST
section title in this file somewhere, or Sphinx gets unhappy.  So we
put it in a section that is ignored by PRTE code.

Hello, world
------------

[stacktrace signal override]

PRTE was inserting a signal handler for signal %d but noticed
that there is already a non-default handler installed.  PRTE's
handler was therefore not installed; your job will continue.  This
warning message will only be displayed once, even if PRTE
encounters this situation again.

To avoid displaying this warning message, you can either not install
the error handler for signal %d or you can have PRTE not try to
install its own signal handler for this signal by setting the
``prte_signals`` MCA parameter.

.. code::

   Signal: %d
   Current prte_signal value: %s

[stacktrace bad signal]

PRTE was inserting a signal handler but was given an invalid
signal number:

.. code::

   Signal string: %s
   Bad value:     %s

The given value must be an integer within the signal number
range. Please correct the value and try again.

[malformed net_private_ipv4]

PRTE has detected at least one malformed IP address or netmask in
the value of the prte_net_private_ipv4 MCA parameter.  The
prte_net_private_ipv4 MCA parameter accepts a semicolon-delimited list
of Classless Inter-Domain Routing (CIDR) notation specifications, each
of the form <ipaddress>/<mask>.  For example:

.. code::

   10.0.0.0/8;172.16.0.0/12;192.168.0.0/16;169.254.0.0/16

The first detected malformed entry was %s.

[invalid-net-mask]

PRTE has detected a malformed IPv4 address or netmask:

.. code::

   Value provided: %s

Accepted values follow the Classless Inter-Domain
Routing (CIDR) notation specifications. For example:

.. code::

   10.0.0.0/8
   172.16/12
   192.168
   169.254.0.0/16

[malformed-uri]

PRTE has detected a malformed URI:

.. code::

   URI:  %s

Accepted values follow IETF RFC3986, e.g. ``file://192.168.1.1/over/there``

[relative-path]

When creating a URI, all files must be specified in absolute paths:

.. code::

   Value provided: %s

Please update your application to provide the full path to the file.

[sys-limit-failed]

Per request, PRTE attempted to set a system resource
limit to a given value:

.. code::

   Resource:  %s
   Limit:     %s

The system has refused to allow this operation. This is likely
due to a permission limitation, or specifying an unsupported
value. Please check the system or remove the request and try
again.

[sys-limit-unrecognized]

PRTE received a request to set a system resource limit.
Sadly, OMPI does not recognize or currently support the specified
resource:

.. code::

   Resource:  %s
   Limit:     %s

Please correct the request and try again.

[dir-mode]

While working through a directory tree, we were unable to set
a directory to the desired mode:

.. code::

   Directory:  %s
   Mode:       %0x
   Error:      %s

Please check to ensure you have adequate permissions to perform
the desired operation.

[mkdir-failed]

A call to mkdir was unable to create the desired directory:

.. code::

   Directory: %s
   Error:     %s

Please check to ensure you have adequate permissions to perform
the desired operation.
