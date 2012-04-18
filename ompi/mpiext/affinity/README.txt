# Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
Copyright (c) 2010 Oracle and/or its affiliates.  All rights reserved.

$COPYRIGHT$

Jeff Squyres
19 April 2010, and
16 April 2012

Terry Dontje
18 November 2010

This extension provides a single new function, OMPI_Affinity_str(),
that takes a format value and then provides 3 prettyprint strings as
output:

fmt_type: is an enum that tells OMPI_Affinity_str() whether to use a
resource description string or layout string format for ompi_bound and
currently_bound output strings.

ompi_bound: describes what sockets/cores Open MPI bound this process
to (or indicates that Open MPI did not bind this process).

currently_bound: describes what sockets/cores this process is
currently bound to (or indicates that it is unbound).

exists: describes what processors are available in the current host.

See OMPI_Affinity_str(3) for more details.
