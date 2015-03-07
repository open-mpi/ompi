Symbol conventions for Open MPI extensions
Last updated: January 2015

This README provides some rule-of-thumb guidance for how to name
symbols in Open MPI extensions.  Every case is different; we've had
long discussions about what to name various functions, constants, etc.
This document is an attempt to capture a few common rules-of-thumb
that we've agreed upon for how to name symbols in new MPI extensions.

Generally speaking, there are usually three kinds of extensions:

1. Functionality that is solely intended for Open MPI.
2. Functionality which may spread to both other MPI implementations
   and/or, ultimately, the MPI standard.
3. Functionality that is strongly expected to be in an upcoming
   version of the MPI specification.

----------------------------------------------------------------------

Case 1

The OMPI_Paffinity_str() extension is a good example of this type: it
is solely intended to be for Open MPI.  It will likely never be pushed
to other MPI implementations, and it will likely never be pushed to
the MPI Forum.

It's Open MPI-specific functionality, through and through.

Public symbols of this type of functionality should be named with an
"OMPI_" prefix to emphasize its Open MPI-specific nature.  To be
clear: the "OMPI_" prefix clearly identifies parts of user code that
are relying on Open MPI (and likely need to be surrounded with #if
OPEN_MPI blocks, etc.).

----------------------------------------------------------------------

Case 2

The MPI extensions mechanism in Open MPI was designed to help MPI
Forum members prototype new functionality that is intended for the
MPI specification itself.  As such, the goal for this kind of
functionality is not only to be included in the MPI spec, but possibly
also be included in another MPI implementation.

As such, it seems reasonable to prefix public symbols in this type of
functionality with "MPIX_".  This commonly-used prefix allows the same
symbols to be available in multiple MPI implementations, and therefore
allows user code to easily check for it.  E.g., user apps can check
for the presence of MPIX_Foo to know if both Open MPI and Other MPI
support the proposed MPIX_Foo functionality.

Of course, when using the MPIX_ namespace, there is the possibility of
symbol name collisions.  E.g., what if Open MPI has an MPIX_Foo and
Other MPI has a *different* MPIX_Foo?

While we technically can't prevent such collisions from happening, we
encourage extension authors to avoid such symbol clashes whenever
possible.

----------------------------------------------------------------------

Case 3

It is well-known that the MPI specification (intentionally) takes a
long time to publish.  MPI implementers can typically know, with a
high degree of confidence, about new functionality that almost
certainly *will* be included in an upcoming MPI specification long
before it is actually published.

For a variety of reasons, Open MPI has tried to implement such
functionality early (i.e., before the actual publication of the
corresponding MPI specification document).

Case in point: the non-blocking collective operations that were
included in MPI-3.0 (e.g., MPI_Ibarrier).  It was known for a year or
two before MPI-3.0 was published that these functions would be
included in MPI-3.0.

There is a continual debate among the developer community: when
implementing such functionality, should the symbols be in the MPIX_
namespace or in the MPI_ namespace?  On one hand, the symbols are not
yet officially standardized -- *they could change* before publication.
On the other hand, developers who participate in the Forum typically
have a good sense for whether symbols are going to change before
publication or not.  On the other hand (yes, we all have three hands),
no one can predict the future -- things could unexpectedly change
before the MPI specification is published.  ...and so on.

After much debate: for functionality that has a high degree of
confidence that it will be included in an upcoming spec (e.g., it has
passed at least one vote in the MPI Forum), our conclusion is that it
is OK to use the MPI_ namespace.

Case in point: Open MPI released non-blocking collectives with the
MPI_ prefix (not the MPIX_ prefix) before the MPI-3.0 specification
officially standardized these functions.

The rationale was threefold:

1. Let users use the functionality as soon as possible.

2. If OMPI initially creates MPIX_Foo, but eventually renames it to
   MPI_Foo when the MPI specification is published, then users will
   have to modify their codes to match.  This is an artificial change
   inserted just to be "pure" to the MPI spec (i.e., it's a "lawyer's
   answer").  But since the MPIX_Foo -> MPI_Foo change is inevitable,
   it just ends up annoying users.

3. Once OMPI introduces MPIX_ symbols, if we want to *not* annoy
   users, we'll likely have weak symbols / aliased versions of both
   MPIX_Foo and MPI_Foo once the Foo functionality is included in a
   published MPI specification.  However, when can we delete the
   MPIX_Foo symbol?  It becomes a continuing annoyance of backwards
   compatibility that we have to keep carrying forward.

For all these reasons, we believe that it's better to put
expected-upcoming official MPI functionality in the MPI_ namespace,
not the MPIX_ namespace.

----------------------------------------------------------------------

All that being said, these are rules of thumb.  They are not an
official mandate.  There may well be cases where there are reasons to
do something different than what is outlined above.
