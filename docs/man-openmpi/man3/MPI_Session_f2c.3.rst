.. _mpi_session_f2c:

MPI_Session_f2c
===============

.. include_body

MPI_Session_c2f, :ref:`MPI_Session_f2c` - Translates a C session handle into a
Fortran INTEGER-style session handle, or vice versa.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Session_f2c(const MPI_Fint *f_session, MPI_Session *c_session)
   int MPI_Session_c2f(const MPI_Session *c_session, MPI_Fint *f_session)

PARAMETERS
----------

* ``f_session``: ``mpi``-style ``INTEGER`` MPI session object
* ``c_session``: C-style MPI session object

DESCRIPTION
-----------

These two procedures are provided in C to convert from a Fortran session
(which is an array of integers) to a C session (which is a structure),
and vice versa. The conversion occurs on all the information in
``session``, including that which is hidden. That is, no session
information is lost in the conversion.

When using MPI_Session_f2c(), if ``f_session`` is a valid Fortran
session, then MPI_Session_f2c() returns in ``c_session`` a valid C
session with the same content. If ``f_session`` is the Fortran value of
MPI_SESSION_NULL, or if ``f_session`` is not a valid Fortran
session, then the call is erroneous.

When using MPI_Session_c2f(), the opposite conversion is applied. If
``c_session`` is MPI_SESSION_NULL, or if ``c_session`` is not a
valid C session, then the call is erroneous.

NOTES
-----

These functions are only available in C; they are not available in any
of the Fortran MPI interfaces.
