.. _mpi_aint_add:


MPI_Aint_add
============

.. include_body

:ref:`MPI_Aint_add`, :ref:`MPI_Aint_diff` - Portable functions for arithmetic
on MPI_Aint values.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   MPI_Aint MPI_Aint_add(MPI_Aint base, MPI_Aint disp)

   MPI_Aint MPI_Aint_diff(MPI_Aint addr1, MPI_Aint addr2)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   INTEGER(KIND=MPI_ADDRESS_KIND) MPI_AINT_ADD(BASE, DISP)
           INTEGER(KIND=MPI_ADDRESS_KIND) BASE, DISP

   INTEGER(KIND=MPI_ADDRESS_KIND) MPI_AINT_DIFF(ADDR1, ADDR2)
           INTEGER(KIND=MPI_ADDRESS_KIND) ADDR1, ADDR2


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   INTEGER(KIND=MPI_ADDRESS_KIND) MPI_AINT_ADD(BASE, DISP)
           INTEGER(KIND=MPI_ADDRESS_KIND) BASE, DISP

   INTEGER(KIND=MPI_ADDRESS_KIND) MPI_AINT_DIFF(ADDR1, ADDR2)
           INTEGER(KIND=MPI_ADDRESS_KIND) ADDR1, ADDR2


INPUT PARAMETERS
----------------
* ``base``: Base address (integer).
* ``disp``: Displacement (integer).
* ``addr1``: Minuend address (integer).
* ``addr2``: Subtrahend address (integer).

DESCRIPTION
-----------

:ref:`MPI_Aint_add` produces a new MPI_Aint value that is equivalent to the
sum of the *base* and *disp* arguments, where *base* represents a base
address returned by a call to :ref:`MPI_Get_address` and *disp* represents
a signed integer displacement. The resulting address is valid only at
the process that generated *base*, and it must correspond to a location
in the same object referenced by *base*, as described in MPI-3.1 section
4.1.12. The addition is performed in a manner that results in the
correct MPI_Aint representation of the output address, as if the process
that originally produced *base* had called:

.. code-block:: c

   MPI_Get_address ((char *) base + disp, &result);

:ref:`MPI_Aint_diff` produces a new MPI_Aint value that is equivalent to
the difference between *addr1* and *addr2* arguments, where *addr1*
and *addr2* represent addresses returned by calls to
:ref:`MPI_Get_address`.  The resulting address is valid only at the
process that generated *addr1* and *addr2*, and *addr1* and *addr2*
must correspond to locations in the same object in the same process,
as described in MPI-3.1 section 4.1.12. The difference is calculated
in a manner that results in the signed difference from *addr1* to
*addr2*, as if the process that originally produced the addresses had
called ``(char *) addr1`` - ``(char *) addr2`` on the addresses
initially passed to :ref:`MPI_Get_address`.


.. seealso::
   * :ref:`MPI_Get_address`
