.. _mpi_win_set_attr:


MPI_Win_set_attr
================

.. include_body

:ref:`MPI_Win_set_attr` - Sets the value of a window attribute.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_set_attr(MPI_Win win, int win_keyval, void *attribute_val)


Fortran Syntax (see FORTRAN 77 NOTES)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_SET_ATTR(WIN, WIN_KEYVAL, ATTRIBUTE_VAL, IERROR)
   	INTEGER WIN, WIN_KEYVAL, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) ATTRIBUTE_VAL


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_set_attr(win, win_keyval, attribute_val, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, INTENT(IN) :: win_keyval
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``win``: Window to which attribute will be attached (handle).

INPUT PARAMETERS
----------------
* ``win_keyval``: Key value (integer).
* ``attribute_val``: Attribute value.

OUTPUT PARAMETER
----------------
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------


FORTRAN 77 NOTES
----------------

The MPI standard prescribes portable Fortran syntax for the
*ATTRIBUTE_VAL* argument only for Fortran 90. FORTRAN 77 users may use
the non-portable syntax

.. code-block:: fortran

        INTEGER*MPI_ADDRESS_KIND ATTRIBUTE_VAL

where MPI_ADDRESS_KIND is a constant defined in mpif.h and gives the
length of the declared integer in bytes.


ERRORS
------

.. include:: ./ERRORS.rst
