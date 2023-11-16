.. _mpi_win_get_attr:


MPI_Win_get_attr
================

.. include_body

:ref:`MPI_Win_get_attr` |mdash| Obtains the value of a window attribute.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_get_attr(MPI_Win win, int win_keyval,
   	void *attribute_val, int *flag)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_GET_ATTR(WIN, WIN_KEYVAL, ATTRIBUTE_VAL, FLAG, IERROR)
   	INTEGER WIN, WIN_KEYVAL, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) ATTRIBUTE_VAL
   	LOGICAL FLAG


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_get_attr(win, win_keyval, attribute_val, flag, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, INTENT(IN) :: win_keyval
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val
   	LOGICAL, INTENT(OUT) :: flag
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``win``: Window to which the attribute is attached (handle).
* ``win_keyval``: Key value (integer).

OUTPUT PARAMETERS
-----------------
* ``attribute_val``: Attribute value, unless *ag* = false
* ``flag``: False if no attribute is associated with the key (logical).
* ``ierror``: Fortran only: Error status (integer).


DESCRIPTION
-----------

Obtains the value of a window attribute.


ERRORS
------

.. include:: ./ERRORS.rst
