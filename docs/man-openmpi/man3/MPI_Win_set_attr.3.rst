.. _mpi_win_set_attr:


MPI_Win_set_attr
================

.. include_body

:ref:`MPI_Win_set_attr` |mdash| Sets the value of a window attribute.

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_set_attr.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``win``: Window to which attribute will be attached (handle).

INPUT PARAMETERS
----------------
* ``win_keyval``: Key value (integer).
* ``attribute_val``: Attribute value.

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).


DESCRIPTION
-----------

Sets the value of a window attribute.


ERRORS
------

.. include:: ./ERRORS.rst
