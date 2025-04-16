.. _mpi_win_get_attr:


MPI_Win_get_attr
================

.. include_body

:ref:`MPI_Win_get_attr` |mdash| Obtains the value of a window attribute.

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_get_attr.rst

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
