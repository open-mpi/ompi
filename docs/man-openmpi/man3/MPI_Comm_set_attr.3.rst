.. _mpi_comm_set_attr:


MPI_Comm_set_attr
=================

.. include_body

:ref:`MPI_Comm_set_attr` |mdash| Stores attribute value associated with a key.

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_set_attr.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``comm``: Communicator from which attribute will be attached (handle).

INPUT PARAMETERS
----------------
* ``comm_keyval``: Key value (integer).
* ``attribute_val``: Attribute value.

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_set_attr` stores the stipulated attribute value *attribute_val*
for subsequent retrieval by :ref:`MPI_Comm_get_attr`. If the value is already
present, then the outcome is as if :ref:`MPI_Comm_delete_attr` was first called
to delete the previous value (and the callback function ``delete_fn`` was
executed), and a new value was next stored. The call is erroneous if
there is no key with value *comm_keyval*; in particular
``MPI_KEYVAL_INVALID`` is an erroneous key value. The call will fail if the
delete_fn function returned an error code other than ``MPI_SUCCESS``.

This function replaces :ref:`MPI_Attr_put`, the use of which is deprecated. The
C binding is identical. The Fortran binding differs in that
*attribute_val* is an address-sized integer.


NOTES
-----

Values of the permanent attributes ``MPI_TAG_UB``, ``MPI_HOST``, ``MPI_IO``, and
``MPI_WTIME_IS_GLOBAL`` may not be changed.

The type of the attribute value depends on whether C or Fortran is being
used. In C, an attribute value is a pointer (``void *``); in Fortran, it is
a single, address-size integer system for which a pointer does not fit
in an integer.

If an attribute is already present, the delete function (specified when
the corresponding keyval was created) will be called.

Note that the ``MPI_HOST`` attribute is  *deprecated* as of MPI-4.1.


ERRORS
------

.. include:: ./ERRORS.rst
