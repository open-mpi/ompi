.. _mpi_pack_external_size:


MPI_Pack_external_size
======================

.. include_body

:ref:`MPI_Pack_external_size` |mdash| Calculates upper bound on space needed to
write to a portable format

.. The following file was automatically generated
.. include:: ./bindings/mpi_pack_external_size.rst

INPUT PARAMETERS
----------------
* ``datarep``: Data representation (string).
* ``incount``: Number of input data items (integer).
* ``datatype``: Datatype of each input data item (handle).

OUTPUT PARAMETERS
-----------------
* ``size``: Upper bound on size of packed message, in bytes (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Pack_external_size` allows the application to find out how much space
is needed to pack a message in the portable format defined by the MPI
Forum. It returns in *size* an upper bound on the increment in
*position* that would occur in a call to :ref:`MPI_Pack_external` with the same
values for *datarep*, *incount*, and *datatype*.

The call returns an upper bound, rather than an exact bound, as the
exact amount of space needed to pack the message may depend on context
and alignment (e.g., the first message packed in a packing unit may take
more space).


NOTES
-----

The *datarep* argument specifies the data format. The only valid value
in the current version of MPI is "external32". The argument is provided
for future extensibility.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Pack_external`
   * :ref:`MPI_Unpack_external`
