MPI_T see MPI-3 section 14.3.5.

.. _mpi_t_cvar_get_info:


MPI_T_cvar_get_info
===================

.. include_body

:ref:`MPI_T_cvar_get_info` |mdash| Query information from a control variable


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_cvar_get_info(int cvar_index, char *name, int *name_len,
                           int *verbosity, MPI_Datatype *datatype, MPI_T_enum *enumtype,
                           const *desc, int *desc_len, int *bind, int *scope)


INPUT PARAMETERS
----------------
* ``cvar_index``: Index of the control variable to be queried.

INPUT/OUTPUT PARAMETERS
-----------------------
* ``name_len``: Length of the string and/or buffer for name.
* ``desc_len``: Length of the string and/or buffer for desc.

OUTPUT PARAMETERS
-----------------
* ``name``: Buffer to return the string containing the name of the control variable.
* ``verbosity``: Verbosity level of this variable.
* ``datatype``: MPI datatype of the information stored in the control variable.
* ``enumtype``: Optional descriptor for enumeration information.
* ``desc``: Buffer to return the string containing the description of the control variable.
* ``bind``: Type of MPI object to which this variable must be bound.
* ``scope``: Scope of when changes to this variable are possible.

DESCRIPTION
-----------

:ref:`MPI_T_cvar_get_info` can be used to query information about a control
variable. The function returns the verbosity, datatype, enumeration
type, binding, and scope of the queried control variable in the
arguments *verbosity*, *datatype*, *enumtype*, *bind*, and *scope*,
respectively. Control variables in Open MPI are the same as MCA
parameters.


VERBOSITY
---------

As Open MPI exposes a very large number of MCA parameters (control
variables), control variables are categorized into nine verbosity
levels corresponding to the equivalent :ref:`ompi_info
<man1-ompi_info>` level. The nine levels are (in increasing order):

#. ``MPI_T_VERBOSITY_USER_BASIC``: Basic information of interest to users

#. ``MPI_T_VERBOSITY_USER_DETAIL``: Detailed information of interest to users

#. ``MPI_T_VERBOSITY_USER_ALL``: All remaining information of interest to users

#. ``MPI_T_VERBOSITY_TUNER_BASIC``: Basic information required for tuning

#. ``MPI_T_VERBOSITY_TUNER_DETAIL``: Detailed information required for tuning

#. ``MPI_T_VERBOSITY_TUNER_ALL``: All remaining information required for tuning

#. ``MPI_T_VERBOSITY_MPIDEV_BASIC``: Basic information for MPI implementors

#. ``MPI_T_VERBOSITY_MPIDEV_DETAIL``: Detailed information for MPI implementors

#. ``MPI_T_VERBOSITY_MPIDEV_ALL``: All remaining information for MPI implementors

For more information see MPI-3 section 14.3.1.


DATATYPE
--------

The datatype returned by :ref:`MPI_T_cvar_get_info` is restricted to
one of the following datatypes: ``MPI_INT``, ``MPI_UNSIGNED``,
``MPI_UNSIGNED_LONG``, ``MPI_UNSIGNED_LONG_LONG``, ``MPI_COUNT``,
``MPI_CHAR``, and ``MPI_DOUBLE``. For more information on datatypes in
MPI_T see MPI-3 section 14.3.5.


SCOPE
-----

The scope describes when and how changes can be made to a control
variable. From MPI-3 section 14.3.6, the scope may be any of the following:

* ``MPI_T_SCOPE_CONSTANT``: read-only, value is constant

* ``MPI_T_SCOPE_READONLY``: read-only, cannot be written, but can change

* ``MPI_T_SCOPE_LOCAL``: may be writeable, writing is a local operation

* ``MPI_T_SCOPE_GROUP``: may be writeable, must be done to a group of
  processes, all processes in a group must be set to consistent
  values

* ``MPI_T_SCOPE_GROUP_EQ``: may be writeable, must be done to a group
  of processes, all processes in a group must be set to the same value

* ``MPI_T_SCOPE_ALL``: may be writeable, must be done to all
  processes, all connected processes must be set to consistent values

* ``MPI_T_SCOPE_ALL_EQ``: may be writeable, must be done to all
  processes, all connected processes must be set to the same value

For more information see MPI-3 section 14.3.6 Table 14.4.


NOTES
-----

This MPI tool interface function returns two strings. This function
takes two argument for each string: a buffer to store the string, and a
length which must initially specify the size of the buffer. If the
length passed is n then this function will copy at most n - 1 characters
of the string into the corresponding buffer and set the length to the
number of characters copied - 1. If the length argument is NULL or the
value specified in the length is 0 the corresponding string buffer is
ignored and the string is not returned.

Open MPI does not currently support binding control variables to MPI
objects.


ERRORS
------

:ref:`MPI_T_cvar_get_info` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_INDEX``: The control variable index is invalid


.. seealso:: :ref:`ompi_info <man1-ompi_info>`
