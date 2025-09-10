.. _mpi_info_get_hw_resource_info:


MPI_Info_get_hw_resource_info
=============================

.. include_body

:ref:`MPI_Info_get_hw_resource_info` |mdash| Returns an info object containing information pertaining to the hardware platform 
are used.

.. The following file was automatically generated
.. include:: ./bindings/mpi_get_hw_resource_info.rst

OUTPUT PARAMETERS
-----------------
* ``info``: Info object created (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Info_get_hw_resource_info` an info object containing information pertaining to the hardware platform on 
which the calling \MPI/ process is executing at the moment of the call.  The application is responsible for freeing
the returned info object using :ref:`MPI_Info_free`.

.. note:: Open MPI currently provides no hardware platform information in the returned ``info`` object.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_free`
