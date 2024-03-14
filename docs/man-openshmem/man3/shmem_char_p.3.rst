.. _shmem_char_p:


shmem_char_p
============

.. include_body

:ref:`shmem_char_p`\ (3), :ref:`shmem_float_p`\ (3), :ref:`shmem_int_p`\ (3),
:ref:`shmem_long_p`\ (3), :ref:`shmem_short_p`\ (3), :ref:`shmem_longlong_p`\ (3),
:ref:`shmem_longdouble_p`\ (3) - These routines provide a low latency
mechanism to write basic types (char, short, int, float, double, long,
long long, long double) to symmetric data objects on remote PEs.


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>


   void shmem_char_p(char *addr, char value, int pe)

   void shmem_short_p(short *addr, short value, int pe)

   void shmem_int_p(int *addr, int value, int pe)

   void shmem_long_p(long *addr, long value, int pe)

   void shmem_longlong_p(long long *addr, long long value, int pe)

   void shmem_float_p(float *addr, float value, int pe)

   void shmem_double_p(double *addr, double value, int pe)

   void shmem_longdouble_p(long double *addr, long double value, int pe)


DESCRIPTION
-----------

These routines provide a very low latency put capability for single
elements of most basic types.

The arguments are as follows:

addr
   The remotely accessible array element or scalar data object which
   will receive the data on the remote PE.

value
   The value to be transferred to addr on the remote PE.

pe
   The number of the remote PE.

As with shmem_put\ (3), these functions start the remote transfer and
may return before the data is delivered to the remote PE. Use
:ref:`shmem_quiet`\ (3) to force completion of all remote PUT transfers.


.. seealso::
   *intro_shmem*\ (3) *shmem_put*\ (3)
