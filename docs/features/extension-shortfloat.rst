.. _ompi-features-extension-shortfloat-label:

Short float extension
=====================

Overview
--------

The ``shortfloat`` extension provides MPI datatypes corresponding to
short / half-precision floating point language types.  Depending on
which types the compiler supports, it defines some or all of:

* ``MPIX_C_FLOAT16`` |mdash| for the C type ``_Float16`` (ISO/IEC TS
  18661-3:2015).  This name and meaning match MPICH.
* ``MPIX_SHORT_FLOAT`` |mdash| for the C/C++ type ``short float``.
* ``MPIX_C_SHORT_FLOAT_COMPLEX`` |mdash| for the C type ``short float
  _Complex``.
* ``MPIX_CXX_SHORT_FLOAT_COMPLEX`` |mdash| for the C++ type
  ``std::complex<short float>``.

These datatypes were proposed for (but not accepted into) the MPI
standard, so they carry the ``MPIX_`` prefix.  See the extension's
``README.md`` in the source tree
(``ompi/mpiext/shortfloat/README.md``) and `MPI Forum issue 65
<https://github.com/mpi-forum/mpi-issues/issues/65>`_ for background.

Because this extension only adds datatype handles (constants), it does
not provide any functions, and therefore has no manual pages.

When it is built
----------------

The ``shortfloat`` extension is built by default, but *only* when the
compiler provides a suitable short / half-precision floating point
type.  Specifically, Open MPI's ``configure`` builds it when either:

* the compiler natively supports a ``short float`` type; or
* Open MPI can provide an equivalent 16-bit type (``opal_short_float_t``,
  typically mapped to ``_Float16``).

If neither is available, the extension is silently omitted |mdash| there
is no dedicated configure option to force it on, since it depends
entirely on compiler support.  As with all extensions, it can be
excluded explicitly with ``--disable-mpi-ext`` or by omitting it from
an explicit ``--enable-mpi-ext=LIST``.  See :ref:`the extensions
overview <ompi-features-extensions-label>` for details.

Availability at run time
------------------------

When the extension is compiled in, ``<mpi-ext.h>`` defines the
preprocessor macro ``OMPI_HAVE_MPI_EXT_SHORTFLOAT`` to ``1``.
Applications should test this macro before using any of the ``MPIX_``
datatypes above, both because the extension may have been omitted (if
the compiler lacked a short float type) and to remain portable to other
MPI implementations.  Note that the individual datatype handles that are
defined depend on which language types were available at build time.
