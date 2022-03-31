# A Developer's Note on OMPI CUDA Code

The initial CUDA implementation in Open MPI was not well factored.
Most of the developers at the time didn't really understand CUDA (or
GPUs), and the developers working on CUDA were new to Open MPI's
abstractions.  It was also unclear whether there would be another
interface for someone else's GPUs or whether the world would choose
CUDA.  With this backrgound, choices were made.

The initial implementation put much of the cuda buffer handling
functions in the datatype engine, including the code to determine if
an address referred to a CUDA buffer.  Many of the users of those
functions were also users of the datatype engine, so it made sense.
There was also a common/cuda library, which provided wrappers around
common cuda functions.  The common/cuda library (usually itself
built as a dso) dlopen'ed the base cuda library, so that no part of
Open MPI had a loader-time dependency on the cuda library.

In 2021, the default build mode for components (including common
components) was changed from DSO to static (ie, part of the base
library, which may still be a dynamic library) to reduce startup
time.  The OFI MTL was also updated to support CUDA buffers, which
required some changes to the datatype interface.  During those
changes, George rightly pushed that the CUDA specific code belonged
not in the datatype engine, but in a CUDA-specific library.  The
develoepr working on the OFI MTL code dutifully moved the code, not
realizing that he had created a circular dependency that broke the
ability of common/cuda to build as a DSO.  The datatype engine
depended on functions in the common/cuda library, but the common/cuda
library depended on libopen-pal.

To fix this issue with minimal interruption to the 5.0 schedule, we
moved the common/cuda component into libopen-pal (ie, it is no longer
a component, but just part of the base library).  Because the cuda
libraries are still dlopen'ed by the OMPI cuda code, this does not
introduce a loader-time dependency on the cuda libraries from Open
MPI, but does break the cycle described above.  This is not a great
abstraction situation, but works.

The "right" solution is an accelerator framework that is in OPAL,
which encapsulates the functions that Open MPI requires from an
accelerator (CUDA, ROCm, Xe, etc.), as we now know there will be more
than one accelerator interface in the world.  An initial take is
proposed in https://github.com/open-mpi/ompi/pull/10069, although
significant work remains to prove that said interface is sufficient to
abstract an accelerator interface (where sufficient is defined as "no
`#if HAVE_CUDA` macros in the general codebase").
