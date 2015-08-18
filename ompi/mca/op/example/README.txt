Copyright 2009 Cisco Systems, Inc.  All rights reserved.

This is a simple example op component meant to be a template /
springboard for people to write their own op components.  There are
many different ways to write components and modules; this is but one
example.

Before reading this example, note that it is probably more complicated
that many op components need to be.  It was done this was
intentionally to show many different OMPI concepts.  As with most
programming examples, there are many different ways to program the
same end effect.  Feel free to customize / simplify / strip out what
you don't need from this example.

This example component supports a ficticious set of hardware that
provides acceleation for the MPI_MAX and MPI_BXOR MPI_Ops.  The
ficticious hardware has multiple versions, too: some versions only
support single precision floating point types for MAX and single
precision integer types for BXOR, whereas later versions support both
single and double precision floating point types for MAX and both
single and double precision integer types for BXOR.  Hence, this
example walks through setting up particular MPI_Op function pointers
based on:

a) hardware availability (e.g., does the node where this MPI process
   is running have the relevant hardware/resources?)

b) MPI_Op (e.g., in this example, only MPI_MAX and MPI_BXOR are
   supported)

c) datatype (e.g., single/double precision floating point for MAX and
   single/double precision integer for BXOR)

Additionally, there are other considerations that should be factored
in at run time.  Hardware accelerators are great, but they do induce
overhead -- for example, some accelerator hardware require registered
memory.  So even if a particular MPI_Op and datatype are supported, it
may not be worthwhile to use the hardware unless the amount of data to
be processed is "big enough" (meaning that the cost of the
registration and/or copy-in/copy-out is ameliorated) or the memory to
be processed is already registered or is otherwise local to the the
accelerator hardware.

Hence, at run-time, the module may choose to use the accelerator
hardware or fail over to a "basic" version of the operation.  This
failover strategy is well-supported by the op framework; during the
query process, a component can "stack" itself similar to how POSIX
signal handlers can be stacked.  Specifically, op components can cache
other implementations of operation functions for use in the case of
failover.  The MAX and BXOR module implementations show one way of
using this method.

Here's a listing of the files in the example component and what they
do:

- configure.m4: Tests that get slurped into OMPI's top-level configure
  script to determine whether this component will be built or not.
- Makefile.am: Automake makefile that builds this component.
- op_example_component.c: The main "component" source file.
- op_example_module.c: The main "module" source file.
- op_example.h: information that is shared between the .c files.
- .ompi_ignore: the presence of this file causes OMPI's autogen.pl to
  skip this component in the configure/build/install process (see
  below).

To use this example as a template for your component (assume your new
component is named "foo"):

shell$ cd (top_ompi_dir)/ompi/mca/op
shell$ cp -r example foo
shell$ cd foo

Remove the .ompi_ignore file (which makes the component "visible" to
all developers) *OR* add an .ompi_unignore file with one username per
line (as reported by `whoami`).  OMPI's autogen.pl will skip any
component with a .ompi_ignore file *unless* there is also an
.ompi_unignore file containing your user ID in it.  This is a handy
mechanism to have a component in the tree but have it not built / used
by most other developers:

shell$ rm .ompi_ignore
*OR*
shell$ whoami > .ompi_unignore

Now rename any file that contains "example" in the filename to have
"foo", instead.  For example:

shell$ mv op_example_component.c op_foo_component.c
#...etc.

Now edit all the files and s/example/foo/gi.  Specifically, replace
all instances of "example" with "foo" in all function names, type
names, header #defines, strings, and global variables.

Now your component should be fully functional (although entirely
renamed as "foo" instead of "example").  You can go to the top-level
OMPI directory and run "autogen.pl" (which will find your component
and att it to the configure/build process) and then "configure ..."
and "make ..." as normal.

shell$ cd (top_ompi_dir)
shell$ ./autogen.pl
# ...lots of output...
shell$ ./configure ...
# ...lots of output...
shell$ make -j 4 all
# ...lots of output...
shell$ make install
# ...lots of output...

After you have installed Open MPI, running "ompi_info" should show
your "foo" component in the output.

shell$ ompi_info | grep op:
                  MCA op: example (MCA v2.0, API v1.0, Component v1.4)
                  MCA op: foo (MCA v2.0, API v1.0, Component v1.4)
shell$

If you do not see your foo component, check the above steps, and check
the output of autogen.pl, configure, and make to ensure that "foo" was
found, configured, and built successfully.

Once ompi_info sees your component, start editing the "foo" component
files in a meaningful way.

