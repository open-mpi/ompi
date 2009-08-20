This is the OMPI contrib system.  It is (far) less functional and
flexible than the OMPI MCA framework/component system.

Each contrib package must have either both a configure.params and a
configure.m4 file, or it must have an autogen.subdirs file.

If it has (configure.params, configure.m4), configure.params can be
just like any MCA component's: specify a list of files to create
during AC_OUTPUT.  The configure.m4 file will be slurped up into the
main configure script, just like other MCA components.  Note that
there is currently no "no configure" option for contrib packages --
you *must* have a configure.m4 (even if all it does it call $1).
Feel free to fix this situation if you want -- see:

    https://svn.open-mpi.org/trac/ompi/ticket/1162

:-)

If it has an autogen.subdirs file, then it needs to be a subdirectory
that is autogen-able (see the vt project for an example).
