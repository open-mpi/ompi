This is the OMPI contrib system.  It is (far) less functional and
flexible than the OMPI MCA framework/component system.

Each contrib package must have a configure.m4.  It may optionally also
have an autogen.subdirs file.

If it has a configure.m4 file, it must specify its own relevant files
to AC_CONFIG_FILES to create during AC_OUTPUT -- just like MCA
components (at a minimum, usually its own Makefile).  The configure.m4
file will be slurped up into the main configure script, just like
other MCA components.  Note that there is currently no "no configure"
option for contrib packages -- you *must* have a configure.m4 (even if
all it does it call $1).  Feel free to fix this situation if you want
-- it probably won't not be too difficult to extend autogen.pl to
support this scenario, similar to how it is done for MCA components.
:-)

If it has an autogen.subdirs file, then it needs to be a subdirectory
that is autogen-able.
