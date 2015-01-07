* create a new subdirectory opal/mca/event/libeventxxxx

* copy these files from the current component to the new directory
	autogen.subdirs
	configure.m4
	libeventxxxx_component.c
	libeventxxxx_module.c
	libeventxxxx.h
	Makefile.am
	opal_check_visibility.m4

* update the libevent version numbers in the above files by doing a global replace

* expand the libevent tarball into the new directory under a subdirectory named "libevent"

* copy libeventxxxx/libevent/opal_rename.h to the new libeventyyyy/libevent and change the version number in the symbol renames

* edit the following files per their counterparts in the current version
	libevent/configure.ac
	libevent/Makefile.am
	libevent/include/event2/event.h
	libevent/include/event2/thread.h
	libevent/include/event2/util.h
	libevent/log-internal.h
	libevent/log.c
	libevent/event.c (#ifdef lines in eventops area)
