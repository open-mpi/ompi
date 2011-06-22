SUBDIRS = . doc

if AMBUILDBINARIES
bin_SCRIPTS = vtsetup
vtbindata_DATA = vtsetup.jar
vtbindatadir = $(bindir)
endif

vtdata_DATA = \
	vtsetup-data.xml \
	vtsetup-data.dtd
vtdatadir = $(datadir)

EXTRA_DIST = \
	vtsetup.jar \
	vtsetup-data.dtd \
	ChangeLog \
	README \
	src/vtsetup-*.src.tar.gz

