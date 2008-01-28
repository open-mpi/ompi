vtdata_DATA = \
	FILTER.SPEC \
	GROUPS.SPEC \
	METRICS.SPEC

vtdoc_DATA = \
	$(top_srcdir)/ChangeLog \
	 \
	LICENSE \
	UserManual.html \
	UserManual.pdf

vtdatadir = $(datadir)
vtdocdir = $(docdir)

LICENSE: $(top_srcdir)/COPYING
	cp $(top_srcdir)/COPYING LICENSE

EXTRA_DIST = \
	$(vtdata_DATA) \
	$(vtdoc_DATA)

