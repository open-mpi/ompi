#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

EXTRA_DIST = \
		openmpi-default-hostfile \
		openmpi-mca-params.conf

# Steal a little trickery from a generated Makefile to only install
# lam-bhost.def if one does not already exist at the target.

install-data-local:
	@ p="openmpi-mca-params.conf openmpi-default-hostfile"; \
	for file in $$p; do \
	  if test -f $(DESTDIR)$(sysconfdir)/$$file; then \
	    echo "******************************* WARNING ************************************"; \
	    echo "*** Not installing new $$file over existing file in:"; \
	    echo "***   $(DESTDIR)$(sysconfdir)/$$file"; \
	    echo "******************************* WARNING ************************************"; \
	  else \
	    if test -f "$$file"; then d=; else d="$(srcdir)/"; fi; \
	    f="`echo $$file | sed -e 's|^.*/||'`"; \
	    echo " $(INSTALL_DATA) $$d$$file $(DESTDIR)$(sysconfdir)/$$f"; \
	    $(INSTALL_DATA) $$d$$file $(DESTDIR)$(sysconfdir)/$$f; \
	  fi; \
	done
