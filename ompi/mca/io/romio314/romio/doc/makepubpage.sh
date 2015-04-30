#!/bin/sh

# if you have bibtext2html installed (http://www.lri.fr/~filliatr/bibtex2html/
# but I know there are other packages by that name), then you can re-generate
# the "ROMIO publication page"
# (http://www.mcs.anl.gov/research/projects/romio/pubs.html)

# If you update the command below, please be sure to retain the link to the
# older papers

WEB_HOST=login3.mcs.anl.gov
WEB_DIR=/mcs/web/research/projects/romio

bibtex2html -t "Papers using ROMIO" \
	--header "Please help us keep this list up to date. Contact mpich-discuss@mcs.anl.gov for any corrections or additions. <p>Last updated at $(date).<p><h2>Recent publications</h2><p>" \
	--footer "<p><h2>Other publications</h2><p>
	<ul>
	<li><a href='http://cucis.ece.northwestern.edu/publications/subject.html#Scalable I/O'>Northwestern University CUCIS group</a>
	<li><a href='http://www.eng.auburn.edu/~wkyu/'>Weikuan Yu</a>
	<li><a href='http://www.umcs.maine.edu/~dickens/pubs.html'>Phillip Dickens</a>
	<li><a href=apers-old.html>Older ROMIO papers</a>" \
	-r -d -both pubs.bib

if [ $? -eq 0 ] ; then
	scp pubs* ${WEB_HOST}:${WEB_DIR}
else
	echo "error running bibtex2html.  website not updated"
fi
