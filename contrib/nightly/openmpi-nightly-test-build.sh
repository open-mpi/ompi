:
buildroot=/home/jsquyres/openmpi
installfile="$buildroot/nightly/`hostname`-installs.txt"
addr=jsquyres@open-mpi.org
#addr=testing@open-mpi.org

export PATH=$buildroot/local/bin:$PATH

versions="trunk v1.0"

for ver in $versions; do
	$buildroot/build_tarball.pl \
		--scratch $buildroot/nightly/$ver \
		--email $addr \
		--url http://www.open-mpi.org/nightly/$ver/ \
		--config $buildroot/$ver-config.txt \
		--leave-install $installfile \
		--make "" \
                --nocheck

#	if test -s $installfile; then
#		dirs=`grep debug $installfile`
#		args=
#		for d in $dirs; do
#			args="--prefix $d $args"
#		done
#
#		$buildroot/illegal_symbols_report.pl $args --email $addr --delete
#		rm -f $installfile
#	fi
done
exit 0

