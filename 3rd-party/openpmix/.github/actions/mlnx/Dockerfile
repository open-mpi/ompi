FROM centos:7.6.1810

RUN \
    yum install -y perl perl-Data-Dumper \
        python3 python3-pip python-virtualenv \
        automake libtool flex make bzip2 git which rpm-build libevent-devel hwloc hwloc-devel

COPY entrypoint.sh /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]

