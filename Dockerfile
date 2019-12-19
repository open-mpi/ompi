FROM centos:7

RUN groupadd -g 11429 swx-jenkins
RUN adduser --uid 6213 --gid 11429 --home /home/swx-jenkins swx-jenkins
RUN echo "swx-jenkins ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

RUN yum install -y \
    bc \
    environment-modules \
    jq \
    perl \
    perl-Data-Dumper \
    perl-Thread-Queue

RUN echo "/hpc/local/etc/modulefiles" >> /usr/share/Modules/init/.modulespath
