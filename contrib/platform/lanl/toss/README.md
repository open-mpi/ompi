These platform files were created from platform files shipped with the release
tarball. Each file has been modified. Here are the details on how they were
created.

- common
  Copy of contrib/platform/lanl/toss/toss-common. Removed entries in bottom
  half of file that were specific to TOSS so that it could be used for Cray
  platforms as well.
- common-optimized
  Copy of contrib/platform/lanl/toss/optimized-common. Used the file as-is.
- toss2-qib-optimized
  Copy of contrib/platform/lanl/toss/optimized with the following changes:
  - source common and common-optimzed instead of toss-common and
    optimized-common
  - added entries that were removed from common:
    - enable_mca_no_build
    - with_slurm
    - with_tm
    - with_pmi
    - NOTE: common had "with_devel_headers=yes" in it that was not propagated.
      This option should not be used in production as per Open MPI developer
      mailing list guidance.
  - Changed comment "Disable components not needed on any TOSS platform" to
    "Disable components not needed on TOSS platforms with high-speed networks"
  - Changed "enable panasas" to "enable lustre"
- toss2-qib-optimized.conf
  - copy of contrib/platform/lanl/toss/optimized.conf with the following
    changes:
    - changed: orte_no_session_dirs = /lustre,/net,/users,/usr/projects
    - changed: btl = ^openib
    - removed: hwloc_base_binding_policy = core (outdated setting)
    - added: rmaps_base_ranking_policy = core (rank by core)
    - added: ras_base_launch_orted_on_hn = true (run orted on parent node of
      allocation)
- toss2-mlx-optimized
  - copy of toss2-qib-optimized
- toss2-mlx-optimized.conf
  - copy of toss2-qib-optimized.conf with the following changes:
    - remove: oob_tcp_if_include = ib0,eth0 (identification of general network
      device names is problematic in RHEL7. Just let Open MPI figure it out)
    - change: btl = vader,openib,self
    - change: btl_openib_receive_queues = X,4096,1024:X,12288,512:X,65536,512
      (change S to X; make sure numbers match those for the same entry in
      contrib/platform/lanl/toss/optimized-mlx.conf)
    - addition: pml = ob1 (disable MXM)
- toss3-hfi-optimized
  - copy of toss2-qib-optimized
- toss3-hfi-optimized.conf
  - copy of toss2-qib-optimized.conf with the following changes:
    - remove: oob_tcp_if_include = ib0,eth0
    - add: oob_tcp_if_exclude = ib0 (Omnipath is flaky; don't use it for oob)
- toss3-wc-optimized (platform file for woodchuck which is an ethernet-only
  connected cluster)
  - copy of toss3-hfi-optimized with the following changes:
    - change: remove "btl-tcp" from the enable_mca_no_build list
    - change: comment "Disable components not needed on TOSS platforms with
      high-speed networks" to "Disable components not needed on TOSS Ethernet-
      connected clusters"
- toss3-wc-optimized.conf
  - copy of toss3-hfi-optimized.conf with the following changes:
    - change: comment "Add the interface for out-of-band communication and set
      it up" to "Set up the interface for out-of-band communication"
    - remove: oob_tcp_if_exclude = ib0
    - remove: btl (let Open MPI figure out what best to use for ethernet-
      connected hardware)
    - remove: btl_openib_want_fork_support (no infiniband)
    - remove: btl_openib_receive_queues (no infiniband)
- cray-lustre-optimized
  - copy of contrib/platform/lanl/cray_xc_cle5.2/optimized-lustre with the
    following changes:
    - remove: whole if/else clause of 'test "$enable_debug" = "yes"'
    - addition: source ./common
    - addition: source ./common-optimized
    - change: with_io_romio_flags="--with-file-system=ufs+nfs+lustre"
    - remove: with_lustre=/opt/cray/lustre-cray_ari_s/default
    - additions from platform/lanl/cray_xc_cle5.2/optimized-common that don't
      go in common-optimzed:
      - enable_mca_no_build=routed-linear,pml-v,pml-example,pml-cm,ess-cnos,grpcomm-cnos,plm-rsh,btl-tcp,oob-ud,ras-simulator,mpool-fake
      - enable_mca_static=btl:ugni,btl:self,btl:vader,pml:ob1
      - enable_mca_directpml-ob1
      - with_tm=no
      - enable_orte_static_ports=no
      - enable_pty_support=no
    - addition: enable_dlopen=yes (change from original platform file as per
      Nathan Hjelm)
- cray-lustre-optimized.conf
  - copy of contrib/platform/lanl/cray_xc_cle5.2/optimized-lustre.conf with
    the following changes:
    - change: orte_no_session_dirs = /lustre,/users,/usr/projects
    - remove: hwloc_base_binding_policy = core (outdated setting)
    - addition: rmaps_base_ranking_policy = core (rank by core)

# vi: filetype=txt
