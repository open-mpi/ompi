ECP-CI at OLCF
--------------
[![pipeline status](https://code.ornl.gov/ecpcitest/ompi-x/naughtont3-ompi/badges/olcf-ecp-ci/pipeline.svg)](https://code.ornl.gov/ecpcitest/ompi-x/naughtont3-ompi/-/commits/olcf-ecp-ci)

CI Tests that run on OLCF resources via ECP project.

Brief summary:
 - OMPI Github mirrored at OLCF Gitlab (pull approx 30-minutes)
 - Builds source tree
    - autogen, configure, make install, make check, make examples
 - Runs tests in batch allocation with single node
 - Currently using 18-node Power9 ["Ascent" machine at OLCF](https://docs.olcf.ornl.gov/systems/ascent_user_guide.html)
 - Tests run using hostfile from allocation (e.g., `$LSB_DJOB_HOSTFILE`)


