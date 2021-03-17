ECP-CI at OLCF
--------------
#[![pipeline status](https://code.ornl.gov/ecpcitest/ompi-x/naughtont3-ompi/badges/olcf-ecp-ci/pipeline.svg)](https://code.ornl.gov/ecpcitest/ompi-x/naughtont3-ompi/-/commits/olcf-ecp-ci)

CI Tests that run on LANL resources.

Brief summary:
 - OMPI GitHub mirrored at LANL GitLab
 - Builds source tree
    - autogen, configure, make install, make check, make examples
 - Runs tests in batch allocation with single node
 - The CI status/results are reported back to GitHub via a python script (`build-status.py`)
    - See also: https://ecp-ci.gitlab.io/docs/guides/build-status-gitlab.html
    - Note: The `BUILDSTATUS_TOKEN` is defined in GitLab and contains a
      personal access token (`repo:status`) for the upstream GitHub user.

