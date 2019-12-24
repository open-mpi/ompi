#!/bin/bash -eE

if [ "$DEBUG" = "true" ]; then
    set -x
fi

# WORKSPACE - root folder for OpenMPI source files
if [ -z "${WORKSPACE}" ]
then
    echo "ERROR: WORKSPACE is not defined"
    exit 1
fi

if [ -z "${OMPI_CI_DOCKER_IMAGE_NAME}" ]
then
    echo "ERROR: OMPI_CI_DOCKER_IMAGE_NAME is not defined"
    exit 1
fi

# Check that you are inside a docker container
cat /proc/1/cgroup

docker images
docker ps -a

printenv

# Run OMPI CI scenarios (build and test)
docker run \
    -v /hpc/local:/hpc/local \
    -v /opt:/opt \
    --network=host \
    --uts=host \
    --ipc=host \
    --ulimit stack=67108864 \
    --ulimit memlock=-1 \
    --security-opt seccomp=unconfined \
    --cap-add=SYS_ADMIN \
    --device=/dev/infiniband/ \
    --env WORKSPACE="${WORKSPACE}" \
    --env DEBUG="${DEBUG}" \
    --env JENKINS_RUN_TESTS=yes \
    --user swx-jenkins \
    "${OMPI_CI_DOCKER_IMAGE_NAME}" \
    "${WORKSPACE}/.ci/mellanox/ompi_test.sh"
