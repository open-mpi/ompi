# Open MPI Continuous Integration (CI) Services
## Mellanox Open MPI CI
[![Build Status](https://dev.azure.com/mlnx-swx/mellanox-ompi/_apis/build/status/Mellanox%20CI?branchName=master)](https://dev.azure.com/mlnx-swx/mellanox-ompi/_build/latest?definitionId=9&branchName=master)
### Scope
[Mellanox](https://www.mellanox.com/) Open MPI CI is intended to verify Open MPI with recent Mellanox SW components ([Mellanox OFED](https://www.mellanox.com/page/products_dyn?product_family=26), [UCX](https://www.mellanox.com/page/products_dyn?product_family=281&mtag=ucx) and other [HPC-X](https://www.mellanox.com/page/products_dyn?product_family=189&mtag=hpc-x) components) in the Mellanox lab environment.

CI is managed by [Azure Pipelines](https://docs.microsoft.com/en-us/azure/devops/pipelines/?view=azure-devops) service.

Mellanox Open MPI CI includes:
* Open MPI building with internal stable engineering versions of UCX and HCOLL. The building is run in Docker-based environment.
* Sanity functional testing.
### How to Run CI
Mellanox Open MPI CI is triggered upon the following events:
* Push a commit into the master branch or release branches (starting from v4.0.x). CI is started automatically. CI status and log files are available on the Azure DevOps server.
* Create a pull request (PR). CI status is visible in the PR status. CI is restarted automatically upon each new commit within the PR. CI status and log files are also available on the Azure DevOps server.
* Trigger CI with special PR comments (for example, `/azp run`). Comment triggers are available only if the comment author has write permission to the PR target repo. Detailed information about comment triggers is available in the official Azure DevOps [documentation](https://docs.microsoft.com/en-us/azure/devops/pipelines/repos/github?view=azure-devops&tabs=yaml#comment-triggers).
### Support
In case of any issues, questions or suggestions please contact to [Mellanox Open MPI CI support team](mailto:swx-azure-svc@mellanox.com).
