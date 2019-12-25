# OpenMPI Continuous Integration (CI) Services
## Mellanox OpenMPI CI
### Scope
[Mellanox](https://www.mellanox.com/) OpenMPI CI is intended to verify OpenMPI with recent Mellanox SW components ([Mellanox OFED](https://www.mellanox.com/page/products_dyn?product_family=26), [UCX](https://www.mellanox.com/page/products_dyn?product_family=281&mtag=ucx) and other [HPC-X](https://www.mellanox.com/page/products_dyn?product_family=189&mtag=hpc-x) components) in the Mellanox lab environment.

CI is managed by [Azure DevOps](https://docs.microsoft.com/en-us/azure/devops/pipelines/?view=azure-devops) framework. 

Mellanox OpenMPI CI includes:
* OpenMPI building with internal stable engineering versions of UCX and HCOLL. The building is run in Docker-based environment.
* Sanity functional testing.
### How to Run CI
Mellanox OpenMPI CI is triggered upon the following events:
* Push a commit into the master branch. CI status and log files are available on the Azure DevOps server.
* Create a pull request (PR). CI status is visible in the PR status. CI status and log files are also available on the Azure DevOps server.
* Trigger CI with special PR comments (for example, `/azp run`). Detailed information about comment triggers is available in the official Azure DevOps [documentation](https://docs.microsoft.com/en-us/azure/devops/pipelines/repos/github?view=azure-devops&tabs=yaml#comment-triggers).
### Support
In case of any issues, questions or suggestions please contact to [Mellanox OpenMPI CI support team](mailto:artemry@mellanox.com;andreyma@mellanox.com).