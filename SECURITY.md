# Security Policy

## Supported Versions

<!-- UPDATE_VERSIONS: When updating, change both the table and the paragraph below -->

We provide security updates for `main` and for at least the last two stable release
series of Open MPI. Security updates will be made available as patch releases.

| Version | Supported          |
| ------- | ------------------ |
| main    | :white_check_mark: |
| v6.0.x  | :white_check_mark: |
| v5.0.x  | :white_check_mark: |
| v4.1.x  | :white_check_mark: |
| < 4.1   | :x:                |

## Reporting a Vulnerability

You can report a vulnerability using GitHub's private reporting feature:

1. Go to [github.com/open-mpi/ompi/security](https://github.com/open-mpi/ompi/security).
2. Click "Report a vulnerability" in the upper right corner of that page.
3. Fill out the form and submit your draft security advisory.

More details are available in
[GitHub's docs](https://docs.github.com/en/code-security/security-advisories/guidance-on-reporting-and-writing/privately-reporting-a-security-vulnerability).

You can expect to hear back about security issues within two days.
If your security issue is accepted, we will do our best to release
a fix within a week. If fixing the issue will take longer than
this, we will discuss timeline options with you.

## Security Considerations for HPC Environments

Open MPI is designed for high-performance computing environments, which have
unique security considerations:

### Network Security

- **Trusted networks**: Open MPI assumes it operates within a trusted network
  environment. MPI communication is not encrypted by default, as encryption
  would significantly impact performance. Deploy Open MPI only in networks
  where you trust all participants.

- **Firewall configuration**: Some MPI transports require specific port ranges
  to be open. Consult your network administrator and the Open MPI documentation
  for your chosen transport layer.

### Privilege and Process Isolation

- **Running as root**: Never run MPI applications as root unless absolutely
  necessary. Use appropriate privilege separation and run jobs under regular
  user accounts.

- **Shared systems**: On multi-user systems, ensure proper file permissions
  for temporary directories and shared memory segments. Open MPI respects
  system umask settings.

### Resource Managers and Launchers

- **Launch security**: When using resource managers (SLURM, PBS, etc.), rely
  on their security mechanisms for job isolation and authentication. Open MPI
  integrates with these systems' existing security models.

## Security-Related Configuration

Open MPI provides several MCA (Modular Component Architecture) parameters
that may be relevant for security-conscious deployments:

### Temporary Directory Control

```bash
# Specify a secure temporary directory
mpirun --mca orte_tmpdir_base /secure/tmp ...

# Or set via environment variable
export OMPI_MCA_orte_tmpdir_base=/secure/tmp
```

### Network Transport Selection

```bash
# Restrict to specific network interfaces
mpirun --mca btl_tcp_if_include eth0 ...

# Exclude specific transports if they pose security concerns
mpirun --mca btl ^sm,tcp ...  # exclude shared memory and TCP
```

### Debugging and Information Disclosure

In production environments, be aware that verbose debugging output may
disclose sensitive information:

For a complete list of MCA parameters and their descriptions, use:
```bash
ompi_info --param all all --level 9
```

Consult the Open MPI documentation and your local security policies when
configuring these parameters for your environment.
