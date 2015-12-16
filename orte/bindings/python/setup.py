from setuptools import setup

setup(
    name = "orte-cffi",
    version = "0.4.0",
    author = "Mark Santcroos",
    author_email = "mark.santcroos@rutgers.edu",
    description = "CFFI-based Python wrapper for Open RTE",
    license = "New BSD",
    keywords = "mpi cffi",
    packages = ['src/orte-cffi'],
    url = "http://www.open-mpi.org",
    setup_requires = ["cffi>=1.5.0"],
    cffi_modules = ["src/orte-cffi/build.py:ffi"],
    install_requires = ["cffi>=1.5.0"],
)
