# Contributing to Open MPI

First off, thank you for taking the time to prepare a contribution to
Open MPI!

![You're awesome!](https://www.open-mpi.org/images/youre-awesome.jpg)

## Content

We love getting contributions from anyone.  But keep in mind that Open
MPI is used in production environments all around the world.

If you're contributing a small bug fix, awesome!

If you're contributing a large new piece of functionality, that will
be best viewed if you -- or someone, anyone -- is also stepping up to
help maintain that functionality over time.  We love new ideas and new
features, but we do need to be realistic in what we can reliably test
and deliver to our users.

## Contributor's Declaration

In order to ensure that we can keep distributing Open MPI under our
[open source license](LICENSE), we need to ensure that all
contributions are compatible with that license.

To that end, we require that all Git commits contributed to Open MPI
have a "Signed-off-by" token indicating that the commit author agrees
with [Open MPI's Contributor's
Declaration](https://github.com/open-mpi/ompi/wiki/Admistrative-rules#contributors-declaration).

If you have not already done so, please ensure that:

1. Every commit contains exactly the "Signed-off-by" token.  You can
add this token via `git commit -s`.
1. The email address after "Signed-off-by" must match the Git commit
email address.

## Style

There are a small number of style rules for Open MPI:

1. For all code:
    * 4 space tabs.  No more, no less.
    * No tab characters *at all*.  2 indentations are 8 spaces -- not a tab.
    * m4 code is a bit weird in terms of indentation: we don't have a
      good, consistent indentation style in our existing code.  But
      still: no tab characters at all.
1. For C code:
    * We prefer if all blocks are enclosed in `{}` (even 1-line
      blocks).
    * We prefer that if you are testing equality with a constant, put
      the constant on the *left* of the `==`.  E.g., `if (NULL ==
      ptr)`.
    * If there are no parameters to a C function, declare it with
      `(void)` (vs. `()`).

That's about it!
