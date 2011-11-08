
nonce
-----

.. The above is magic that makes all the following sections visible. I am not
   sure why.


Synopsis
========

.. code-block:: text

    arx shdat (-b <size>)? (-o <output file>)? < input
    arx shdat (-b <size>)? (-o <output file>)? <input file>+
    arx tmpx <options>* (//+ <shell command> (//+ <options>*)?)?

Description
===========

The `arx` tool automates a common task in the world of operations automation:
packing code, sending it to a remote machine, unpacking in a temporary
directory, running a task therein and then removing the temporary directory.
One might do this when setting up a moderately complicated back-up script,
installing a new version of nginx or even just to run jobs across ones
infrastructure.

The `arx` tool has no in-built notion of remote connections or server
clusters; all automation is captured as Bourne compatible scripts that use a
small number of UNIX utilities in a broadly portable way. At present, the
utilities used are `sed`, `tr`, `head`, `tar`, `bzip2` and `gzip`. The
generated scripts can be piped to `ssh` to be executed remotely.

The `tmpx` subcommand of `arx` offers a variety of options for bundling code
and a task to run. The `shdat` subcommand exposes the lower-level
functionality of encoding binary data in a shell script that outputs that
binary data, using HERE documents and some odd replacement rules for nulls.

tmpx
====

The `tmpx` subcommand bundles together archives, environment settings and an
executable or shell command in to a Bourne-compatible script that runs the
command or executable in a temporary directory, after having unpacked the
archives and set the environment. After execution, the temporary directory is
removed (or not, depending on options described below).

shdat
=====

The `shdat` subcommand translates binary data in to a shell script which
outputs the binary data. The data is encoded in HERE documents in such a way
that data without NULs is not changed and that data with NULs is minimally
expanded (about 1% for randomish data like compressed tarballs).

HERE docs, with the so-called 'quoted delimiter', turn out to be the fastest
string type implemented in commonly availabe shells.

Data without NULs need only be scanned to find a short delimiter string. For
data with NULs, the NULs must be encoded and a shell command constructed that
restores them. A low-frequency character is chosen to replace NUL and a second
character is chosen to act as an escape character. All instances of the
character used to replace nulls and the escape character are replaced with
escape sequences. A pipeline with `tr` and `sed` is constructed that restores
the NULs and translates the escape sequences back to the characters they
replaced.

