==========================
 arx - archived execution
==========================

Synopsis
--------

.. code-block:: text

    arx ... (-h|-[?]|--help)? ...
    arx shdat (-b <size>)? (-o <output file>)? < input
    arx shdat (-b <size>)? (-o <output file>)? <input file>+
    arx tmpx <options>* (//+ <shell command> (//+ <options>*)?)?

Description
-----------

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

For all subcommands, when options overlap in their effect -- for example,
setting the output with ``-o`` -- the rightmost option takes precedence.
Whenever ``-h``, ``-?`` or ``--help`` is present on the command line, help is
displayed and the program exits.

When paths are specified on an ``arx`` command line, they must be qualified,
starting with ``/``, ``./`` or ``../``. This simplifies the command line
syntax, overall, without introducing troublesome ambiguities.

tmpx
----

The `tmpx` subcommand bundles together archives, environment settings and an
executable or shell command in to a Bourne-compatible script that runs the
command or executable in a temporary directory, after having unpacked the
archives and set the environment. After execution, the temporary directory is
removed (or not, depending on the ``-rm[10!_]`` family of options).

  ``-ar <path>``
    An archive to include in the generated shell script. If no archives are
    specified, or ``-ar -`` is given, then STDIN will be included.

  ``-rm0``, ``-rm1``, ``-rm_``, ``-rm!``
    By default, the temporary directory created by the script will be deleted
    no matter the exit status status of the task. These options cause a script
    to be generated that deletes the temporary directory only on success, only
    on failure, always (the default) or never.

  ``-b <size>``
    Please see the documentation for this option, shared with `shdat`, below.

  ``-o <path>``
    By default, the generated script is sent to STDOUT. With ``-o``, output is
    redirected to the given path.

  ``-e <path>``
    Causes the file specified to be packaged as the task to be run. A binary
    executable, a Ruby script or a longish shell script all fit here.

In addition to these options, arguments of the form ``VAR=VALUE`` are
recognized as environment mappings and stored away in the script, to be
sourced on execution.

Without ``-e``, the `tmpx` subcommand tries to find the task to be run as a
sequence of arguments delimited by a run of slashes. The following forms are
all recognized:

.. code-block:: text

    arx tmpx ...some tmpx args... // ...command... // ...more tmpx args...
    arx tmpx // ...command... // ...some tmpx args...
    arx tmpx ...some tmpx args... // ...command...

The slash runs must have the same number of slashes and must be the longest
continuous runs of slashes on the command line. The command will be included
as is in a Bourne shell script.

shdat
-----

The `shdat` subcommand translates binary data in to a shell script which
outputs the binary data. The data is encoded in HERE documents in such a way
that data without NULs is not changed and that data with NULs is minimally
expanded: about 1% for randomish data like compressed tarballs and about 10%
in pathological cases.

The `shdat` subcommand can be given any number of paths, which will be
concatenated in the order given. If no path is given, or if ``-`` is given,
then STDIN will be read.

  ``-b <size>``
    The size of data chunks to place in each HERE document. The argument is a
    positive integer followed by suffixes like ``B``, ``K``, ``KiB``, ``M``
    and ``MiB``, in the manner of ``dd``, ``head`` and many other tools. The
    default is 4MiB.  This is unlikely to make a difference for you unless the
    generated script is intended to be run on a memory-constrained system.

  ``-o <path>``
    By default, the generated script is sent to STDOUT. With ``-o``, output is
    redirected to the given path.

Examples
--------

.. code-block:: bash

  # Installer script that preserves failed builds.
  git archive HEAD | bzip2 | arx tmpx -rm0 // make install > go.sh
  # Now install as root; but don't log in as root.
  cat ./go.sh | ssh joey@hostname sudo /bin/sh

