# ENSIME

ENhanced Scala Interaction Mode for text Editors
--- especially [GNU Emacs](http://www.gnu.org/software/emacs/).

ENSIME brings IDE-like features to your favourite text editor, such as:

- Show the `type` of the symbol under the cursor.
- Contextual completion for `var`s, `val`s and `def`s.
- Add an import for the symbol under the cursor.
- Fast classpath search (types and members).
- Jump to source code or documentation.
- Browse packages and type hierarchies.
- Find all references to a symbol.
- Refactorings (rename, organize imports, extract method).
- REPL with stack trace highlighting.
- Errors and warnings in your code: *red squigglies*.
- Debugging

and many more.


# Contributions

This project is actively community maintained, and we are very pleased
to see contributions from new members.

If you use this software you are already well placed, as a Scala
developer, to make it even better.

You can help out by:

* [Triaging our open tickets](http://codetriage.com/ensime/ensime-server)
* [Fixing a bug](http://github.com/ensime/ensime-server/issues?labels=Bug)
* [Helping with the current Milestone](http://github.com/ensime/ensime-server/issues/milestones)
* Sending an unsolicited pull request with a new feature
* Having a conversation on the [ENSIME Google Group](https://groups.google.com/forum/#!forum/ensime)
* Telling your co-workers!

We are using some great technologies to automate our build and testing process:

* Kanban project planning from [waffle.io](https://waffle.io/ensime/ensime-server)
* Continuous Integration from [travis-ci.org](https://travis-ci.org/ensime/ensime-server)
* ~~Coverage reports from coveralls.io~~ [#391](/ensime/ensime-server/issues/391)
* Binary distribution from [bintray.com](https://bintray.com/ensime/maven/ensime/view/files/org/ensime)
* Emacs distributions from [MELPA](http://melpa.milkbox.net/#/ensime)

Along with unit testing, we have automated coverage checks and code
formatting as part of our build process. Pull requests will only be
accepted if the build and tests are successful, and coverage has not
decreased. Pull requests must be reviewed and should not be merged
by the person who created the request (except for trivial changes
and hotfixes).

We have branches for older versions of scala, which are merged regularly.


# Quick Start

There are a few ways to install the ENSIME server:

1. Install [ensime-emacs](http://github.com/ensime/ensime-emacs) and it will do it for you automatically.
2. Download a recent `-assembly.jar` binary from [bintray.com](https://bintray.com/ensime/maven/ensime/view/files/org/ensime).
3. Build from source: fork this repo, clone locally and `sbt assembly`.

If you download manually or build from source, you'll need to move or
link the `-assembly.jar` to where the editor plugin expects to find
it: e.g. `~/.emacs.d/ensime-servers/SCALA_VERSION/`. When developing
the ENSIME server itself, it is convenient to create a symbolic link
from here to the `target/classes/scala-VERSION` output directory.


## Further Information

Most of the server documentation is in the code itself. A readable
version of the SWANK protocol is documented in the
[ENSIME User Manual](http://ensime.github.io/) (although best to always check the
code.)

[Older releases](https://www.dropbox.com/sh/ryd981hq08swyqr/V9o9rDvxkS/ENSIME%20Releases)
are bundled with the emacs plugin.
