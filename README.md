# ENSIME

[![Bountysource](https://www.bountysource.com/badge/tracker?tracker_id=239449)](https://www.bountysource.com/trackers/239449-ensime?utm_source=239449&utm_medium=shield&utm_campaign=TRACKER_BADGE)
[![Build Status](https://travis-ci.org/ensime/ensime-server.svg?branch=master)](https://travis-ci.org/ensime/ensime-server)
[![Stories in Ready](https://badge.waffle.io/ensime/ensime-server.png?label=Low+Hanging+Fruit)](https://waffle.io/ensime/ensime-server)
[![Coverage Status](https://coveralls.io/repos/ensime/ensime-server/badge.png)](https://coveralls.io/r/ensime/ensime-server)
[![Melpa Status](http://melpa.milkbox.net/packages/ensime-badge.svg)](http://melpa.milkbox.net/#/ensime)

ENhanced Scala Interaction Mode for text Editors, especially [GNU Emacs](http://www.gnu.org/software/emacs/).

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

<!--* [Triaging our open tickets](http://codetriage.com/ensime/ensime-server)-->

* [Pick up some low hanging fruit](https://github.com/ensime/ensime-server/issues?labels=Low+Hanging+Fruit)
* [Fixing a bug](http://github.com/ensime/ensime-server/issues?labels=Bug)
* [Helping with the current Milestone](http://github.com/ensime/ensime-server/issues/milestones)
* Sending an unsolicited pull request with a new feature
* Having a conversation on the [ENSIME Google Group](https://groups.google.com/forum/#!forum/ensime)
* Telling your co-workers!

We are using some great technologies to automate our build and testing process:

* Kanban project planning from [waffle.io](https://waffle.io/ensime/ensime-server)
* Continuous Integration from [travis-ci.org](https://travis-ci.org/ensime/ensime-server)
* Coverage reports from coveralls.io [coveralls.io](https://coveralls.io/r/ensime/ensime-server)
* Binary distribution from [sonatype.org](http://www.sonatype.org/)
* Emacs distributions from [MELPA](http://melpa.milkbox.net/#/ensime)

Along with unit testing, we have automated coverage checks and code
formatting as part of our build process. Pull requests will only be
accepted if the build and tests are successful, and coverage has not
decreased. Pull requests must be reviewed and should not be merged
by the person who created the request (except for trivial changes
and hotfixes).

We have branches for older versions of scala, which are merged regularly.


# Quick Start

See our [Quick Start Guide](http://github.com/ensime/ensime-server/wiki/Quick-Start-Guide) to learn how to install and start ENSIME.

ENSIME is released on a continuous "rolling release" basis every time a pull request is merged. This dramatically speeds up the development cycle and you are advised to always update your ENSIME before reporting any issues.

If you are still experiencing a problem with the latest version of ENSIME, before reporting an issue please:

* check the [tickets flagged as FAQ](https://github.com/ensime/ensime-server/issues?labels=FAQ).
* check the [most recently updated tickets](http://github.com/ensime/ensime-server/issues?direction=desc&sort=updated) (others are probably talking about it already with workarounds).
* do a few keyword searches using the github search (top of the page) to see if anybody has reported this already.


## Further Information

Most of the server documentation is in the code itself. A readable
version of the SWANK protocol is documented in the
[ENSIME User Manual](http://ensime.github.io/) (although best to always check the
code.)

[Older releases](https://www.dropbox.com/sh/ryd981hq08swyqr/V9o9rDvxkS/ENSIME%20Releases)
are bundled with the emacs plugin.
