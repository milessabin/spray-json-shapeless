[![Stories in Ready](https://badge.waffle.io/ensime/ensime-server.png?label=ready&title=Ready)](https://waffle.io/ensime/ensime-server)
# ENSIME
the ENhanced Scala Interaction Mode for Emacs

# Help wanted!

Please help developing this project. You'll find hacking instructions at the end of the document.
Also QA help is of high value so please go to the [project's Code Triage page](http://www.codetriage.com/ensime/ensime-server)
and start commenting on issues.

# Links
- [ Download releases ](https://www.dropbox.com/sh/ryd981hq08swyqr/V9o9rDvxkS/ENSIME%20Releases)
- [ Manual ](http://ensime.github.io/)
- [ Manual as PDF ](http://ensime.github.io/manual.pdf)
- [ Discussion Group ](http://groups.google.com/group/ensime?hl=en)


## Features

- Highlight errors and warnings in your code buffers, as you type.
- Inspect the type of any expression.
- Browse packages
- Completion for variables, methods, constructors, etc.
- Incrementally search through classpath symbols
- Find all references to a symbol
- Jump to symbol definitions, including inside source jars
- Semantic Highlighting
- Automated Refactorings (rename, organize imports, extract method)
- Source Formatting
- AST-based selection
- Supports sbt 12,13
- Embedded sbt shell
- REPL featuring stack trace highlighting with links to source code
- Debug support


## Demo Videos (some are very out of date!)

- [Overview (a bit out of date)](http://www.youtube.com/watch?v=A2Lai8IjLoY)
- [Searching](http://www.youtube.com/watch?v=fcgnAJz98QE)
- [Debugger Support](http://www.youtube.com/watch?v=v7-G6vD42z8)
- [Import Suggestions](http://www.youtube.com/watch?v=Ynp8Df7-paw&hd=1)



## System Requirements

- Emacs 22 or later.
- Linux, Mac OSX, Windows
- Java Runtime
- A Scala 2.9.x, 2.10.x, and 2.11.x (comming soon) project


## Documentation

- [The ENSIME User Manual](http://ensime.github.com/ensime-src/index.html)


## Getting Started

__1) Install scala-mode2__

Although it's not required, ENSIME is designed to compliment an existing scala major mode. scala-mode2 is an excellent scala mode, and can be found at https://github.com/hvesalai/scala-mode2

__2) Install the ENSIME Server__

Download the ENSIME distribution from the [releases page](https://www.dropbox.com/sh/ryd981hq08swyqr/V9o9rDvxkS/ENSIME%20Releases). Unpack the ENSIME distribution into a directory of your choosing. 

Add the following lines to your .emacs file:

    ;; load the ensime lisp code...
    (add-to-list 'load-path "ENSIME_ROOT/src/main/elisp/")
    (require 'ensime)

    ;; This step causes the ensime-mode to be started whenever
    ;; scala-mode is started for a buffer. You may have to customize this step
    ;; if you're not using the standard scala mode.
    (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


__3) If you are using sbt, install the ENSIME Sbt Plugin (otherwise, see the [manual](http://ensime.github.com/ensime-src/index.html#tth_sEc3.1.2))__

Add the following lines to your project/plugins.sbt file:

    addSbtPlugin("org.ensime" % "ensime-sbt-cmd" % "VERSION")

Replace VERSION with the latest version of the plugin, available on [the plugin page](https://github.com/ensime/ensime-sbt-cmd).  Then, from an sbt shell, generate your ENSIME project:
    
    ensime generate

Note: Currently, it may be necessary to first delete your project/target directories before running 'ensime generate'.

You should now have a .ensime file in the root of your project. There's no need to edit this file manually as you can now specify ENSIME settings directly from your sbt build file. Check the [manual](http://ensime.github.com/ensime-src/index.html#tth_sEc3.1.1) for details.


__4) Start ENSIME__

From inside Emacs, execute M-x ensime


## Developer Quick Start
Note: This section is for people who want to hack on ENSIME itself.

After cloning, and before you can run ENSIME, you must create the distribution directory structure. The sbt task 'stage' will create the directory 'dist' underneath the root clone directory. Then, follow the install instructions in section 2.2 above, substituting CLONE_DIR/dist as the root of your ENSIME distribution.


The work-flow I use when hacking ENSIME:

- Edit source files
- 'sbt stage'
- Stop existing ENSIME server by killing *inferior-ensime-server* buffer
- Restart ENSIME with M-x ensime
