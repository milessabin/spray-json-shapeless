# ENSIME
the ENhanced Scala Interaction Mode for Emacs

## Features

- Highlight errors and warnings in your code buffers.
- Inspect the type of any expression.
- Browse packages
- Completion-on-demand for variable, methods, constructores, etc.
- Jump to symbol definitions.
- sbt support
- Check out this [video](http://www.youtube.com/watch?v=A2Lai8IjLoY)
- or this [one](http://www.youtube.com/watch?v=v7-G6vD42z8) showcasing the debugger

## System Requirements

- Emacs 22 or later.
- Unix-like OS or Windows. Note that you'll need to use bin/server.bat on windows.
- Java Runtime
- Scala 2.8 compatible source and libraries. ENSIME is built against the 2.8 nightly Scala releases. 


## Installation from Distribution Zip

__scala-mode__

ENSIME is designed to compliment scala-mode. scala-mode can be found in the Scala distribution under ./misc/scala-tool-support/emacs/

__ensime-mode__

Download the latest ENSIME distribution from the github [downloads page](http://github.com/aemoncannon/ensime/downloads). Unpack the ENSIME distribution into a directory of your choosing. 

Add the following lines to your .emacs file:
    (require 'scala-mode)
    (add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
    (add-to-list 'load-path "ENSIME_ROOT/elisp/")
    (require 'ensime)
    (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
    ;; MINI HOWTO: open .scala file. Ensure bin/server.sh is executable. M-x ensime

__Project Configuration: .ensime __
ENSIME has built-in support for generating configuration files. In Emacs, execute M-x ensime-config-gen, then follow directions in the mini-buffer. ENSIME will try to guess what type of project you are defining. If the config generator does a poor job for your project, please let us know so we can improve it! and of course you can still create the .ensime file for your project manually.


__Permissions__

Verify that the startup script (usually bin/server.sh) has executable permissions.



__Note for sbt Users__ 

Customize the ensime-sbt-compile-on-save variable if you'd like sbt to recompile your project whenever you save. This is disabled by default as it consumes a lot of CPU without much benefit over ENSIME's built-in type checking.


__Note for Scala Standard Library (or other giant project) hackers__ 

You may want to increase the jvm heap size to give ENSIME some more breathing room. We've had some reports of ENSIME hanging when retrieving type information in huge projects. You can add the necessary flags in bin/server.sh. 



## Installation from Git Repository (for ENSIME hackers)

ENSIME is an sbt project and we have a custom sbt task, 'dist', that generates the distributable directory structure. When hacking ENSIME, you don't want to run the ensime server directly from the git clone. First, run 'sbt dist'. Then, follow the install instructions above, using CLONE_DIR/dist as your ensime-root.


## Usage

To enable ensime type M-x ensime. Follow the minibuffer instructions to specify the location of your .ensime project file. 

__TAB__    - Start completing a method/variable.

__C-c t  /  Double-Click__  - Inspect the type of the expression under the cursor.

__M-.  /  Control-Click__  - Jump to definition of symbol under cursor.

__M-,__  - Pop back to previously visited position.

__Double-Click(on an import statement)__  - Inspect the package under cursor.

__Mouse Hover__    - Echo the type of the expression under the cursor.

__C-c p__  - Inspect the package of the current source file.

__C-c o__  - Inspect the package specified in .ensime as :project-package.

__.__  - Forward one page in the inspector history.

__,__  - Backward one page in the inspector history.

__M-n  /  TAB__  - Forward one link in the inspector.

__M-p__  - Backward one link in the inspector.

__C-c C-a__  - Switch to the sbt command-line (works for sbt projects only)

__C-c C-z__  - Switch to the scala interpreter, with project classes in the classpath.

__C-c c__  - Type-check the current file.

__C-c a__  - Type-check all files in the project.




## Troubleshooting

You may want to examine the contents of the \*inferior-ensime-server\* buffer. This buffer collects the stdout and stderr of the server process, which is useful for debugging.


  





