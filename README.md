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

__Project Configuration__

ENSIME has built-in support for generating configuration files. In Emacs, execute M-x ensime-config-gen. Follow directions in the mini-buffer to create a .ensime file for your project.. 

ENSIME will try to guess the type(sbt, mvn, etc) of your project, based on the files and directory structure. If the config generator does a poor job for your project, please let us know so we can improve it. And of course you can still create the .ensime file for your project manually. See the section on the .ensime format below.


__Permissions__

Verify that the startup script (usually bin/server.sh) has executable permissions.



__Note for sbt Users__ 

Customize the ensime-sbt-compile-on-save variable if you'd like sbt to recompile your project whenever you save. This is disabled by default as it consumes a lot of CPU without much benefit over ENSIME's built-in type checking.


__Note for Scala Standard Library (or other giant project) hackers__ 

You may want to increase the jvm heap size to give ENSIME some more breathing room. We've had some reports of ENSIME hanging when retrieving type information in huge projects. You can add the necessary flags in bin/server.sh. 



## Installation from Git Repository (for ENSIME hackers)

ENSIME is an sbt project and we have a custom sbt task, 'dist', that generates the distributable directory structure. When hacking ENSIME, you don't want to run the ensime server directly from the git clone. First, run 'sbt dist'. Then, follow the install instructions above, using CLONE_DIR/dist as your server-root.


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



## Configuration Format

The .ensime file must be placed in the root directory of your project(or sub-project if you have a multi-module sbt build). The contents of the file must be a valid Emacs-Lisp S-Expression. 
<br/>
--------------------
<br/>
Here's a quick primer on ELisp values:
<br/>

__"..."__   -  A String

__t__   -  True

__nil__    -  False or opposite of t.

__(...)__    -  A literal list.

__:abcd123__   -  A keyword

__(:key1 val1 :key2 val2)__    -  A 'property list'. Acts like a literal dictionary, indexed by keyword.

<br/>
--------------------
<br/>
And now a description of all the available configuration options:
<br/>

__:server-root "...."__

Required. The absolute path to the root of your ENSIME distribution. Note, this is not your project directory.
<br/>

__:server-cmd  "...."__

The command with which to invoke the ENSIME server. By default, this will be set to "bin/server.sh" on Unix systems and "bin/server.bat" on Windows.
<br/>


__:server-host "localhost"__

The host to connect to. Connecting to remote ENSIME servers is not currently supported (though it may work...)
<br/>


__:use-sbt t__

Assume a standard sbt directory structure. Look in default sbt locations for dependencies, sources, target, etc.
<br/>


__:sbt-compile-conf "compile"__<br/>
__:sbt-runtime-conf "runtime"__

Specify the names of dependency profiles to be used for compilation and runtime scenarios. Only necessary if you have custom configurations!
<br/>

  
__:use-maven t__

Use an existing pom.xml to determine the dependencies for the project. A Maven-style directory structure is assumed.
<br/>


__:maven-compile-scopes "compile"__<br/>
__:maven-runtime-scopes "runtime"__

Specify the names of dependency profiles to be used for compilation and runtime scenarios. Only necessary if you have custom scopes!
<br/>

__:use-ivy t__

Use an existing ivy.xml to determine the dependencies for the project. A Maven-style directory structure is assumed.
<br/>


__:ivy-compile-conf "compile"__<br/>
__:ivy-runtime-conf "compile"__

Specify the names of dependency profiles to be used for compilation and runtime scenarios. Only necessary if you have custom configurations!
<br/>


__:project-package "...."__

The main scala package for your project. Used by ENSIME to populate the project outline view. 
<br/>


__:sources ([dir | file]*)__

Manually include source files by directory(recursively) or by filename. If directory is given, only .scala and .java files will be considered.
<br/>


__:dependency-jars ([dir | file]*)__

Manually include jars by directory(recursively) or by filename.
<br/>


__:compile-dependency-jars ([dir | file]*)__

Manually include jars by directory(recursively) or by filename, to be included only at compile time.
<br/>


__:runtime-dependency-jars ([dir | file]*)__

Manually include jars by directory(recursively) or by filename, to be included only at run time.
<br/>


__:dependency-dirs ([dir | file]*)__

Manually include directories of .class files.
<br/>


__:target dir__

Manually specify the target of the project build process. Should be the directory where .class files are written. The target is used to populate the classpath when launching the inferior scala repl and the debugger.
<br/>




  





