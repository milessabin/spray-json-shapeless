## ENSIME - ENhanced Scala Interaction Mode for Emacs

ENSIME provides IDE-like support for developing Scala code in Emacs.

### Features: 

- Highlights errors and warnings in your code buffers.
- Inspect the type of any expression, 
- Type member name completion


### Installation:

__scala-mode__
ENSIME is designed to compliment scala-mode, so it is recommended that you 
first install scala-mode. It can be found in the scala distribution under 
./misc/scala-tool-support/emacs/

__ensime-mode__
Download and unpack the ENSIME project into a directory of your choosing. 

Add the following lines to your .emacs file:
    (require 'scala-mode)
    (add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
    (add-to-list 'load-path "ENSIME_ROOT/src/elisp/")
    (require 'ensime)

Create an .ensime configuration file for your project. See .ensime in the root 
of the ENSIME distribution for an up-to-date example.
[http://github.com/aemoncannon/ensime/blob/master/.ensime]

Finally, open one of the source files for your scala project, and execute M-x ensime.
Follow the minibuffer instructions to specify the location of your .ensime project 
file.

You may want to examine the contents of the *inferior-ensime-server* buffer. ENSIME is
a young project, and if there are bugs, they will likely be visible in the
server's output.


  





