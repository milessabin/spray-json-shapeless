# ENSIME - ENhanced Scala Interaction Mode for Emacs

ENSIME provides IDE-like support for developing Scala code in Emacs.

## Features: 

- Highlights errors and warnings in your code buffers.
- Inspect the type of any expression, 
- Type member name completion


## Installation:

- Download and install scala-mode for Emacs.
- Download and extract ENSIME to a directory of your choosing.
- Add the following lines to your .emacs file:

ksjdf ksd f
skdfjdsf dkjf

    (require 'scala-mode)
    (add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
    (add-to-list 'load-path "ENSIME_ROOT/src/elisp/")
    (require 'ensime)


- Create a .ensime configuration file for your project. See the example .ensime in the root of the ENSIME distribution.
- In a scala source buffer, M-x ensime


  





