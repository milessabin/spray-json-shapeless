# ENSIME
the ENhanced Scala Interaction Mode for Emacs

## Features

- Highlight errors and warnings in your code buffers.
- Inspect the type of any expression.
- Type member name completion.

## System Requirements

- Emacs 23(?) It may well work with earlier versions, but I haven't tested.
- A Unixy OS. It shouldn't be much work to get it running on windows, but you're on your own for now :)

## Installation

__scala-mode__

ENSIME is designed to compliment scala-mode. scala-mode can be found in the scala distribution under ./misc/scala-tool-support/emacs/

__ensime-mode__

Download and unpack the ENSIME project into a directory of your choosing. 

Add the following lines to your .emacs file:
    (require 'scala-mode)
    (add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
    (add-to-list 'load-path "ENSIME_ROOT/src/elisp/")
    (require 'ensime)

Create an .ensime configuration file for your project. See [.ensime](http://github.com/aemoncannon/ensime/blob/master/.ensime) in the root of the ENSIME distribution for an up-to-date example.

Finally, open one of the source files for your scala project, and execute M-x ensime. Follow the minibuffer instructions to specify the location of your .ensime project file. 

## Usage

__C-c c__  - Force a type-check. Otherwise type-checking takes place whenever a file is saved.

__C-c t__  - Inspect the type of the expression under the cursor.

__TAB__    - Complete a type member.

## Troubleshooting

You may want to examine the contents of the \*inferior-ensime-server\* buffer. This buffer collects the stdout and stderr of the server process, which is useful for debugging.


  





