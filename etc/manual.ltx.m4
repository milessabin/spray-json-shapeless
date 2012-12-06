% NO_MANUAL_EDIT_WARNING
\documentclass{article}

\usepackage[utf8]{inputenc}
\usepackage{graphics}
\usepackage{amsmath}

\newenvironment{mylisting}
{\begin{list}{}{\setlength{\leftmargin}{1em}}\item\bfseries}
  {\end{list}}


\title{ENSIME User Manual}
\author{Aemon Cannon}

\begin{document}

\maketitle

\begin{center}
\textbf{Last Updated}: MODIFIED_DATE
\end{center}

\newpage

\tableofcontents

\newpage


\pagestyle{empty}


\section{Introduction}

\subsection{What is ENSIME?}
ENSIME is the ENhanced Scala Interaction Mode for Emacs. It provides many features that are commonly found only in IDEs, such as live error-checking, symbol inspection, package/type browsing, and basic refactorings. ENSIME's architecture is client/server: a thin Emacs client communicates with an analysis server written in Scala. The client component is based heavily on the SLIME environment for Common Lisp - using the same Swank RPC protocol. The server component services these calls using an instance of the official Scala compiler, so the results should always be consistent with the commandline compiler.

\section{Installation}

\subsection{System Requirements}

\begin{itemize}
\item Emacs 22 or later (23 is recommended)
\item Linux, Mac OSX, Windows
\item JVM Version 6
\item Scala 2.8.x or 2.9.x compatible project
\end{itemize}

\subsection{Getting Started}
\label{sec:install}

\noindent
\textbf{scala-mode}:\\
Although it's not required, ENSIME is designed to compliment an existing scala major mode. scala-mode2 is an excellent scala mode, and can be found at https://github.com/hvesalai/scala-mode2\\

\noindent
\textbf{ensime-mode}:\\
Download the latest ENSIME distribution from github.com at \begin{verbatim}http://github.com/aemoncannon/ensime/downloads\end{verbatim} Make sure you get the version that is appropriate for your Scala version. Unpack the ENSIME distribution into a directory of your choosing, and add the following lines to your .emacs file:

\begin{mylisting}
\begin{verbatim}
;; Load the ensime lisp code...
(add-to-list 'load-path "ENSIME_ROOT/elisp/")
(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

\end{verbatim}
\end{mylisting}


\noindent
\textbf{Create a .ensime config file}:\\
See section \ref{sec:config}.\\


\noindent
\textbf{Finally...}\\

\noindent
Execute \emph{M-x ensime} from Emacs. Follow the instructions in the minibuffer to select your project. If you encounter any problems, see section \ref{sec:troubleshooting} for troubleshooting.



\section{Creating a Project}


\subsection{Generating a Config File}
\label{sec:config}


\subsubsection{Using sbt to Generate a Config File}

\noindent
First, you need to install the ENSIME sbt plugin. Add the following lines to your project/plugins.sbt:

\begin{mylisting}
\begin{verbatim}
addSbtPlugin("org.ensime" % "ensime-sbt-cmd" % "VERSION")
\end{verbatim}
\end{mylisting}

\noindent
Replace VERSION with the latest version of the plugin, available at https://github.com/aemoncannon/ensime-sbt-cmd. Then, from an sbt shell, generate your ENSIME project:

\begin{mylisting}
\begin{verbatim}
ensime generate
\end{verbatim}
\end{mylisting}

\noindent
You should now have a .ensime file in the root of your project. Instead of editing this file directly, if you need to adjust ENSIME configuration settings, you can change the value of the sbt setting \emph{ensime-config}. The value should be a lisp S-Expression, just like the .ensime configuration format. The ENSIME sbt plugin includes some helpers for building S-Expressions. For example, your Build.scala might include the following:

\begin{mylisting}
\begin{verbatim}
import org.ensime.sbt.Plugin.Settings.ensimeConfig
import org.ensime.sbt.util.SExp._

ensimeConfig := sexp(
  key(":compiler-args"), sexp("-Ywarn-dead-code", "-Ywarn-shadowing"),
  key(":formatting-prefs"), sexp(
    key(":alignParameters"), true
  )
)
\end{verbatim}
\end{mylisting}

If you have multiple sbt subprojects, they may each specify different ensime-config values.


\subsubsection{Generating Configs for Other Project Types}

ENSIME includes a wizard for automatically generating configuration files. In Emacs, execute \emph{M-x ensime-config-gen}. Then simply follow the directions in the mini-buffer to create a .ensime file for your project.  ENSIME will try to guess the type(mvn, custom, etc) of your project, based on the files and directory structure. If the config generator does a poor job for your project, please let us know so we can improve it. And of course you can still create the .ensime file for your project manually. See the section on the .ensime format below.

\subsection{Notes on Specific Project Types}

\subsubsection{SBT}

\noindent
\textbf{Inferior SBT}:\\
The keystrokes \emph{C-c C-v s} will launch (or switch to an existing) inferior sbt session.\\

\noindent
\textbf{Compile-on-Save}:\\
If the value of the Emacs-Lisp variablt ensime-sbt-compile-on-save is non-nil ENSIME will invoke the 'compile' task in the inferior sbt process(presuming you have one running) whenever you save a Scala buffer. This option is enabled by default.\\


\subsubsection{Other}
See section \ref{sec:configformat} for how to specify dependency, source, and class-output locations.


\subsection{Config File Format}
\label{sec:configformat}

Each project \emph{must} have a .ensime file. The .ensime file contains the configuration for your project, and must be located in your project's root directory.The contents of the file must be a valid Emacs-Lisp S-Expression. Here's a quick primer on ELisp values.

\vspace{1 cm}

\begin{tabular}{|l|l|}
  \hline
  {\bf "..."} & A String  \\ \hline
  {\bf t} & True \\ \hline
  {\bf nil} & False, null, or opposite of t.  \\ \hline
  {\bf (...)} & A literal list. \\ \hline
  {\bf :abcd123} & A keyword \\ \hline
  {\bf (:key1 val1 :key2 val2)} & An indexed property-list. \\ \hline
\end{tabular}

\vspace{1 cm}

What follows is a description of all available configuration options. Required options are marked as 'Required'. Any filename or directory paths may be relative to the project root.

\vspace{5 mm}

% Inserted by gen_manual.sh
CONFIG_DOCUMENTATION  

\section{Usage}

\subsection{Startup}
To start ensime type \emph{M-x ensime}. You only need to do this once per project session. Follow the minibuffer instructions to specify the location of your .ensime project file. Bear in mind that the server may take several seconds to finish loading and analyzing your project's sources. To watch the progress of the ENSIME startup, switch to the \emph{*inferior-ensime-server*} buffer.

\subsection{Symbol and Member Completion}
ENSIME completion is initiated by pressing the \emph{TAB} key. To complete a symbol, type the first couple characters, then press \emph{TAB}. Currently this works for local variables, method parameters, unqualified method names, and type names. To complete a type member, type '.' or \emph{SPACE} followed by \emph{TAB}.\\

\noindent
\textbf{Completion menu key commands}:\\
Candidates can be scrolled with \emph{M-n} and \emph{M-p} or \emph{UP} and \emph{DOWN}. Candidates can be searched by typing \emph{C-s}. Press \emph{TAB} again to complete a common prefix. To cancel completion, type \emph{C-g}. Finally, if you've selected the completion you want, press \emph{ENTER}. If the selected completion was a method name, the minibuffer will display help for the method parameters.

\subsection{Type/Package Inspector}


\noindent
\textbf{Invocation}:\\
Control+Right-Clicking on a symbol in a Scala buffer, or typing \emph{C-c C-v i} while the point is over a symbol will launch the type inspector. Typing \emph{C-c C-v o} will open the inspector on the current project's main package. \emph{C-c C-v p} will inspect the package of the current source file. Use the command \emph{M-x ensime-inspect-by-path} to inspect an arbitrary type or package.\\

\noindent
\textbf{Package Inspector}:\\
Displays a hierarchical view of a package, including all top-level types. Select a type to open the Type Inspector.\\

\noindent
\textbf{Type Inspector}:\\
Lists the interfaces that contribute members to the inspected type. List each interface's methods, with full type signatures. If the type has a companion object/class, a link to the companion will appear under the heading.\\

\noindent
\textbf{Navigation}:\\
Most things in the inspector are hyper-linked. You can click these links with the mouse or position your cursor over them and press ENTER. A history is kept of all the pages you view in the inspector. Go back in this history by typing '\emph{,}' and forward by typing '\emph{.}'.


\subsection{The Scala REPL}

First, ensure that you've set the :target directive in your config file. The REPL will load your project classes from the :target directory. Then, type \emph{C-c C-v z} to launch the embedded Scala REPL. The REPL should be launched with all your project classes loaded and available. Please note that the Scala 2.8 REPL tab-completion does not currently work under ENSIME.

\subsection{Incremental Building}

Incremental building allows for fast turn-around in the running/testing of your application. The building/rebuilding support in ENSIME is intended for those who are not already using the sbt build system, as sbt users will probably wish to continue using the sbt shell's support for incremental building (it may still be worth a try though, as the build-manager included in ENSIME uses a finer grained check for modifications).

First, ensure that you've set the :target directive in your config file. The :target directory is where the classes will be written by the incremental builder. Then, type \emph{C-c C-b b} to start building your project. When the build is finished, a window will appear listing the result of the build. After subsequent source changes, you may type \emph{C-c C-b r} to rebuild only those parts of the project that depend on things you've changed. 

\subsection{Debugging}

Debugging support in ENSIME is a work in progress. Feedback is welcome.

\noindent
\textbf{Break Points}:\\
With your cursor on a line of Scala source, type \emph{C-c C-d b} to set a breakpoint. Type \emph{C-c C-d u} to remove the breakpoint. Note that breakpoints can be added and removed outside of any debug session. Breakpoints are not, however, persisted between runs of ENSIME.\\

\noindent
\textbf{Launching the Debugger}:\\
Type \emph{C-c C-d r} to launch the embedded Scala Debugger. ENSIME will prompt you for the class (with 'main' function) that you want to run (tab-completion works here), and then launch the debug VM. The first breakpoint your program hits will be highlighted and centered in Emacs.\\

\noindent
\textbf{Run Control}:\\
Type \emph{C-c C-d c} to continue after hitting a breakpoint, or \emph{C-c C-d s} to step into the current line, or \emph{C-c C-d n} to step to the next line, or \emph{C-c C-d o} to step out of the current function.\\

\noindent
\textbf{Value Inspection}:\\
When execution is paused, with your cursor over a local variable, type \emph{C-c C-d i} to inspect the runtime value of a variable.

\noindent
\textbf{Show Backtrace}:\\
When execution is paused, type \emph{C-c C-d t} to display the current backtrace.

\subsection{Refactoring}

\noindent
\textbf{Rename}:\\
Place your cursor over the symbol you'd like to rename. Type \emph{M-x ensime-refactor-rename} and follow the minibuffer instructions.\\

\noindent
\textbf{Organize Imports}:\\
Type \emph{M-x ensime-refactor-organize-imports} in a Scala source buffer. Follow the minibuffer instructions.\\

\noindent
\textbf{Extract Method}:\\
Select a region by setting the mark using \emph{C-SPACE} and then placing the point at the end of the region. All selected code will be extracted into a helper method. Type \emph{M-x ensime-refactor-extract-method} and follow the minibuffer instructions.\\

\noindent
\textbf{Inline Local}:\\
Place your cursor over the local val whose value you'd like to inline. Type \emph{M-x ensime-refactor-inline-local} and follow the minibuffer instructions.\\

\subsection{Global Type and Method Search}

Type \emph{C-c C-v v} to start a global search. Type space separated keywords to filter the results of the search. For example, if I wanted to find java.util.Vector, I might start by typing 'vector', which would list all symbols containing the word 'vector'(case-insensitive), and then I would type 'java' to further refine the search. \emph{C-p} and \emph{C-n} move the selection up and down, respectively, and \emph{ENTER} will jump to the source or definition of the selected symbol.

Note that typing a keyword with a capital letter will automatically enable case-sensitivity.

\subsection{Source Formatting}

ENSIME uses the Scalariform library to format Scala sources. Type \emph{C-c C-v f} to format the current buffer. See section \ref{sec:configformat} for instructions on how to customize the formatting preferences.


\subsection{Semantic Highlighting}

Normally syntax highlighting is based on the \emph{syntactic} aspects of the source code. Semantic Highlighting adds color-coding based on semantic properties of the source. For example: a syntax highlighter can't tell whether a given identifier is a var or a val or a method call. Semantic Highlighting on the other hand can color vars differently to warn of their mutability. 


Semantic Highlighting is \emph{disabled} by default. People use a wide variety of color schemes in Emacs; it would have been difficult to arrive at coloring scheme that worked well for everyone. 

Enabling Semantic Highlighting is as simple as changing the value of the variable ensime-sem-high-faces, which stores a list of (symbolType . face) associations. A ``face'' can be a reference to an existing Emacs face, such as font-lock-keyword-face, or a list of the form (:foreground ``color''), where ``color'' is either a standard Emacs color (such as ``slate gray'') or a hex value like ``\#ff0000''. For example, you might add the following to your .emacs file:
\begin{mylisting}
\begin{verbatim}
(setq ensime-sem-high-faces
  '(
   (var . (:foreground "#ff2222"))
   (val . (:foreground "#dddddd"))
   (varField . (:foreground "#ff3333"))
   (valField . (:foreground "#dddddd"))
   (functionCall . (:foreground "#84BEE3"))
   (param . (:foreground "#ffffff"))
   (class . font-lock-type-face)
   (trait . (:foreground "#084EA8"))
   (object . (:foreground "#026DF7"))
   (package . font-lock-preprocessor-face)
   ))
\end{verbatim}
\end{mylisting}
Once the value of ensime-sem-high-faces has changed, the next time you save a file, the designated symbol types will be highlighted.
By the way, the symbol types in the example above are all that are currently supported. 

\subsection{Scaladoc and Javadoc Browsing (customizing)}

If ENSIME cannot find the source for a type or member, it will instead try to browse to the www documentation. Support is included for the java and scala standard libraries, as well as the android class library. To add your own doc library, you need to add a handler to the \emph{ensime-doc-lookup-map}. This handler list is made up of (regex . handler) pairs, where regex is a regular expression string that will be matched against the fully qualified type name, and handler is a function that will be applied to the requested type and member and should return a url. Here's an example of how you might add new java docs for classes in com.example:

\begin{mylisting}
\begin{verbatim}

(defun make-example-doc-url (type &optional member)
  (ensime-make-java-doc-url-helper 
    "http://developer.example.com/apidocs/" type member))

(add-to-list 'ensime-doc-lookup-map '("^com\\.example\\." . make-example-doc-url))

\end{verbatim}
\end{mylisting}

\noindent
Note that \emph{ensime-make-java-doc-url-helper}, and its Scala equivalent \emph{ensime-make-scala-doc-url-helper}, are provided for doing the harder work of building the url paths.



\subsection{Command Reference}

\vspace{5 mm}

\noindent
{\bf TAB}\\
Start completing a method/variable.
\vspace{5 mm}

\noindent
{\bf C-c C-v i  or  Control+Right-Click}\\
Inspect the type of the expression under the cursor.
\vspace{5 mm}

\noindent
{\bf M-.  or  Control+Left-Click}\\
Jump to definition of symbol under cursor.
\vspace{5 mm}

\noindent
{\bf M-,}\\
Pop back to previously visited position.
\vspace{5 mm}

\noindent
{\bf C-c C-v .}\\
Select the surrounding syntactic context. Subsequent taps of '.' and ',' will grow and shrink the selection, respectively.
\vspace{5 mm}

\noindent
{\bf C-c C-v v}\\
Search globally for methods or types.
\vspace{5 mm}

\noindent
{\bf Control+Right-Click(on an imported package)}\\
Inspect the package under cursor.
\vspace{5 mm}

\noindent
{\bf Mouse Hover}\\
Echo the type of the expression under the cursor.
\vspace{5 mm}

\noindent
{\bf C-c C-v p}\\
Inspect the package of the current source file.
\vspace{5 mm}

\noindent
{\bf C-c C-v o}\\
Inspect the package specified in .ensime as :package.
\vspace{5 mm}

\noindent
{\bf C-c C-v r}\\
List all references to the symbol under the cursor.
\vspace{5 mm}

\noindent
{\bf .}\\
Forward one page in the inspector history.
\vspace{5 mm}

\noindent
{\bf ,}\\
Backward one page in the inspector history.
\vspace{5 mm}

\noindent
{\bf C-n or TAB}\\
Forward one link in the inspector.
\vspace{5 mm}

\noindent
{\bf C-p}\\
Backward one link in the inspector.
\vspace{5 mm}

\noindent
{\bf C-c C-v s}\\
Switch to the sbt command-line (works for sbt projects only)
\vspace{5 mm}

\noindent
{\bf C-c C-v z}\\
Switch to the scala interpreter, with project classes in the classpath.
\vspace{5 mm}

\noindent
{\bf C-c C-v c}\\
Typecheck the current file.
\vspace{5 mm}

\noindent
{\bf C-c C-v a}\\
Typecheck all files in the project.
\vspace{5 mm}

\noindent
{\bf C-c C-v e}\\
Show all errors and warnings in the project.
\vspace{5 mm}

\noindent
{\bf C-c C-v f}\\
Format the current Scala source file.
\vspace{5 mm}

\noindent
{\bf C-c C-v u}\\
Undo a refactoring or formatting change.
\vspace{5 mm}

\noindent
{\bf M-n}\\
Go to the next compilation note in the current buffer.
\vspace{5 mm}

\noindent
{\bf M-p}\\
Go to the previous compilation note in the current buffer.
\vspace{5 mm}

\noindent
{\bf C-c C-d \emph{x}}\\
Where \emph{x} is one of:
\begin{itemize}
\item {\bf d}  Start and run the debugger. 
\item {\bf r}  Start and run the debugger. 
\item {\bf b}  Set a breakpoint.
\item {\bf u}  Clear a breakpoint.
\item {\bf s}  Step.
\item {\bf n}  Step over.
\item {\bf o}  Step out.
\item {\bf c}  Continue from a breakpoint.
\item {\bf q}  Kill the debug session.
\item {\bf i}  Inspect the local variable at cursor.
\item {\bf t}  Show backtrace.
\end{itemize}
\vspace{5 mm}

\noindent
{\bf C-c C-r \emph{x}}\\
Where \emph{x} is one of:
\begin{itemize}
\item {\bf r}  Rename the symbol at point. 
\item {\bf o}  Organize imports. 
\item {\bf l}  Extract local. 
\item {\bf m}  Extract method. 
\item {\bf i}  Inline local. 
\item {\bf t}  Add import for type at point.
\end{itemize}
\vspace{5 mm}

\noindent
{\bf C-c C-b \emph{x}}\\
Where \emph{x} is one of:
\begin{itemize}
\item {\bf b}  Build the entire project. 
\item {\bf r}  Rebuild the project, incrementally. 
\end{itemize}
\vspace{5 mm}

\noindent
{\bf M-x ensime-reload}\\
Reload the .ensime file and recompile the project. Useful if you hit a server bug.
\vspace{5 mm}

\noindent
{\bf M-x ensime-config-get}\\
Start the automatic configuration file generator.
\vspace{5 mm}


\section{Troubleshooting}
\label{sec:troubleshooting}

\subsection{Diagnosing Issues}
You may want to examine the contents of the \emph{*inferior-ensime-server*} buffer. This buffer collects the stdout and stderr of the server process, which is useful for debugging. If the compiler is in a broken state, you can restart it with M-x ensime-reload. Otherwise, if things are irreparably b0rked, you can always kill the \emph{*inferior-ensime-server*} buffer (which kills the server process) and restart ensime with M-x ensime. 

If you've hit a recurring bug, please post an issue to github.com/aemoncannon/ensime. Please include your OS, Emacs version, ENSIME version, and the contents of \emph{*inferior-ensime-server*}.

\subsection{Emacs Binary Search Path}
When launching the embedded sbt shell, or the Scala repl, ENSIME uses the Emacs start-process command. Rather than using the value of the PATH environment variable, this command searches for binaries using the paths stored at the Emacs variable exec-path. On some Windows and OSX machines, exec-path will not by default contain the value of PATH. See \begin{verbatim}http://xahlee.org/emacs/emacs_env_var_paths.html\end{verbatim} for more details. For example, the following Emacs Lisp could be used to manually add a Scala binary directory to the exec-path:

\begin{mylisting}
\begin{verbatim}
(setq exec-path (append exec-path (list "/home/aemon/scala/bin" )))
\end{verbatim}
\end{mylisting}


\subsection{Custom JVM Options}
If you're having problems with the default arguments (max heap, initial heap) that ENSIME uses in its startup script, you can modify the environment variable ENSIME\_JVM\_ARGS to override the arguments that are passed to the ENSIME Server JVM.

\newpage

\appendix

\section{Installation from Git Clone}

Note: This section is for people who want to hack on ENSIME itself.\\

\noindent
After cloning, run 'sbt update'. Then run 'sbt stage' to create the deployment directories underneath the root clone directory. Then follow the install instructions in section \ref{sec:install} above, substituting CLONE\_DIR/dist as the root of your ENSIME distribution.\\

\noindent
A common work-flow when hacking ENSIME:
\begin{itemize}
\item Edit source files
\item 'sbt stage'
\item Stop ENSIME server by killing \emph{*inferior-ensime-server*} buffer
\item Restart ENSIME with M-x ensime
\end{itemize}


\section{Running the End-to-End ENSIME Tests}

\begin{itemize}
\item 'sbt stage'
\item 'cd etc'
\item 'bash run\_emacs\_tests.sh'
\item Please be patient. These tests take a few mins to run. If all goes well you should see a buffer with a long list of 'ok's.
\end{itemize}


\section{Using the ENSIME Server with Other Editors}

The ENSIME server is intentionally editor agnostic. It is our hope that it may be used to provide semantic information to Scala modes in other text editors. In order to interact with the ENSIME server, your editor's extension mechanism should ideally be able to open a persistent socket connection and respond to asynchronous events on that socket. Otherwise it may be difficult to interact with some of the long-running calls.

\subsection{Starting the Server}

Emacs starts the ENSIME server using the server.sh script in the \emph{bin} folder of the ENSIME distribution. Rather than tell the server explicitely what tcp port it should bind to,we instead pass the filename of a temporary file to the script. The first thing the server does on startup is choose a random, open port, and write the number to the given file. Emacs then reads this file and connects to the server. 


\subsection{The Swank Protocol}
The Emacs ENSIME client communicates with the server using the Swank protocol. This protocol was originally developed for the SLIME lisp environment for Emacs. A socket connection is maintained for the duration of the session. The client and server exchange s-expressions. At the wire level, these messages are encoded as sequences of bytes. Each message is prepended with a fixed-size header denoting its length.

To send an s-expression, first encode the s-expression as a UTF-8 string. Determine the string's length in bytes and encode that length as a padded six-digit hexadecimal string.  Write this value (which will always be six bytes) to the output socket first, then write the UTF-8 encoded s-expression.  On the receiving side, the reader loop should read six bytes into a buffer and convert that into an integer, then read that number of bytes from the socket. The result is a UTF-8 string representation of the s-expression. This s-expression should then be parsed using a suitable lisp reader.\\

\vspace{5 mm} 

\includegraphics{wire_protocol.png} \\

\vspace{7 mm} 

See the E-Lisp function \emph{ensime-net-send} in ensime.el for details on how messages are sent from Emacs, and the function \emph{ensime-net-encode-length} for the implementation of the header encoding. See the functions readMessage and writeMessage in org.ensime.protocol.SwankProtocol to see how the messaging is handled in Scala. 

At the application level, the s-expressions encode RPC calls, RPC responses, and events. RPC calls are used to make requests of the server. Events are generally used for the server to communicate un-prompted, asynchronous activity to the client -- such as an error or warning generated during compilation. What follows is a commented excerpt from the initialization of an ENSIME session. These s-expressions were copied from the \emph{*ensime-events*} buffer in Emacs which logs all protocol events (useful for learning the application protocol!). Server messages are indented. Comments prefixed with \#.

\begin{mylisting}
\begin{verbatim}

# The client requests information about the server
(:swank-rpc
 (swank:connection-info) 1)

# Server responds. Note the numbering scheme for RPC calls: call & response 
# have the same call id (1 in this case).
(:return
 (:ok
  (:pid nil :server-implementation
	(:name "ENSIMEserver")
	:machine nil :features nil :version "0.0.1")) 1)

# The client initializes the session by sending the configuration. 
# This is read from the .ensime file.
(:swank-rpc
 (swank:init-project
  (:package "org.ensime" :root-dir "/home/aemon/src/misc/ensime/")) 2)

# The acknowledges the message and sends some useful project info 
# back to the client.
(:return
 (:ok
  (:name "ensime" :source-roots
		 ("/home/aemon/src/misc/ensime/src/main/scala" 
		 "/home/aemon/src/misc/ensime/src/main/java" 
		 "/home/aemon/src/misc/ensime/src/test/scala"))) 2)


# Server asks client to display a user message. 
# Note: this is not part of any RPC call - it's just an event from the server.
(:background-message 105 "Initializing Analyzer. Please wait...")

# Server alerts client that the analyzer is now ready for RPC requests.
(:compiler-ready t)

# Server tells client the result of the last full compilation 
# (nil means 0 notes (errors or warnings))
(:typecheck-result
 (:lang :scala :is-full t :notes nil))

\end{verbatim}
\end{mylisting}

\subsection{ENSIME Swank RPC: Version PROTOCOL_VERSION}

\subsubsection{Protocol Change Log}

\begin{verbatim}
PROTOCOL_CHANGE_LOG
\end{verbatim}


\subsubsection{Important Datastructures}

Certain datastructures, such as the \emph{position} structure used to describe a source position, are re-used in many RPC calls. Implementors may wish to factor these structures out as classes or utility functions.

% Inserted by gen_manual.sh
PROTOCOL_DATA_DOCUMENTATION  


\subsubsection{RPC Calls}

The ENSIME server understands all of the following RPC calls:

% Inserted by gen_manual.sh
PROTOCOL_RPC_DOCUMENTATION  

\subsubsection{Events}

The ENSIME server will dispatch the following types of events:

% Inserted by gen_manual.sh
PROTOCOL_EVENTS_DOCUMENTATION  



\subsection{Other Protocols}

The ENSIME server is designed to support pluggable protocols. org.ensime.protocol.SwankProtocol is just one implementation of the org.ensime.protocol.Protocol interface. Adding a new protocol (JSON-based, or binary or Java marshalled objects...) should only require adding a new implementation of org.ensime.protocol.Protocol. Please contact the ENSIME maintainer if this is your plan, however, since we still need to add a command-line switch to control the protocol that ENSIME uses.

\end{document}




