Protocol Version: 0.8.15 (Must match version at ConnectionInfo.protocolVersion)

Protocol Change Log:
  0.8.16
    Added swank:implicit-info
    Added new symbol designation types: "implicitConversion" and "implicitParams"
  0.8.15
    Removed all *undo* requests: they were too unreliable
    Removed all *patch* requests: they were too unreliable
    Removed replconfig: clients encouraged to read config directly
  0.8.14
    Added swank:symbol-by-name
    Removed swank:member-by-name
  0.8.13
    Added swank:doc-uri-for-symbol
  0.8.12
    Added swank:doc-uri-at-point
  0.8.11
    Some calls now take (:file "filename" :contents "xxxx")
      as a way to transmit source files:
        - swank:typecheck-file
        - swank:completions
    Made the "reload" parameter of swank:completions optional
    Added swank:format-one-source
  0.8.10
    Remove the config argument from init-project. Server loads
    configuration on its own.
  0.8.9
    Remove Incremental builder - removed
      swank:builder-init
      swank:builder-update-files
      swank:builder-add-files
      swank:builder-remove-files
  0.8.8
    Add optional :archive member to Position and RangePosition
  0.8.7
    Add optional file contents parameter to typecheck-file
  0.8.6
    Add support for ranges to type-at-point, inspect-type-at-point,
      type-by-name-at-point
  0.8.5
    DebugLocation of type 'field now gets field name from :field, not from :name
    The debug-to-string call now requires thread id
  0.8.4
    Add local-name to SymbolInfo
  0.8.3
    Add debug-to-string call.
    Refactor debug-value-for-name to debug-locate-name + debug-value
    Adds typecheck-files
    Unifies debug locations under DebugLocation
  0.8.2
    Debug attachment to remote VM
    CompletionInfo type-sig now has structure see CompletionSignature
  0.8.1
    Add patch-source.
    Completions now takes a 'reload' argument.
  0.8
    Add RPC calls for debugging
    Protocol is now explicitly UTF-8
  0.7.4
    Add optional 'owner-type-id' key to SymbolInfo
    Add optional 'case-sens' option to swank:completions call
  0.7.3
    Add optional 'to-insert' key to CompletionInfo
    Add optional a max results argument to swank:completions call
  0.7.2
    Get rid of scope and type completion in favor of unified
    swank:completions call.
    Wrap completion result in CompletionInfoList.
  0.7.1
    Remove superfluous status values for events such as :compiler-ready,
    :clear-scala-notes, etc.
  0.7
    Rename swank:perform-refactor to swank:prepare-refactor.
    Include status flag in return of swank:exec-refactor.
