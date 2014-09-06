ToolCabal
=========

Extending Cabal with Plugins, Preprocessors and Multi-target Compilers

This a rewrite form scratch of the build system of cabal

This is a beta version and documentation is not yet complete.

The following is supported:
- Multiple targets and flavours to be build simultaneously (only GHC)
  - Use --compiler-target=TARGET and --compiler-flavour=FLAVOUR flags
- use preprocessors directly in the package description
  - use-tool PREPROCESSOR (INPUTFILE, OUTPUTFILE, FLAGS)
      - FLAGS are always written as -f -f=OPTION --f --f=OPTION syntax
- plugins for preprocessors
  - see for an example the istTool.hs file inside TestProject
- For the new package description format
    - see example inside the TestProject

To test:
- build the tools library and toolCabal executable (using cabal)
- run toolCabal in the TestProject folder

