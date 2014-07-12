module Tools.BasicTools.Types where

import Prelude hiding (init)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.List (intercalate)
import System.FilePath (combine, (</>), pathSeparator, (<.>))
import Control.Lens hiding (Setting, (<.>))
import Control.Monad.State
import Control.Monad.Except
import Language.Haskell.Exts (parseFile, Module(..), ImportDecl(..), ParseResult(..), ModuleName(..))


import Cabal
import Tools.Flag
import Tools.DSet
import Tools.Core.Core

import Debug

---------------------------------------
-- * Data types
---------------------------------------

---------------------------------------
-- ** Tool Data types
---------------------------------------

data Haddock = Haddock
  deriving (Ord, Eq, Show)

data HsColour = HsColour
  deriving (Ord, Eq, Show)

data CppHs = CppHs
  deriving (Ord, Eq, Show)

data UUAGC = UUAGC
  deriving (Ord, Eq, Show)

data GHC = GHC
  deriving (Ord, Eq, Show)

data UHC = UHC
  deriving (Ord, Eq, Show)

---------------------------------------
-- * Preprocesses and files
---------------------------------------

-- ** fileTypes
haskellType = "*.hs"
litHaskellType = "*.lhs"
agType = "*.ag"
cType = "*.c"
objectType = "*.o"


-- ** targets
javascriptFile = "plain javascript executable"
libJsFile = "plain javascript library"
javascriptDebugFile = "debug javascript executable"
libJsDebugFile = "debug javascript library"
exeFile = "plain bytecode executable"
libFile = "plain bytecode library"
profExeFile = "profiling bytecode executable"
profLibFile = "profiling bytecode library"
debugExeFile = "debug bytecode executable"
debugLibFile = "debug bytecode library"

-- ** utilities
getImportList :: FilePath -> ToolMonad [FilePath]
getImportList f = do
  pr <- liftIO $ parseFile f
  hf <- case pr of
    ParseOk a -> return a
  return $ S.toList $ S.fromList $ map (toFilePath . getImportModule) $ getImportDecls hf
  where toFilePath m = map (\x -> if x == '.' then pathSeparator else x) m <.> "hs"
        getImportDecls (Module _ _ _ _ _ i _) =  i
        getImportModule (ImportDecl _ (ModuleName m) _ _ _ _ _) = m
  

---------------------------------------
-- * Haddock
---------------------------------------

-- | Flags are valid for version 2.13.2
supportedHaddockFlags :: DSet Flag
supportedHaddockFlags = cssOnlyOne <&&> latexDependent <&&> htmlDependent <&&> (singleFlags <*|> (outputType <||> optionalFlags))
  where singleFlags = uniqueRequired $ S.fromList [help, version, interfaceVersion, ghcVersion, printGhcPath, printGhcLibdir]
        latexDependent = dependOnAll latexStyle $ S.singleton latex
        htmlDependent = dependAllOnOne (S.fromList [u, sourceBase, s, sourceEntity, commentsBase, commentsModule, commentsEntity, ccss, ctheme, builtInThemes, useContents, genContents, useIndex, genIndex, prettyHtml]) $ S.fromList [h]
        outputType = uniqueRequired $ S.fromList [h, latex, hoogle]
        optionalFlags = oneof $ S.fromList [b, o, l, i, d, p, t, q, v, ignoreAllExports, hide, optghc, w, noTmpCompDir]
        cssOnlyOne = uniqueOptional $ S.fromList [ccss, ctheme]

        -- The flags
        b                = Flag "B" ""                   (Just $ Free "DIR")       Nothing "path to a GHC lib dir, to override the default path"
        o                = Flag "o" "odir"               (Just $ Free "DIR")       Nothing "directory in which to put the output files"
        l                = Flag "l" "lib"                (Just $ Free "DIR")       Nothing "location of Haddock's auxiliary files"
        i                = Flag "i" "read-interface"     (Just $ Free "FILE")      Nothing "read an interface from FILE"
        d                = Flag "D" "dump-interface"     (Just $ Free "FILE")      Nothing "write the resulting interface to FILE"
        h                = Flag "h" "html"               Nothing                   Nothing "output in HTML (XHTML 1.0)"
        latex            = Flag ""  "latex"              Nothing                   Nothing "use experimental LaTeX rendering"
        latexStyle       = Flag ""  "latex-style"        (Just $ Free "FILE")      Nothing "provide your own LaTeX style in FILE"
        u                = Flag "U" "use-unicode"        Nothing                   Nothing "use Unicode in HTML output"
        hoogle           = Flag ""  "hoogle"             Nothing                   Nothing "output for Hoogle"
        sourceBase       = Flag ""  "source-base"        (Just $ Free "URL")       Nothing "URL for a source code link on the contents and index pages"
        s                = Flag "s" "source-module"      (Just $ Free "URL")       Nothing "URL for a source code link for each module (using the %{FILE} or %{MODULE} vars)"
        sourceEntity     = Flag ""  "source-entity"      (Just $ Free "URL")       Nothing "URL for a source code link for each entity (using the %{FILE}, %{MODULE}, %{NAME}, %{KIND} or %{LINE} vars)"
        commentsBase     = Flag ""  "comments-base"      (Just $ Free "URL")       Nothing "URL for a comments link on the contents and index pages"
        commentsModule   = Flag ""  "comments-module"    (Just $ Free "URL")       Nothing "URL for a comments link for each module (using the %{MODULE} var)"
        commentsEntity   = Flag ""  "comments-entity"    (Just $ Free "URL")       Nothing "URL for a comments link for each entity (using the %{FILE}, %{MODULE}, %{NAME}, %{KIND} or %{LINE} vars)"
        ccss             = Flag "c" "css"                (Just $ Free "PATH")      Nothing "the CSS file or theme directory to use for HTML output"
        ctheme           = Flag "c" "theme"              (Just $ Free "PATH")      Nothing "the CSS file or theme directory to use for HTML output"
        builtInThemes    = Flag ""  "built-in-themes"    Nothing                   Nothing "include all the built-in haddock themes"
        p                = Flag "p" "proloque"           (Just $ Free "FILE")      Nothing "file containing prologue text"
        t                = Flag "t" "title"              (Just $ Free "TITLE")     Nothing "page heading"
        q                = Flag "q" "qual"               (Just $ Choice $ S.fromList ["none", "full", "local", "relative", "aliased"]) (Just "none") "qualification of names, one of 'none' (default), 'full', 'local' 'relative' or 'aliased'"
        help             = Flag "?" "help"               Nothing                   Nothing "display this help and exit"
        version          = Flag "V" "version"            Nothing                   Nothing "output version information and exit"
        interfaceVersion = Flag ""  "interface-version"  Nothing                   Nothing "output interface file version and exit"
        v                = Flag "v" "verbosity"          (Just $ Free "VERBOSITY") Nothing "set verbosity level"
        useContents      = Flag ""  "use-contents"       (Just $ Free "URL")       Nothing "use a separately-generated HTML contents page"
        genContents      = Flag ""  "gen-contents"       Nothing                   Nothing "generate an HTML contents from specified interfaces"
        useIndex         = Flag ""  "use-index"          (Just $ Free "URL")       Nothing "use a separately-generated HTML index"
        genIndex         = Flag ""  "gen-index"          Nothing                   Nothing "generate an HTML index from specified interfaces"
        ignoreAllExports = Flag ""  "ignore-all-exports" Nothing                   Nothing "behave as if all modules have the ignore-exports atribute"
        hide             = Flag ""  "hide"               (Just $ Free "MODULE")    Nothing "behave as if MODULE has the hide attribute"
        optghc           = Flag ""  "optghc"             (Just $ Free "OPtion")    Nothing "option to be forwarded to GHC"
        ghcVersion       = Flag ""  "ghc-version"        Nothing                   Nothing "output GHC version in numeric format"
        printGhcPath     = Flag ""  "print-ghc-path"     Nothing                   Nothing "output path to GHC binary"
        printGhcLibdir   = Flag ""  "print-ghc-libdir"   Nothing                   Nothing "output GHC lib dir"
        w                = Flag "w" "no-warnings"        Nothing                   Nothing "turn off all warnings"
        noTmpCompDir     = Flag ""  "no-tmp-comp-dir"    Nothing                   Nothing "do not re-direct compilation output to a temporary directory"
        prettyHtml       = Flag ""  "pretty-html"        Nothing                   Nothing "generate html with newlines and indenting (for use with --html)"

---------------------------------------
-- * HsColour
---------------------------------------

-- | Flags are valid for version 1.20
supportedHsColourFlags :: DSet Flag
supportedHsColourFlags = onlyOneConstraint <&&> latexDependent <&&> htmlDependent <&&> (forceEmptyDSet <*|> (singleFlags <*|> (outputType <||> optionalFlags)))
  where singleFlags = uniqueRequired $ S.fromList [help, version, printCss]
        latexDependent = dependOnAll litTex $ S.singleton latex
        htmlDependent = dependAllOnOne (S.fromList [css, icss, anchor, noanchor]) $ S.fromList [html]
        outputType = uniqueRequired $ S.fromList [html, latex]
        optionalFlags = oneof $ S.fromList [o, annot, tty, tty256, mirc, lit, nolit, partial, nopartial]
        cssOnlyOne = uniqueOptional $ S.fromList [css, icss]
        ttyOnlyOne = uniqueOptional $ S.fromList [tty, tty256]
        litOnlyOne = uniqueOptional $ S.fromList [lit, litTex, nolit]
        anchorOnlyOne = uniqueOptional $ S.fromList [anchor, noanchor]
        partialOnlyOne = uniqueOptional $ S.fromList [partial, nopartial]

        onlyOneConstraint = cssOnlyOne <&&> ttyOnlyOne <&&> litOnlyOne <&&> anchorOnlyOne <&&> partialOnlyOne

        --flags
        o         = Flag "o"         "" (Just $ Free "OUTPUT")         Nothing ""
        annot     = Flag "annot"     "" (Just $ Free "ANNOTATIONFILE") Nothing ""
        help      = Flag "help"      "" Nothing                        Nothing ""
        version   = Flag "version"   "" Nothing                        Nothing ""
        printCss  = Flag "print-css" "" Nothing                        Nothing ""
        html      = Flag "html"      "" Nothing                        Nothing ""
        css       = Flag "css"       "" Nothing                        Nothing ""
        icss      = Flag "icss"      "" Nothing                        Nothing ""
        tty       = Flag "tty"       "" Nothing                        Nothing ""
        tty256    = Flag "tty256"    "" Nothing                        Nothing ""
        latex     = Flag "latex"     "" Nothing                        Nothing ""
        mirc      = Flag "mirc"      "" Nothing                        Nothing ""
        lit       = Flag "lit"       "" Nothing                        Nothing ""
        litTex    = Flag "lit-tex"   "" Nothing                        Nothing ""
        nolit     = Flag "nolit"     "" Nothing                        Nothing ""
        anchor    = Flag "anchor"    "" Nothing                        Nothing ""
        noanchor  = Flag "noanchor"  "" Nothing                        Nothing ""
        partial   = Flag "partial"   "" Nothing                        Nothing ""
        nopartial = Flag "nopartial" "" Nothing                        Nothing ""

---------------------------------------
-- * CppHs
---------------------------------------

-- | Flags are valid for version 1.17.1
supportedCppHsFlags :: DSet Flag
supportedCppHsFlags = forceEmptyDSet <*|> (singleFlags <*|> optionalFlags)
  where singleFlags = uniqueRequired $ S.fromList [help, version]
        optionalFlags = oneof $ S.fromList [d, i, o, nomacro, noline, linepragma, pragma, text, strip, stripEol, hashes, layout, unlit, help, version, cpp, include]         

        --flags
        d = Flag "D" "" (Just $ Free "sym|sym=val") Nothing "" 
        i = Flag "I" "" (Just $ Free "path")        Nothing "" 
        o = Flag "O" "" (Just $ Free "file")        Nothing ""
        
        nomacro    = Flag "" "nomacro"    Nothing                         Nothing ""
        noline     = Flag "" "noline"     Nothing                         Nothing ""
        linepragma = Flag "" "linepragma" Nothing                         Nothing ""
        pragma     = Flag "" "pragma"     Nothing                         Nothing ""
        text       = Flag "" "text"       Nothing                         Nothing ""
        strip      = Flag "" "strip"      Nothing                         Nothing ""
        stripEol   = Flag "" "strip-eol"  Nothing                         Nothing ""
        hashes     = Flag "" "hashes"     Nothing                         Nothing ""
        layout     = Flag "" "layout"     Nothing                         Nothing ""
        unlit      = Flag "" "unlit"      Nothing                         Nothing ""
        help       = Flag "" "help"       Nothing                         Nothing ""
        version    = Flag "" "version"    Nothing                         Nothing ""
        cpp        = Flag "" "cpp"        (Just $ Free "std-cpp-options") Nothing ""
        include    = Flag "" "include"    (Just $ Free "filename")        Nothing ""

---------------------------------------
-- * UUAGC
---------------------------------------

-- | Flags are valid for version 0.9.50
supportedUUAGCFlags :: DSet Flag
supportedUUAGCFlags = groupConstraint <&&> onlyOneConstraint <&&> dependencies <&&> (forceEmptyDSet <*|> (singleFlags <*|> (outputType <||> optionalFlags)))
  where singleFlags = uniqueRequired $ S.fromList [h, version]
        
        dataDependent = dependAllOnOne (S.fromList [datarecords, strictdata]) $ S.fromList [d]
        wrapDependent = dependAllOnOne (S.fromList [strictwrap]) $ S.fromList [w]
        semDependent = dependAllOnOne (S.fromList [prefix, s, strictsem, splitsems, sepsemmods]) $ S.fromList [f]
        visitDependent = dependAllOnOne (S.fromList [nopervisitcostcentres, optimize, seqf, bangpats, casef, localcps, dummytokenvisit]) $ S.fromList [visit]
        caseDependent = dependAllOnOne (S.fromList [strictcase, strictercase]) $ S.fromList [casef]
        aspectagDependent = dependAllOnOne (S.fromList [nogroup]) $ S.fromList [aspectag]
        cataDependent = dependAllOnOne (S.fromList [nocatas]) $ S.fromList [c]
        dummyDependent = dependAllOnOne (S.fromList [tupleasdummytoken, stateasdummytoken, strictdummytoken]) $ S.fromList [dummytokenvisit]
        parallelDependent = dependAllOnOne (S.fromList [noeagerblackholing]) $ S.fromList [parallel]
        costDependent = dependAllOnOne (S.fromList [nopervisitcostcentres, noperrulecostcentres]) $ S.fromList [gencostcentres]

        
        outputType = uniqueRequired $ S.fromList [monadic, ocaml, cleanlang]
        optionalFlags = oneof $ S.fromList [newtypes, modcopy, nest, syntaxmacro, o, v, search, cyclef, unbox,
            dumpgrammar, dumpcgrammar, gentraces, genusetraces, genfiledeps, genvisage,
            extends, genattrlist, uniquedispenser, reference, self, wmax, latehigherorderbinding,
            visitcode, kennedywarren, statistics, checkParseRhs, checkParseTys, checkParseBlocks, checkParseHaskell,
            monadicwrapper, noperruletypesigs, noperstatetypesigs, noincludes, quiet]
        
        moduleOnlyOne = uniqueOptional $ S.fromList [m, modulef, modulefp]
        errorOnlyOne = uniqueOptional $ S.fromList [werrors, wignore]
        irrefutableOnlyOne = uniqueOptional $ S.fromList [forceirrefutable, forceirrefutablep]
        breadthfirstOnlyOne = uniqueOptional $ S.fromList [breadthfirst, breadthfirstStrict]
        dummyOnlyOne = uniqueOptional $ S.fromList [tupleasdummytoken, stateasdummytoken]
        optimizeOnlyOne = uniqueOptional $ S.fromList [optimize, nooptimize]
        inlineOnlyOne = uniqueOptional $ S.fromList [noinlinepragmas, helpinlining, aggressiveinlinepragmas]

        allGroup = (uniqueRequired $ S.fromList [a]) <*|> (oneof $ S.fromList [d, c, f, s, p, r, m])
        haskellGroup = (uniqueRequired $ S.fromList [haskellsyntax]) <*|> (oneof $ S.fromList [lckeywords, doublecolons, genlinepragmas])

        dependencies = dataDependent <&&> wrapDependent <&&> semDependent <&&> visitDependent <&&> caseDependent <&&> aspectagDependent
            <&&> cataDependent <&&> dummyDependent <&&> parallelDependent <&&> costDependent
        onlyOneConstraint = moduleOnlyOne <&&> errorOnlyOne <&&> irrefutableOnlyOne <&&> breadthfirstOnlyOne <&&>
            dummyOnlyOne <&&> optimizeOnlyOne <&&> inlineOnlyOne
        groupConstraint = allGroup <&&> haskellGroup

        --flags
        m                       = Flag "m" ""                        Nothing                             Nothing "generate default module header"
        modulef                 = Flag ""  "module"                  Nothing                             Nothing "generate module header"
        modulefp                = Flag ""  "module"                  (Just $ Free "name")                Nothing "generate module header, specify module name"
        d                       = Flag "d" "data"                    Nothing                             Nothing "generate data type definition"
        datarecords             = Flag ""  "datarecords"             Nothing                             Nothing "generate record data types"
        strictdata              = Flag ""  "strictdata"              Nothing                             Nothing "generate strict data fields (when data is generated)"
        strictwrap              = Flag ""  "strictwrap"              Nothing                             Nothing "generate strict wrap fields for WRAPPER generated data"
        c                       = Flag "c" "catas"                   Nothing                             Nothing "generate catamorphisms"
        f                       = Flag "f" "semfuns"                 Nothing                             Nothing "generate semantic functions"
        s                       = Flag "s" "signatures"              Nothing                             Nothing "generate signatures for semantic functions"
        newtypes                = Flag ""  "newtypes"                Nothing                             Nothing "use newtypes instead of type synonyms"
        p                       = Flag "p" "pretty"                  Nothing                             Nothing "generate pretty printed list of attributes"
        w                       = Flag "w" "wrappers"                Nothing                             Nothing "generate wappers for semantic domains"
        r                       = Flag "r" "rename"                  Nothing                             Nothing "rename data constructors"
        modcopy                 = Flag ""  "modcopy"                 Nothing                             Nothing "use modified copy rule"
        nest                    = Flag ""  "nest"                    Nothing                             Nothing "use nested tuples"
        syntaxmacro             = Flag ""  "syntaxmacro"             Nothing                             Nothing "experimental: generate syntax macro code (using knit catas)"
        o                       = Flag "o" "output"                  (Just $ Free  "file")               Nothing "specify output file"
        v                       = Flag "v" "verbose"                 Nothing                             Nothing "verbose error message format"
        h                       = Flag "h" "help"                    Nothing                             Nothing "get (this) usage information"
        a                       = Flag "a" "all"                     Nothing                             Nothing "do everything (-dcfsprm)"
        search                  = Flag "P" "search"                  (Just $ Free "path")                Nothing "specify search path"
        prefix                  = Flag ""  "prefix"                  (Just $ Free "prefix")              Nothing "set prefix for semantic functions"
        self                    = Flag ""  "self"                    Nothing                             Nothing "generate self attribute"
        cyclef                  = Flag ""  "cycle"                   Nothing                             Nothing "check for cyclic definitions"
        version                 = Flag ""  "version"                 Nothing                             Nothing "get version information"
        optimize                = Flag "O" "optimize"                Nothing                             Nothing "optimize generated code (--visit --case)"
        visit                   = Flag ""  "visit"                   Nothing                             Nothing "try generating visit functions"
        seqf                    = Flag ""  "seq"                     Nothing                             Nothing "force evaluation using function seq (visit functions only)"
        unbox                   = Flag ""  "unbox"                   Nothing                             Nothing "use unboxed tuples"
        bangpats                = Flag ""  "bangpats"                Nothing                             Nothing "use bang patterns (visit functions only)"
        casef                   = Flag ""  "case"                    Nothing                             Nothing "Use nested cases instead of let (visit functions only)"
        strictcase              = Flag ""  "strictcase"              Nothing                             Nothing "Force evaluation of the scrutinee of cases (in generated code, visit functions only)"
        strictercase            = Flag ""  "strictercase"            Nothing                             Nothing "Force evaluation of all variables bound by a case statement (in generated code)"
        strictsem               = Flag ""  "strictsem"               Nothing                             Nothing "Force evaluation of sem-function arguments (in generated code)"
        localcps                = Flag ""  "localcps"                Nothing                             Nothing "Apply a local CPS transformation (in generated code, visit functions only)"
        splitsems               = Flag ""  "splitsems"               Nothing                             Nothing "Split semantic functions into smaller pieces"
        werrors                 = Flag ""  "Werrors"                 Nothing                             Nothing "Turn warnings into fatal errors"
        wignore                 = Flag ""  "Wignore"                 Nothing                             Nothing "Ignore warnings"
        wmax                    = Flag ""  "Wmax"                    (Just $ Free "<max errs reported>") Nothing "Sets the maximum number of errors that are reported"
        dumpgrammar             = Flag ""  "dumpgrammar"             Nothing                             Nothing "Dump internal grammar representation (in generated code)"
        dumpcgrammar            = Flag ""  "dumpcgrammar"            Nothing                             Nothing "Dump internal cgrammar representation (in generated code)"
        gentraces               = Flag ""  "gentraces"               Nothing                             Nothing "Generate trace expressions (in generated code)"
        genusetraces            = Flag ""  "genusetraces"            Nothing                             Nothing "Generate trace expressions at attribute use sites (in generated code)"
        gencostcentres          = Flag ""  "gencostcentres"          Nothing                             Nothing "Generate cost centre pragmas (in generated code)"
        genlinepragmas          = Flag ""  "genlinepragmas"          Nothing                             Nothing "Generate GHC LINE pragmas (in generated code)"
        sepsemmods              = Flag ""  "sepsemmods"              Nothing                             Nothing "Generate separate modules for semantic functions (in generated code)"
        genfiledeps             = Flag "M" "genfiledeps"             Nothing                             Nothing "Generate a list of dependencies on the input AG files"
        genvisage               = Flag ""  "genvisage"               Nothing                             Nothing "Generate output for the AG visualizer Visage"
        aspectag                = Flag ""  "aspectag"                Nothing                             Nothing "Generate AspectAG file"
        nogroup                 = Flag ""  "nogroup"                 (Just $ Free "attributes")          Nothing "specify the attributes that won't be grouped in AspectAG"
        extends                 = Flag ""  "extends"                 (Just $ Free "module")              Nothing "specify a module to be extended"
        genattrlist             = Flag ""  "genattrlist"             Nothing                             Nothing "Generate a list of all explicitly defined attributes (outside irrefutable patterns)"
        forceirrefutable        = Flag ""  "forceirrefutable"        (Just $ Free "file")                Nothing "Force a set of explicitly defined attributes to be irrefutable"
        forceirrefutablep       = Flag ""  "forceirrefutable"        Nothing                             Nothing "Force a set of explicitly defined attributes to be irrefutable, specify file containing the attribute set"
        uniquedispenser         = Flag ""  "uniquedispenser"         (Just $ Free "name")                Nothing "The Haskell function to call in the generated code"
        lckeywords              = Flag ""  "lckeywords"              Nothing                             Nothing "Use lowercase keywords (sem, attr) instead of the uppercase ones (SEM, ATTR)"
        doublecolons            = Flag ""  "doublecolons"            Nothing                             Nothing "Use double colons for type signatures instead of single colons"
        haskellsyntax           = Flag "H" "haskellsyntax"           Nothing                             Nothing "Use Haskell like syntax (equivalent to --lckeywords and --doublecolons --genlinepragmas)"
        reference               = Flag ""  "reference"               Nothing                             Nothing "Use reference attributes"
        monadic                 = Flag ""  "monadic"                 Nothing                             Nothing "Experimental: generate monadic code"
        ocaml                   = Flag ""  "ocaml"                   Nothing                             Nothing "Generate Ocaml code"
        cleanlang               = Flag ""  "cleanlang"               Nothing                             Nothing "Generate Clean code"
        breadthfirst            = Flag ""  "breadthfirst"            Nothing                             Nothing "Experimental: generate breadth-first code"
        breadthfirstStrict      = Flag ""  "breadthfirst-strict"     Nothing                             Nothing "Experimental: outermost breadth-first evaluator is strict instead of lazy"
        visitcode               = Flag ""  "visitcode"               Nothing                             Nothing "Experimental: generate visitors code"
        kennedywarren           = Flag ""  "kennedywarren"           Nothing                             Nothing "Use Kennedy-Warren's algorithm for ordering"
        statistics              = Flag ""  "statistics"              (Just $ Free "FILE to append to")   Nothing "Append statistics to FILE"
        checkParseRhs           = Flag ""  "checkParseRhs"           Nothing                             Nothing "Parse RHS of rules with Haskell parser"
        checkParseTys           = Flag ""  "checkParseTys"           Nothing                             Nothing "Parse types of attrs with Haskell parser"
        checkParseBlocks        = Flag ""  "checkParseBlocks"        Nothing                             Nothing "Parse blocks with Haskell parser"
        checkParseHaskell       = Flag ""  "checkParseHaskell"       Nothing                             Nothing "Parse Haskell code (recognizer)"
        nocatas                 = Flag ""  "nocatas"                 (Just $ Free "list of nonterms")    Nothing "Nonterminals not to generate catas for"
        nooptimize              = Flag ""  "nooptimize"              Nothing                             Nothing "Disable optimizations"
        parallel                = Flag ""  "parallel"                Nothing                             Nothing "Generate a parallel evaluator (if possible)"
        monadicwrapper          = Flag ""  "monadicwrapper"          Nothing                             Nothing "Generate monadic wrappers"
        helpinlining            = Flag ""  "helpinlining"            Nothing                             Nothing "Generate inline directives for GHC"
        dummytokenvisit         = Flag ""  "dummytokenvisit"         Nothing                             Nothing "Add an additional dummy parameter to visit functions"
        tupleasdummytoken       = Flag ""  "tupleasdummytoken"       Nothing                             Nothing "Use conventional tuples as dummy parameter instead of a RealWorld state token"
        stateasdummytoken       = Flag ""  "stateasdummytoken"       Nothing                             Nothing "Use RealWorld state token as dummy parameter instead of conventional tuples (default)"
        strictdummytoken        = Flag ""  "strictdummytoken"        Nothing                             Nothing "Strictify the dummy token that makes states and rules functions"
        noperruletypesigs       = Flag ""  "noperruletypesigs"       Nothing                             Nothing "Do not generate type sigs for attrs passed to rules"
        noperstatetypesigs      = Flag ""  "noperstatetypesigs"      Nothing                             Nothing "Do not generate type sigs for attrs saved in node states"
        noeagerblackholing      = Flag ""  "noeagerblackholing"      Nothing                             Nothing "Do not automatically add the eager blackholing feature for parallel programs"
        noperrulecostcentres    = Flag ""  "noperrulecostcentres"    Nothing                             Nothing "Do not generate cost centres for rules"
        nopervisitcostcentres   = Flag ""  "nopervisitcostcentres"   Nothing                             Nothing "Do not generate cost centres for visits"
        noinlinepragmas         = Flag ""  "noinlinepragmas"         Nothing                             Nothing "Definitely not generate inline directives"
        aggressiveinlinepragmas = Flag ""  "aggressiveinlinepragmas" Nothing                             Nothing "Generate more aggressive inline directives"
        latehigherorderbinding  = Flag ""  "latehigherorderbinding"  Nothing                             Nothing "Generate an attribute and wrapper for late binding of higher-order attributes"
        noincludes              = Flag ""  "noincludes"              Nothing                             Nothing "Ignore include directives in .ag files"
        quiet                   = Flag ""  "quiet"                   Nothing                             Nothing "Dont print some compilation information"

---------------------------------------
-- * GHC
---------------------------------------

-- I am not even going to try to list all the available flags and their dependecies for GHC
supportedGHCFlags :: DSet Flag
supportedGHCFlags = emptyDSet

---------------------------------------
-- * UHC
---------------------------------------

-- | Flags are valid for version 1.1.5
supportedUHCFlags :: DSet Flag
supportedUHCFlags = pkgDependent <&&> hideOnlyOne <&&> (forceEmptyDSet <*|> (singleFlags <*|> optionalFlags))
  where singleFlags = uniqueRequired $ S.fromList [h, version, versionDotted, versionAsnumber, numericVersion,
            metaVariant, metaTargetDefault, metaTargets, metaOptimizations, metaPkgdirSystem, metaPkgdirUser]
  
        optionalFlags = oneof $ S.fromList [v, t, targetFlavor, p, optimise, noRecomp, noPrelude, noHiCheck, c, i, libSearchPath, cpp,
            limitTysynExpand, odir, o, keepIntermediateFiles, package, hideAllPackages, pkgBuild, pkgExpose, pkgHide, pkgHideAll,
            pkgSearchpath, cfgInstallRoot, cfgInstallVariant, optP, pgmP, coreopt]

        hideOnlyOne = uniqueOptional $ S.fromList [hideAllPackages, pkgHide, pkgHideAll]
        pkgDependent = dependAllOnOne (S.fromList [package, pkgExpose]) $ S.fromList [pkgHideAll, hideAllPackages]


        --flags
        h                     = Flag "h" "help"                    Nothing                     Nothing "print this help (then stop)"
        version               = Flag ""  "version"                 Nothing                     Nothing "print version info (then stop)"
        versionDotted         = Flag ""  "version-dotted"          Nothing                     Nothing "print version in \"x.y.z\" style (then stop)"
        versionAsnumber       = Flag ""  "version-asnumber"        Nothing                     Nothing "print version in \"xyz\" style (then stop)"
        numericVersion        = Flag ""  "numeric-version"         Nothing                     Nothing "see --version-dotted (to become obsolete)"
        v                     = Flag "v" "verbose"                 (Just $ Choice $ S.fromList ["0", "1", "2", "3", "4"])             (Just "1")     "be verbose, 0=quiet, 4=debug, default=1"
        t                     = Flag "t" "target"                  (Just $ Choice $ S.fromList ["bc", "js"])                          (Just "bc")    "generate code for target, default=bc"
        targetFlavor          = Flag ""  "target-flavor"           (Just $ Choice $ S.fromList ["debug", "plain"])                    (Just "plain") "generate code for target flavor, default=plain"
        p                     = Flag "p" "pretty"                  (Just $ Choice $ S.fromList ["hs", "eh", "ast", "-"])              (Just "eh")    "show pretty printed source or EH abstract syntax tree, default=eh, -=off, (downstream only)"
        optimise              = Flag "O" "optimise"                (Just $ Choice $ S.fromList ["0", "1", "2", "3", "true", "false"]) (Just "1")     "optimise with level or specific by name, default=1"
        noRecomp              = Flag ""  "no-recomp"               Nothing                     Nothing "turn off recompilation check (force recompile)"
        noPrelude             = Flag ""  "no-prelude"              Nothing                     Nothing "do not assume presence of Prelude"
        noHiCheck             = Flag ""  "no-hi-check"             Nothing                     Nothing "no check on .hi files not matching the compiler version"
        c                     = Flag "c" "compile-only"            Nothing                     Nothing "compile only, do not link"
        i                     = Flag "i" "import-path"             (Just $ Free "path")        Nothing "search path for user files, separators=';', appended to previous"
        libSearchPath         = Flag "L" "lib-search-path"         (Just $ Free "path")        Nothing "search path for library files, see also --import-path"
        cpp                   = Flag ""  "cpp"                     Nothing                     Nothing "preprocess source with CPP"
        limitTysynExpand      = Flag ""  "limit-tysyn-expand"      (Just $ Free "<nr>")        Nothing "type synonym expansion limit"
        odir                  = Flag ""  "odir"                    (Just $ Free "dir")         Nothing "base directory for generated files. Implies --compile-only"
        o                     = Flag "o" "output"                  (Just $ Free "file")        Nothing "file to generate executable to"
        keepIntermediateFiles = Flag ""  "keep-intermediate-files" Nothing                     Nothing "keep intermediate files (default=off)"
        metaVariant           = Flag ""  "meta-variant"            Nothing                     Nothing "meta: print variant (then stop)"
        metaTargetDefault     = Flag ""  "meta-target-default"     Nothing                     Nothing "meta: print the default codegeneration target (then stop)"
        metaTargets           = Flag ""  "meta-targets"            Nothing                     Nothing "meta: print supported codegeneration targets (then stop)"
        metaOptimizations     = Flag ""  "meta-optimizations"      Nothing                     Nothing "meta: print optimization names (then stop)"
        metaPkgdirSystem      = Flag ""  "meta-pkgdir-system"      Nothing                     Nothing "meta: print system package dir (then stop)"
        metaPkgdirUser        = Flag ""  "meta-pkgdir-user"        Nothing                     Nothing "meta: print user package dir (then stop)"
        package               = Flag ""  "package"                 (Just $ Free "package")     Nothing "see --pkg-expose"
        hideAllPackages       = Flag ""  "hide-all-packages"       Nothing                     Nothing "see --pkg-hide-all"
        pkgBuild              = Flag ""  "pkg-build"               (Just $ Free "package")     Nothing "pkg: build package from files. Implies --compile-only"
        pkgExpose             = Flag ""  "pkg-expose"              (Just $ Free "package")     Nothing "pkg: expose/use package"
        pkgHide               = Flag ""  "pkg-hide"                (Just $ Free "package")     Nothing "pkg: hide package"
        pkgHideAll            = Flag ""  "pkg-hide-all"            Nothing                     Nothing "pkg: hide all (implicitly) assumed/used packages"
        pkgSearchpath         = Flag ""  "pkg-searchpath"          (Just $ Free "path")        Nothing "pkg: package search directories, each dir has <pkg>/<variant>/<target>/<flavor>"
        cfgInstallRoot        = Flag ""  "cfg-install-root"        (Just $ Free "dir")         Nothing "cfg: installation root (to be used only by wrapper script)"
        cfgInstallVariant     = Flag ""  "cfg-install-variant"     (Just $ Free "variant")     Nothing "cfg: installation variant (to be used only by wrapper script)"
        optP                  = Flag ""  "optP"                    (Just $ Free "opt for cmd") Nothing "opt: option for cmd used by compiler, currently only P (preprocessing)"
        pgmP                  = Flag ""  "pgmP "                   (Just $ Free "alternate program for cmd") Nothing "pgm: alternate executable used by compiler, currently only P (preprocessing)"
        coreopt               = Flag ""  "coreopt"                 (Just $ Free "opt[,...]")   Nothing "core opts: pp-parseable"

---------------------------------------
-- * Instances
---------------------------------------

---------------------------------------
-- ** Tool Instances
---------------------------------------

instance Tool Haddock where
  getName _ = "haddock"
  supportedFlags _ _ = return supportedHaddockFlags
  requiredArguments _ _ = return ["FILE"]

instance Tool HsColour where
  getName _ = "HsColour"
  supportedFlags _ _ = return supportedHsColourFlags
  requiredArguments _ _ = return ["FILE"]
  flagToString _ f = return $ case f ^. shortName of
    "o" -> ["-o" ++ (fromMaybe "" $ f ^. defaultArgument)]
    "annot" -> ["-annot=" ++ (fromMaybe "" $ f ^. defaultArgument)]
    _ -> flagToProgArg f

instance Tool CppHs where
  getName _ = "cpphs"
  supportedFlags _ _ = return supportedCppHsFlags
  requiredArguments _ _ = return ["FILE"]

instance Tool UUAGC where
  getName _ = "uuagc"
  supportedFlags _ _ = return supportedUUAGCFlags
  requiredArguments _ _ = return ["FILE"]

instance Tool GHC where
  getName _ = "ghc"
  supportedFlags _ _ = return supportedGHCFlags
  requiredArguments _ _ = return ["FILE"]
  flagToString _ f = return $ case f ^. shortName of
    "i" -> ["-i" ++ (fromMaybe "" $ f ^. defaultArgument)]
    _ -> flagToProgArg f

instance Tool UHC where
  getName _ = "uhc"
  supportedFlags _ _ = return supportedUHCFlags
  requiredArguments _ _ = return ["FILE"]

---------------------------------------
-- ** OutputTool Instances
---------------------------------------

instance OutputTool Haddock where
  outputDirFlag _ = return . newFlagWithArgument "odir"

instance OutputTool HsColour where
  outputFileFlag _ = return . newShortFlagWithArgument "o"

instance OutputTool CppHs where
  inputDirFlag _ = return . newShortFlagWithArgument "I"
  outputFileFlag _ = return . newShortFlagWithArgument "O"

instance OutputTool UUAGC where
  inputDirFlag _ = return . newShortFlagWithArgument "P"
  outputFileFlag _ = return . newShortFlagWithArgument "o"

instance OutputTool GHC where
  inputDirFlag _ = return . newShortFlagWithArgument "i"
  outputFileFlag _ = return . newShortFlagWithArgument "o"
  outputDirFlag _ = return . newShortFlagWithArgument "outputdir"

instance OutputTool UHC where
  inputDirFlag _ = return . newShortFlagWithArgument "i"
  outputFileFlag _ = return . newShortFlagWithArgument "o"
  outputDirFlag _ = return . newFlagWithArgument "odir"

---------------------------------------
-- ** PreProcessor Instances
---------------------------------------

instance PreProcessor CppHs where

instance PreProcessor UUAGC where
 
instance PreProcessor GHC where
  supportedTargets _ _ = return [exeFile, libFile, profExeFile, profLibFile]
  sourceTypes _ _ _ = return [haskellType, litHaskellType, objectType]
  directDependencies _ cp target targetType = case targetType of
    "exe" -> return ["{sources}.o"]
    "lib" -> return ["{sources}.o", "{sources}.hi"]
    "o" -> return ["{x}.hs"]
    "o-prof" -> return ["{x}.hs"]
    x -> throwError $ Exception $ "Can not produce file of type " ++ x
  indirectDependencies _ cp target targetType dDeps =  case targetType of
    "exe" -> return []
    "lib" -> return []
    "o" -> do 
      x <- mapM getImportList dDeps
      return $ concat x
    "o-prof" -> do 
      x <- mapM getImportList dDeps
      return $ concat x
    x -> throwError $ Exception $ "Can not produce file of type " ++ x
  transFormStep _ cp target targetType targetFile allDeps = undefined

instance PreProcessor UHC where 

---------------------------------------
-- ** PackageManager Instances
---------------------------------------

instance PackageManager GHC where
  packageFlag _ = return . newShortFlagWithArgument "package"
  hideAllPackagesFlag _ = return $ newShortFlag "hide-all-packages"

instance PackageManager UHC where
  packageFlag _ = return . newFlagWithArgument "pkg-expose"
  hideAllPackagesFlag _ = return $ newFlag "pkg-hide-all"


---------------------------------------
-- ** Compiler Instances
---------------------------------------

--instance Compiler GHC where
--  supportedBackends _ = M.fromList [("c", Right "")]
--  supportedFlavours _ = M.fromList [("normal", Right ""), ("prof", Left $ newShortFlag "prof" & description .~ "profiling")]
--  --supportedExtensions _ = ["hs", "lhs"]
--  --defaultExtensions = 

--instance Compiler UHC where
--  supportedBackends _ = M.fromList [("bc", Left $ newShortFlagWithArgument "t" "bc" & description .~ "compile to c"), ("js", Left $ newShortFlagWithArgument "t" "js" & description .~ "javascript")]
--  supportedFlavours _ = M.fromList [("plain", Left $ newFlagWithArgument "target-flavor" "plain" & description .~ "plain"), ("debug", Left $ newFlagWithArgument "target-flavor" "debug" & description .~ "debug")]
--  --supportedExtensions _ = ["hs", "eh"]
--  --defaultExtensions = 