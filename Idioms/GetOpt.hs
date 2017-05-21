-- GetOpt
-- ref: https://wiki.haskell.org/GetOpt
-- module: http://www.math.columbia.edu/~bayer/Haskell/Annote/GetOpt.html
-- ref: https://wiki.haskell.org/High-level_option_handling_with_GetOpt
-- other-parsers: https://wiki.haskell.org/Command_line_option_parsers

-- GetOpt

-- 1. Ginormous record
data Settings = Settings { filter     :: Maybe String
                        , dateFormat :: Maybe String
                        , blahBlah   :: Maybe Blah
                        ...
                        , thisIsGetting :: RatherLargeIsntIt
                        }

emptySettings :: Settings
emptySettings = Settings { filter = Nothing
                        , dateFormat = Nothing
                        }

toSettings :: [Flag] -> Settings
toSettings fs = toSettingsH fs emptySettings

toSettingsH :: [Flag] -> Settings -> Settings
toSettingsH (Filter s:fs)     i = toSettingsH fs (i { filter = s })
toSettingsH (DateFormat s:fs) i = toSettingsH fs (i { dateFormat = i= })
-- ...

{-
    Advantages:

    simple, easy to look up settings

    Disadvantages:

    boring; have to write
    Flag type
    Settings record type
    default Settings
    processFlag entry
    GetOpt entry
    record gets really really huge if you have a lot of flags
-}

-- 2. List of flags (darcs)
fromDateFormat :: Flag -> Maybe String
fromDateFormat (DateFormat x) = Just x
fromDateFormat _ = Nothing
 
hasDateFormat fs = any (isJust.fromDateFormat) fs
getDateFormat fs = listToMaybe $ mapMaybe fromDateFormat fs


{-
    Advantages:

    simple
    very convenient to add flags (as a minimum, you have to write
    flag type
    GetOpt entry
    lookup code (but pay-as-you-go)
    can support using a flag multiple times (-v -v -v)
    
    Disadvantages:

    still a bit boilerplatey
-}

-- 3. No lists, just records (lhs2TeX)

type Flag a = (a -> Settings -> Settings)
 
options :: [OptDescr Flag]
options =
  [ Option "f" ["filter"]
      (ReqArg (\x s -> s { filter = Just x }) "TYPE")
      "blahblah"
  , Option "d" ["date-format"]
      (ReqArg (\x s -> s { dateFormat = Just x }) "TYPE")
      "blahblah"
 
  ]

{-
    Advantages:

    very convenient/compact; have to write
    Flag type
    Settings record type/GetOpt in one go
    default Settings
    easy to lookup flags

    Disadvantages:

    Not as flexible
    can't group flags into blocks and have different programs that use different subsets of flags (without sharing the same Setting type)
    everything must go into Settings
    seems harder to say stuff like 'if flag X is set and flag Y are in the list of Flags, then parameterise flag Z this way' or 'flags X and Y are mutually exclusive'

-}

-- 4. List of flags + existential types(GenI)

{-# OPTIONS_GHC -fglasgow-exts #-}
import Data.List (find)
import Data.Typeable (Typeable, cast, typeOf)
data Flag = 
    forall f x . (Show f, Show x, Typeable f, Typeable x) => Flag (x -> f) x 
    deriving Typeable

instance Show Flag where
  show (Flag f x) = "Flag " ++ show (f x)

isFlag     :: (Typeable f, Typeable x) => (x -> f) -> Flag -> Bool
hasFlag    :: (Typeable f, Typeable x) => (x -> f) -> [Flag] -> Bool
deleteFlag :: (Typeable f, Typeable x) => (x -> f) -> [Flag] -> [Flag]
setFlag    :: (Show f, Show x, Typeable f, Typeable x) => (x -> f) -> x -> [Flag] -> [Flag]
getFlag    :: (Show f, Show x, Typeable f, Typeable x) => (x -> f) -> [Flag] -> Maybe x

isFlag f1 (Flag f2 _) = typeOf f1 == typeOf f2
hasFlag f      = any (isFlag f)
deleteFlag f   = filter (not.isFlag f)
setFlag f v fs = (Flag f v) : tl where tl = deleteFlag f fs
getFlag f fs   = find (isFlag f) fs >>= cast


data LogFileFlag = LogFileFlag String deriving (Eq, Show, Typeable)
data TimeoutFlag = TimeoutFlag Int    deriving (Eq, Show, Typeable)
 
lf = Flag LogFileFlag "hi"
tf = Flag TimeoutFlag 3

-- hasFlag LogFileFlag [ lf, tf ]
-- setFlag LogFileFlag "bar" [ lf, tf ]

{-
    Advantages:

    no more boilerplate only have to define
    flag type, although ugly
    getopt stuff
    extensible (as any list of flags approach)
    mix-n-matchable (cf #3; different programs can share subset of flags)
    can really just say 'getFlag FooFlag'
    setFlag / deleteFlag
    (I'm not claiming there are more advantages; it's just that I wrote this and can remember why)

    Disadavantages:

    can't enforce that some flags are always set (cf #1 and #4)
    making things too complicated! Existential types seems like overkill for GetOpt (well, I mostly did this to learn what they were)
    ugly cpp macro or repetitive
        data FilterFlag = FilterFlag String deriving (Eq, Show, Typeable)
        data TimeoutFlag = TimeoutFlag Int deriving (Eq, Show, Typeable)
    ugly GetOpt wrappers
        reqArg :: forall f x . (Eq f, Show f, Typeable f, Eq x, Show x, Typeable x)
        => (x -> f)      -- ^ flag
        -> (String -> x) -- ^ string reader for flag (probably |id| if already a String)
        -> String        -- ^ description
        -> ArgDescr Flag
        reqArg s fn desc = ReqArg (\x -> Flag s (fn x)) desc
-}

-- 5. Association Lists
-- One can simplify the interface provided by System.Console.GetOpt, by treating options as association lists. One works with pairs of the form
-- (a, String)
-- a : an enumeration type of option keys
-- Then passes a list of the type [OptDescr (a, String)] to getOpt

data Flag
    = Filter
    | CodeDir
    | DateFormat
    | Help
    deriving (Eq)
 
defaults :: OptionList Flag
defaults =
    [ (Filter,     "Markdown.pl")
    , (DateFormat, "%B %e, %Y %H:%M:%S")
    ]
 
flags :: OptionSpecs Flag
flags = makeOptions
    [ (Filter,     'm', "markup", reqArg, "path",   "Path to Markdown.pl")
    , (CodeDir,    'c', "code",   reqArg, "path",   "Path to code directory")
    , (DateFormat, 'd', "date",   reqArg, "format", "Date format")
    , (Help,       'h', "help",   noArg,  [],       "This help message")
    ]

getOptionOr :: Eq a => a -> OptionList a -> String -> String
getOptionOr options assoc def = case lookup options assoc of
    Nothing -> def
    Just "" -> def
    Just s  -> s

-- getOptionOr CodeDir options "src"


-- High-level option handling with GetOpt

data Flag = Verbose               -- this option has no arguments
        | Version               -- no arguments
        | Input (Maybe String)  -- optional argument
        | Output String         -- mandatory argument
        | LibDir String         -- mandatory argument

data Flag = Verbose               -- this option has no arguments
        | Version               -- no arguments
        | Input (Maybe String)  -- optional argument
        | Output String         -- mandatory argument
        | LibDir String         -- mandatory argument

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"]
        (ReqArg
            (\arg opt -> return opt { optInput = readFile arg })
            "FILE")
        "Input file"
 
    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optOutput = writeFile arg })
            "FILE")
        "Output file"
 
    , Option "s" ["string"]
        (ReqArg
            (\arg opt -> return opt { optInput = return arg })
            "FILE")
        "Input string"
 
    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Enable verbose messages"
 
    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.01"
                exitWith ExitSuccess))
        "Print version"
 
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
    	        prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]

main = do
    args <- getArgs
 
    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
 
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions
 
    let Options { optVerbose = verbose
                , optInput = input
                , optOutput = output   } = opts
 
    when verbose (hPutStrLn stderr "Hello!")
 
    input >>= output