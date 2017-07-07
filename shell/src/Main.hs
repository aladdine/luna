{-# LANGUAGE OverloadedStrings #-}

module Main where

import Luna.Prelude hiding (Level, switch, argument)
import qualified Luna.Shell
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map  as Map
import qualified Data.Text as Text
import qualified System.Environment as System

import Control.Lens.Aeson
import System.Console.Options hiding (main)
import qualified System.Console.Options as O
import System.Exit (exitSuccess, exitFailure)
import Text.Parsert hiding (option)
import qualified Data.Layout        as Doc
import           Data.Layout        ((<+>), (</>), (<//>))

import qualified Data.Aeson         as Aeson
import           Data.Aeson         (ToJSON, toJSON, FromJSON, fromJSON)
import qualified Data.Aeson.Diff    as Aeson
import qualified Data.Aeson.Pointer as Aeson
import Control.Monad.Branch

import qualified Text.Parsert as Parsert
import Text.Parsert

-- class Pretty a where
--     showPretty :: a -> Text
--     readPretty :: Text -> Either String a



--
--
class CmdParser a where
    parseCmd :: Parser a
--
--
--
--
-- phantom :: Mod FlagFields () -> Parser ()
-- phantom = flag () ()
--
-- phantom' :: Parser ()
-- phantom' = phantom mempty




-- === Verbosity === --

-- data Verbosity = Verbosity { _scope :: Text
--                            , _level :: Level
--                            } deriving (Show)
-- data Level = Debug
--            | Info
--            | Error
--            | Panic
--            deriving (Show)
--
-- makeLenses ''Verbosity


-- ------------------------
-- -- === PassStatus === --
-- ------------------------
--
-- data PassStatus = Enable | Disable deriving (Show, Read)
--
-- passMapParser :: Parser (Map Text PassStatus)
-- passMapParser = foldr (uncurry Map.insert) mempty <$> many passPassStatusParser
--
-- passPassStatusParser :: Parser (Text, PassStatus)
-- passPassStatusParser = flip (,)
--                <$> option (eitherReader $ readPretty . convert) (hidden <> long "pass" <> metavar "switch name" <> help "Switch passes execution.")
--                <*> strArgument (internal <> metavar "name")
--
-- instance Pretty PassStatus where
--     showPretty   = Text.toLower . convert . show
--     readPretty s = mapLeft (const . convert $ "Unexpected pass command '" <> s <> "'. Expecting 'enable' or 'disable'.") . tryReads $ Text.toTitle s


------------------------
-- === ConfigTree === --
------------------------

type ConfigTree = Map Text Text

configTreeParser :: Parser ConfigTree
configTreeParser = foldr (uncurry Map.insert) mempty <$> multiple optSetParser


fullOptSetParser :: Parser (Text, Text)
fullOptSetParser = sub $ (,)
    <$> strOption "--" "set" (help $ "Set configuration options." </> "Use `luna help generic-config` to learn more.")
    <*> arg (convert <$> lexeme anyWord) id

shortOptSwitchParser :: Char -> Text -> (ArgConfig -> ArgConfig) -> Parser (Text, Text)
shortOptSwitchParser pfx val cfg = sub $ ((,val) . (<> ".enabled"))
    <$  arg (token pfx <* notFollowedBy (token pfx)) (cfg . tag "option" . label (convert pfx <> " " <> "opt"))
    <*> arg (convert <$> lexeme anyWord) id

shortOptDisableParser, shortOptEnableParser :: Parser (Text, Text)
shortOptDisableParser = shortOptSwitchParser '-' "false" (help "Shortcut for --set opt.enabled false")
shortOptEnableParser  = shortOptSwitchParser '+' "true"  (help "Shortcut for --set opt.enabled true")

optSetParser :: Parser (Text, Text)
optSetParser = fullOptSetParser <|> shortOptDisableParser <|> shortOptEnableParser

-- luna build --set pass.analysis.simpleaa.enabled   true
-- luna build +pass.analysis.simpleaa

-- luna build --set pass.analysis.simpleaa.verbosity debug
-- luna build --verbosity pass.analysis.simpleaa debug
-- luna build --verbosity pass.analysis.* debug

-- luna build --set pass.analysis.simpleaa.verbosity enabled
-- luna build +pass.analysis.simpleaa.verbosity
-- luna build +*.verbosity
-- luna build +verbosity   # global option for the most important things

-- luna build --set report.unused.variable warning


-- stats   = +pass.**.stats
-- verbose = +pass.**.verbose



-- === Build === --

data ReportLevel = Silent
                 | Warning
                 | Error
                 deriving (Show)

data Optimization = None
                  | Normal
                  | Full
                  | Selected [Text]
                  deriving (Show)

-- data PassCfg = PassCfg { _verbosity :: Verbosity
--                        , _timeStats :: Bool
--                        , _memStats  :: Bool
--                     --    ,
--                        }

data BuildCfg = BuildCfg
    { --_optimization :: Optimization
    -- , _pragmas      :: Map Text Pragma
    -- , _pretend      :: Bool
     _run          :: Bool
    , _pass         :: ConfigTree
    -- , _report       :: ConfigTree
    } deriving (Generic, Show)

-- stats
instance ToJSON    BuildCfg where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance FromJSON  BuildCfg where parseJSON  = lensJSONParse
instance CmdParser BuildCfg where
    parseCmd = addHelp' $ (\cfg sets -> cfg) <$> ((\x -> BuildCfg x mempty)
           <$> flag "run" (help "Run the output program after successful compilation.")
           ) <#> multiple optSetParser

-- configTreeParser

-- === Clean === --

data CleanOpts = CleanOpts deriving (Generic, Show)

instance ToJSON    CleanOpts
instance FromJSON  CleanOpts
instance CmdParser CleanOpts where
    parseCmd = pure CleanOpts


-- === Doc === --

data DocOpts = DocOpts deriving (Generic, Show)

instance ToJSON    DocOpts
instance FromJSON  DocOpts
instance CmdParser DocOpts where
    parseCmd = pure DocOpts


-- === Env === --

data EnvOpts = EnvOpts deriving (Generic, Show)

instance ToJSON    EnvOpts
instance FromJSON  EnvOpts
instance CmdParser EnvOpts where
    parseCmd = pure EnvOpts


-- === Install === --

data InstallOpts = InstallOpts deriving (Generic, Show)

instance ToJSON    InstallOpts
instance FromJSON  InstallOpts
instance CmdParser InstallOpts where
    parseCmd = pure InstallOpts


-- === New === --

data NewOpts = NewOpts deriving (Generic, Show)

instance ToJSON    NewOpts
instance FromJSON  NewOpts
instance CmdParser NewOpts where
    parseCmd = pure NewOpts


-- === Package === --

data PackageOpts = PackageOpts deriving (Generic, Show)

instance ToJSON    PackageOpts
instance FromJSON  PackageOpts
instance CmdParser PackageOpts where
    parseCmd = pure PackageOpts


-- === Help === --

data HelpOpts = HelpAboutConfig deriving (Generic, Show)

instance ToJSON    HelpOpts
instance FromJSON  HelpOpts
-- instance CmdParser HelpOpts where
--     parseCmd = subparser (mconcat [ commandGroup "Help topics:", metavar "topic"
            --   , command "configuration" . info (pure HelpAboutConfig)   $ progDesc "Luna toolkit configuration management."
            --   ])


-- === Run === --

data RunOpts = RunOpts deriving (Generic, Show)

instance ToJSON    RunOpts
instance FromJSON  RunOpts
instance CmdParser RunOpts where
    parseCmd = pure RunOpts


-- === Version === --

data VersionOpts = VersionOpts deriving (Generic, Show)

instance ToJSON    VersionOpts
instance FromJSON  VersionOpts
-- instance CmdParser VersionOpts where
--     parseCmd = pure VersionOpts



-- === Root === --

data RootCmd = Build   BuildCfg
             | Clean   CleanOpts
             | Doc     DocOpts
             | Env     EnvOpts
             | Install InstallOpts
             | New     NewOpts
             | Package PackageOpts
             | Help    HelpOpts
             | Run     RunOpts
             | Version VersionOpts
             deriving (Generic, Show)

instance ToJSON   RootCmd
instance FromJSON RootCmd


rootCmd :: Parser RootCmd
rootCmd = subcommand "build"   Build   (help "Compile packages and dependencies.")
    --   <|> subcommand "clean"   Clean   (help "Clean compilation cache.")
    --   <|> subcommand "install" Install (help "Compile and install packages and dependencies.")
    --   <|> subcommand "run"     Run     (help "Compile and run Luna programs.")
    --   <|> command    "help"  helpCmd (help $ "Access help information." </> "Use `luna help topics` for extra help topics.")

helpCmd :: Parser a
helpCmd = helpTopicsCmd
      <|> command_ "topics" (action $ printHelpAndExit helpTopicsCmd)
      <|> action  (printHelpAndExit rootCmd)


helpTopicsCmd :: Parser a
helpTopicsCmd = helpTopic "generic-config" "hello" (help "Using generic interface for setting configuration.")

helpTopic :: Text -> Text -> (ArgConfig -> ArgConfig) -> Parser a
helpTopic n s f = command n (action $ print s >> liftIO exitSuccess) (f . tag_ "help-topic")


-- rootCmd = subparser (mconcat [ commandGroup "Compilation:", metavar "command"
--           , command "build"    . cmdInfo Build   $ progDesc "Compile packages and dependencies."
--           , command "clean"    . cmdInfo Clean   $ progDesc "Remove compilation cache."
--           , command "install"  . cmdInfo Install $ progDesc "Compile and install packages and dependencies."
--           , command "run"      . cmdInfo Run     $ progDesc "Compile and run Luna programs."
--           ])
--       <|> subparser (mconcat [ commandGroup "Package management:", hidden
--           , command "new"      . cmdInfo New     $ progDesc "Create new package."
--           , command "package"  . cmdInfo Package $ progDesc "Package management tools."
--           ])
--       <|> subparser (mconcat [ commandGroup "Information:", hidden
--           , command "help"     . cmdInfo Help    $ progDesc "Additional help topics."
--           , command "info"     . cmdInfo Version $ progDesc "Access environment information."
--           ])


subcommand n t = command n (t <$> parseCmd)

addHelp :: (OptParserT t m, HelpProvider t, MonadIO m) => FreeParserT t m a -> FreeParserT t m a
addHelp p = p <|> command_ "help" (action $ printHelpAndExit p)

addHelp' :: (OptParserT t m, HelpProvider t, MonadIO m) => FreeParserT t m a -> FreeParserT t m a
addHelp' p = command_ "help" (action $ printHelpAndExit p) <|> p

-- printHelpAndExit :: MonadIO m => m a
-- printHelpAndExit = liftIO $ outputHelp [("command", "Available commands:"), ("option", "Available options:")] rootCmd >> exitSuccess

printHelpAndExit :: (HelpProvider t, MonadIO m) => FreeParserT t n x -> m a
printHelpAndExit p = liftIO $ outputHelp titleTagMap p >> exitSuccess where
    titleTagMap = [ (,) "command"    "Available commands:"
                  , (,) "option"     "Available options:"
                  , (,) "help-topic" "Available help topics:"
                  ]


-------------------
-- === Shell === --
-------------------


updateCfg :: (FromJSON a, ToJSON a) => [Text] -> Text -> a -> Aeson.Result a
updateCfg path val a = fromJSON =<< newVal where
    newVal  = Aeson.patch diff (toJSON a)
    diff    = Aeson.Patch [Aeson.Rep (Aeson.Pointer (Aeson.OKey <$> path)) (mkVal val)]
    mkVal   = \case
        "true"  -> Aeson.Bool True
        "false" -> Aeson.Bool False
        s       -> Aeson.String s

main :: IO ()
main = do
    -- O.main
    -- putStrLn "----------------"
    args <- System.getArgs
    if null args
        then printHelpAndExit rootCmd
        else do
            out <- runOptionParser rootCmd args
            putStrLn . convert $ Aeson.encode out
            print $ updateCfg ["contents", "run"] "true" out

            -- let Just newVal = Aeson.decode "{\"contents\": {\"_run\": true}}" :: Maybe Aeson.Value
            --     -- diff   = Aeson.diff (Aeson.Object mempty) newVal
            --     diff   = Aeson.Patch [Aeson.Rep (Aeson.Pointer [Aeson.OKey "contents", Aeson.OKey "_run"]) (Aeson.Bool True)]
            --     newObj = Aeson.patch diff (toJSON out)
            --     newOut = fromJSON =<< newObj
            -- print diff
            -- pprint (toJSON out)
            -- pprint newObj
            -- print (newOut :: Aeson.Result RootCmd)
    -- print =<< runTest7
    -- return ()

runTest7 :: IO (Either (NonEmpty String) (String,String))
runTest7 = runBranchBreaker
         $ evalBacktracker
         $ runFailParser
         $ evalStreamProvider (listStream "bazzz")
         $ evalOffsetRegister
        --  $ (tokens "ba" <|> tokens "b")
         $ (,) <$> branched (tokens "ba") <*> (tokens "b")

-- runOptionParser :: MonadIO m => ParserT m a -> [String] -> m a

    -- putStrLn ""

    -- opts <- handleParseResult $ execParserPure prefs pinfo (preprocessArgs args)
    -- print opts
    -- return ()
    -- where prefs = defaultPrefs {prefShowHelpOnEmpty = True}
    --       pinfo = info rootCmd
    --             $ fullDesc <> header "Luna compiler and ecosystem toolkit."
    --                        <> footer "Use `luna [topic] help` for more information about that topic."

--
-- preprocessArgs :: [String] -> [String]
-- preprocessArgs = concat . go 0 where
--     go i   = \case []     -> []
--                    (a:as) -> (procArg i a) : go (succ i) as
--     procArg i a = if
--         | a == "help" && i /= 0 -> ["--help"] -- FIXME: https://github.com/pcapriotti/optparse-applicative/issues/272
--         | countMinus a == 1     -> ["--set", drop 1 a <> ".enabled", "false"]
--         | countPlus  a == 1     -> ["--set", drop 1 a <> ".enabled", "true"]
--         | otherwise             -> [a]
--
--
-- countMinus, countPlus :: String -> Int
-- countMinus = countPrefix '-'
-- countPlus  = countPrefix '+'
--
-- countPrefix :: Char -> String -> Int
-- countPrefix c = length . List.takeWhile (== c)
--
--
-- info p = Opts.info (p <**> helper)
--
-- cmdInfo p = info (p <$> parseCmd)
--
-- helper :: Parser (a -> a)
-- helper = abortOption ShowHelpText (internal <> long "help" <> help "Show this help text.")
--

-- print =<< customExecParser () opts
--   where
--     opts = info (buildOpts <**> helper)
--       ( fullDesc
--      <> progDesc "Print a greeting for TARGET"
--      <> header "hello - a test for optparse-applicative" )

-- greet :: BuildCfg -> IO ()
-- greet (BuildCfg h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
-- greet _ = return ()


--
-- luna build +pass simpleaa
--
-- luna build +debug.verbose import qualified Data.Map as Map


-- luna build --verbose debug
-- luna build +verbose
-- luna build -verbose --pass enable
