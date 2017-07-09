{-# LANGUAGE OverloadedStrings #-}

module Main where

import Luna.Prelude hiding (Level, switch, argument, (<+>))
import qualified Luna.Shell
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map  as Map
import qualified Data.Text as Text
import qualified System.Environment as System

import qualified Control.Lens.Aeson as Lens
import System.Console.Options hiding (main)
import qualified System.Console.Options as O
import System.Exit (exitSuccess, exitFailure)
import qualified Data.Layout        as Doc
import           Data.Layout        ((<+>), (</>), (<//>))

import qualified Data.Aeson         as Aeson
import           Data.Aeson         (ToJSON, toJSON, FromJSON, fromJSON)
import qualified Data.Aeson.Diff    as Aeson
import qualified Data.Aeson.Pointer as Aeson
import Control.Monad.Branch

import qualified Text.Parsert as Parsert
import Text.Parsert hiding (Result)

import qualified Data.TreeMap as TreeMap
import           Data.TreeMap (SparseTreeMap)

import qualified Data.Aeson.Lens
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Vector       as Vector

instance Convertible Bool String where convert = show
instance Convertible Bool Text   where convert = convertVia @String

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


--------------------
-- === Errors === --
--------------------

-- === WrongConfPathError === --

newtype WrongConfPathError = WrongConfPathError Text deriving (Show)
makeLenses ''WrongConfPathError

wrongConfPathError :: Text -> SomeError
wrongConfPathError = toSomeError . WrongConfPathError

instance IsError WrongConfPathError where
    renderError e  = "Wrong configuration path:" <+> convert (unwrap e)



---------------------------------------
-- === Generic config management === --
---------------------------------------

-- === Config update === --

updateCfg :: (FromJSON a, ToJSON a) => [Text] -> Text -> a -> Either Text a
updateCfg path val a = convert1 $ fromJSON =<< newVal where
    newVal  = Aeson.patch diff (toJSON a)
    diff    = Aeson.Patch [Aeson.Rep (Aeson.Pointer (Aeson.OKey <$> path)) (mkVal val)]
    mkVal s = case Text.toLower s of
        "true"  -> Aeson.Bool True
        "false" -> Aeson.Bool False
        _       -> Aeson.String s

-- updateCfg2 :: (FromJSON a, ToJSON a) => [Text] -> Text -> a -> Either Text a
-- updateCfg2 path val a = convert1 $ fromJSON =<< newVal where
--     newVal  = Aeson.patch diff (toJSON a)
--     diff    = Aeson.Patch [Aeson.Rep (Aeson.Pointer (Aeson.OKey <$> path)) (mkVal val)]
--     mkVal s = case Text.toLower s of
--         "true"  -> Aeson.Bool True
--         "false" -> Aeson.Bool False
--         _       -> Aeson.String s



-- mapMValue :: Monad m => Text -> (Aeson.Value -> m Aeson.Value) -> Aeson.Value -> m Aeson.Value
-- mapMValue t f = \case
--     Aeson.Object m -> Aeson.Object <$> HashMap.lookup t m
    -- Aeson.Array  v -> Aeson.Array  <$> Vector.mapM (mapMValue f) v
    -- Aeson.String s -> error "todo"
    -- Aeson.Number n -> error "todo"
    -- Aeson.Bool   b -> error "todo"
    -- Aeson.Null     -> error "todo"

-- traverseWithKey :: Applicative f => (k -> v1 -> f v2) -> HashMap k v1 -> f (HashMap k v2)

updateCfgM :: (FromJSON a, ToJSON a, MonadErrorParser SomeError m) => [Text] -> Text -> a -> m a
updateCfgM = either (raise . wrongConfPathError) return .:. updateCfg


-- === Parsers === --

fullOptSetParser :: Parser ([Text], Text)
fullOptSetParser = subParser $ (,) . Text.splitOn "."
    <$> strOption "--" "set" (help $ "Set configuration options." </> "Use `luna help generic-config` to learn more.")
    <*> argument (convert <$> lexeme anyWord) id

shortOptSwitchParser :: Char -> Text -> (ArgConfig -> ArgConfig) -> Parser ([Text], Text)
shortOptSwitchParser pfx val cfg = subParser $ ((,val) . (<> ["enabled"]) . Text.splitOn ".")
    <$  argument (token pfx <* notFollowedBy (token pfx)) (cfg . tag "option" . label (convert pfx <> " " <> "opt"))
    <*> argument (convert <$> lexeme anyWord) id

shortOptDisableParser, shortOptEnableParser :: Parser ([Text], Text)
shortOptDisableParser = shortOptSwitchParser '-' "false" (help "Shortcut for --set opt.enabled false")
shortOptEnableParser  = shortOptSwitchParser '+' "true"  (help "Shortcut for --set opt.enabled true")

optSetParser :: Parser ([Text], Text)
optSetParser = fullOptSetParser <|> shortOptDisableParser <|> shortOptEnableParser

handleGenConf :: (FromJSON a, ToJSON a) => Parser a -> Parser a
handleGenConf p = bindParser (uncurry $ foldM (flip $ uncurry updateCfgM))
                $ (,) <$> p <#> multiple optSetParser

handleGenConf' :: (FromJSON a, ToJSON a) => Parser (a, [([Text], Text)]) -> Parser a
handleGenConf' p = bindParser (uncurry $ foldM (flip $ uncurry updateCfgM))
                 $ flip ((_2 %~) . flip (<>)) <$> p <#> multiple optSetParser


-- === Aeson utils === --

-- TODO: We might want to generalize them
instance IsString e => Convertible1 Aeson.Result (Either e) where
    convert1 = \case
        Aeson.Success a -> Right a
        Aeson.Error   e -> Left $ fromString e



------------------------
-- === ConfigTree === --
------------------------

type ConfigTree = Map Text Text

-- configTreeParser :: Parser ConfigTree
-- configTreeParser = foldr (uncurry Map.insert) mempty <$> multiple optSetParser




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
--
-- newtype Switch = Switch { _enabled :: Bool } deriving (Generic, Show)
-- makeLenses ''Switch

-- instance ToJSON   Switch where toEncoding = Lens.toEncoding; toJSON = Lens.toJSON
-- instance FromJSON Switch where parseJSON  = Lens.parse


data Hook = Hook
    { _enabled :: Bool
    , _options :: Map Text Text
    } deriving (Generic, Show)
makeLenses ''Hook

instance ToJSON   Hook where toEncoding = Lens.toEncoding; toJSON = Lens.toJSON
instance FromJSON Hook where parseJSON  = Lens.parse


data BuildHooks = BuildHooks
    { _after :: Map Text Hook
    } deriving (Generic, Show)

instance ToJSON   BuildHooks where toEncoding = Lens.toEncoding; toJSON = Lens.toJSON
instance FromJSON BuildHooks where parseJSON  = Lens.parse

data BuildCfg = BuildCfg
    { --_optimization :: Optimization
    -- , _pragmas      :: Map Text Pragma
    -- , _pretend      :: Bool
     _hooks        :: BuildHooks
    , _pass        :: ConfigTree
    -- , _report       :: ConfigTree
    } deriving (Generic, Show)
makeLenses ''BuildCfg

instance Mempty BuildCfg where
    mempty = BuildCfg (BuildHooks $ fromList [("run", Hook False mempty)]) mempty

-- stats
instance ToJSON   BuildCfg where toEncoding = Lens.toEncoding; toJSON = Lens.toJSON
instance FromJSON BuildCfg where parseJSON  = Lens.parse

instance CmdParser BuildCfg where
    parseCmd = addHelp'
             $ handleGenConf' . fmap (mempty,)
             $ pure [] -- <$> (confAlias "hooks.after.run.enabled" <$> flag "run" (help "Run the output program after successful compilation."))

 --  $ (\r -> mempty & hooks.after.run.enabled .~ r) <$> flag "run" (help "Run the output program after successful compilation.")
--
confAlias :: Convertible' t Text => Text -> t -> ([Text],Text)
confAlias s = ((,) . Text.splitOn ".") s . convert'

-- updateCfg :: (FromJSON a, ToJSON a) => [Text] -> Text -> a -> Either Text a
-- foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
-- bindParser :: Monad m => (x -> m a) -> FreeParserT t m x -> FreeParserT t m a

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
      <|> subcommand "clean"   Clean   (help "Clean compilation cache.")
      <|> subcommand "install" Install (help "Compile and install packages and dependencies.")
      <|> subcommand "run"     Run     (help "Compile and run Luna programs.")
      <|> command    "help"    helpCmd (help $ "Access help information." </> "Use `luna help topics` for extra help topics.")

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











data Result e a = Result [e] Bool a
                | Err    [Text] [Text]
                deriving (Show, Functor, Traversable, Foldable)
makeLenses ''Result

mapErr :: ([Text] -> [Text]) -> Result e a -> Result e a
mapErr f = \case
    Result es b a -> Result es b a
    Err    e  s   -> Err (f e) s

class Monad m => MonadRegex m where
    returnUnchanged :: forall a. a -> m a
    returnChanged   :: forall a. a -> m a
    -- checkSuccess    :: forall a. m a -> Bool
    -- dropErrors      :: forall a. m a -> m a


class MonadErr e m where
    failed     :: forall a. e -> a -> m a


instance Applicative (Result e) where
    pure = Result mempty False
    Err e s       <*> _               = Err e s
    _             <*> Err e s         = Err e s
    Result es b f <*> Result es' b' a = Result (es <> es') (b || b') (f a)

instance Monad (Result e) where
    Err e s >>= f = Err e s
    Result es b a >>= f = case f a of
        Result es' b' a' -> Result (es <> es') (b || b') a'
        Err    e s       -> Err e s

instance MonadRegex (Result e) where
    returnUnchanged = Result mempty False
    returnChanged   = Result mempty True
    -- checkSuccess    = view success
    -- dropErrors      = ways .~ mempty

instance (e ~ e') => MonadErr e (Result e') where
    failed e = Result [e] False



tstf :: Aeson.Value -> Result Text Aeson.Value
tstf = \case
    Aeson.String s -> returnChanged $ Aeson.String "newVal"


updateCfg2 :: [Text] -> (Aeson.Value -> Result Text Aeson.Value) -> Aeson.Value -> Result Text Aeson.Value
updateCfg2 path f a = case path of
    []          -> f a
    ["*"]       -> error "todo"
    ["**"]      -> error "todo"
    ("*"  : ps) -> mapErr ("*" :) $ mapMValue (const $ updateCfg2 ps f) a
    (p    : ps) -> case val of
        Result es ok a -> if ok then Result mempty ok a else Err [p] es
        Err    e s     -> Err (p:e) s
        where val = mapMValue (\t -> if t == p then updateCfg2 ps f else failed t) a
        -- if checkSuccess val then dropErrors val else val where
        -- case val of

    -- (p:ps)   -> mapMValue p

mapMValue :: Monad m => (Text -> Aeson.Value -> m Aeson.Value) -> Aeson.Value -> m Aeson.Value
mapMValue f = \case
    Aeson.Object m -> Aeson.Object <$> HashMap.traverseWithKey f m
    Aeson.Array  v -> Aeson.Array  <$> Vector.mapM (mapMValue f) v
    Aeson.String s -> error "todo"
    Aeson.Number n -> error "todo"
    Aeson.Bool   b -> error "todo"
    Aeson.Null     -> error "todo"




data Foo = Foo { _x :: Text
               , _y :: Text
               , _z :: Text
               } deriving (Generic, Show)
makeLenses ''Foo

instance ToJSON   Foo where toEncoding = Lens.toEncoding; toJSON = Lens.toJSON
instance FromJSON Foo where parseJSON  = Lens.parse

data Bar = Bar { _foo1 :: Foo
               , _foo2 :: Foo
               } deriving (Generic, Show)
makeLenses ''Bar

instance ToJSON   Bar where toEncoding = Lens.toEncoding; toJSON = Lens.toJSON
instance FromJSON Bar where parseJSON  = Lens.parse




main :: IO ()
main = do
    -- O.main
    -- putStrLn "----------------"
    -- let m = fromList [("a","b")] :: Map Text Text
    -- print $ Aeson.encode m

    let json = toJSON (Bar (Foo "defx" "defy" "defz") (Foo "defx2" "defy2" "defz2"))
    pprint json
    pprint $ updateCfg2 ["*", "a"] tstf json


    args <- System.getArgs
    if null args
        then printHelpAndExit rootCmd
        else do
            out <- runOptionParser rootCmd args
            print out


-- runTest7 :: IO (Either (NonEmpty String) (String,String))
-- runTest7 = runBranchBreaker
--          $ evalBacktracker
--          $ runFailParser
--          $ evalStreamProvider (listStream "bazzz")
--          $ evalOffsetRegister
--         --  $ (tokens "ba" <|> tokens "b")
--          $ (,) <$> branched (tokens "ba") <*> (tokens "b")

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
