module Gui
    (runGui
    ,stateFuncL --testing
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State --use Lazy?
import Data.String
-- import Data.IORef
-- import System.Glib.UTFString.GLibString --required for type of fatEntryRow2
                                           --otherwise no haddock for this?
import Graphics.UI.Gtk hiding (Action, backspace)
import CmdLine (execParseStrings)
import Options.Applicative (handleParseResult, ParserFailure(..), ParserResult(..))
import Options.Applicative.Help.Types

-- |Run the graphical user interface to input the fat information
runGui :: IO()
runGui = do
  void initGUI
  builder <- builderNew
  builderAddFromFile builder "gui.glade" -- proper location of glade files?
  window <- builderGetObject builder castToWindow "mainWindow"
  okButton <- builderGetObject builder castToButton "okButton"
  cancelButton <- builderGetObject builder castToButton "cancelButton"
  addButton <- builderGetObject builder castToButton "addButton"
  --------------------Event Connections----------------------------------
  on window deleteEvent $ do
    liftIO $ putStrLn "Got deleteEvent on main window"
    liftIO mainQuit
    return False
  -- this should call widgetDestroy on the toplevel
  on cancelButton buttonActivated $ liftIO mainQuit
  on okButton buttonActivated $ okReadAction builder
  on addButton buttonActivated $ addFatAction2 builder
  widgetShowAll window  
  mainGUI               

-- |Same as runGui but using option string to initialise fat entry fields
runGuiWith :: IsString s => s -> IO()
runGuiWith = undefined
-------------------------data entry field construction--------------------
-- | a rewrite of boxPackStart for chaining
boxPacker
  :: (BoxClass b, WidgetClass child) =>
     Packing -> Int -> b -> child -> IO b
boxPacker packType pad bx elem = boxPackStart bx elem packType pad >> return bx

-- |Add a new entry for fat input and display it.
-- A fat entry field is a 2-row vbox consisting of a row for the fat type
-- and one for the fat amount
addFatAction2 :: Builder -> IO()
addFatAction2 build =
  builderGetObject build castToBox "entryBoxContainer" >>= packEntry >>= showAll
  where
    packEntry b = newFatEntry2 >>= boxPacker PackNatural 5 b
    showAll b = widgetShowAll b
    
-- |Pack rows for fat-type and amount into the entry box
newFatEntry2 :: IO VBox
newFatEntry2 =
  vBoxNew False 0 >>= packTypeRow >>= packAmountRow
  where 
    packTypeRow vb = fatEntryRow2 "Fat type" addDeleteButton2 >>= packNatural vb
    packAmountRow vb = fatEntryRow2 "Amount" (return . id) >>= packNatural vb
    packNatural = boxPacker PackNatural 0 

 {-|Pack a label with text labtext, an entry field and a delete button
   with action delButton into a row for the fat-entry. The idea is to have
   a delete button in the amount row generated by @delButton=addDeleteButton@
   for deleting the whole entry and no button in the amount row using
   @delButton=return . id@. See the usage in @packTypeRow@ and @packAmountRow@
   in @newFatEntry2@
 -}
--fatEntryRow2 :: System.Glib.UTFString.GlibString string =>
                --string -> (HBox -> IO b) -> IO b   
fatEntryRow2 labtxt delButton =
  hBoxNew False 0 >>= packLabel >>= packEntry >>= delButton
     where
       packLabel = \b -> labelNew (Just labtxt) >>= boxPackerLocalNat b
       packEntry = \b -> entryNew >>= boxPackerLocalGrow b
       boxPackerLocalNat  = boxPacker PackNatural 10
       boxPackerLocalGrow  = boxPacker PackGrow 0
-- |Pack a button with label in the given and connect an action that removes
-- the fat entry from the entry containers
-- addDeleteButton2 :: BoxClass b => b -> IO b
addDeleteButton2 hb =
  buttonNewWithLabel "-" >>= packer hb >>= onActivation hb >> return hb
    where
      -- ^Note: boxPacker can't be used here as
      -- > packer = boxPacker PackNatural 0
      -- it returns the wrong type, namely @hb@ rather than @elem@
      packer hb = \elem -> boxPackStart hb elem PackNatural 0 >> return elem
      onActivation hpar = \button -> on button buttonActivated (rmAction hpar)
      -- rmAction :: GObjectClass a => a -> IO ()
      rmAction h = do
        Just ctent <- widgetGetParent (castToWidget h)
        Just con <- widgetGetParent ctent
        containerRemove (castToContainer con) ctent
-------------------------------data extraction---------------------------------
-- |State transition function for creating StateT monad to pass string as
-- state and list of row-boxes as values in a StateT String IO monad
-- Deprecated, use stateFuncL to conform with getArgs usage
stateFunc :: GObjectClass a => [a] -> [Char] -> IO ([a], [Char])
stateFunc (b:r) s|s=="" = getText b >>= \str -> return (r,"--fatspec " ++ str)
             |otherwise = getText b >>= \str -> return (r,s ++ ":" ++ str)
             where getText c= containerGetChildren (castToContainer c) >>= etext
                   etext (_:etrt:_) = entryGetText $ castToEntry etrt
                   etext _ = error "etext not receiving proper row"
stateFunc [] s = return ([],s) -- TODO: think about this

-- |State transition function for creating StateT monad to pass [String] as
-- state and list of row-boxes as values in a StateT [String] IO monad
-- to produce output the same way getArgs would as a list of strings
stateFuncL :: GObjectClass obj => [obj] -> [[Char]] -> IO ([obj], [[Char]])
stateFuncL (b:r) s|s==[] = getText b >>= \str -> return (r,["--fatspec",str])
                  |otherwise = getText b >>= \str -> return (r,init s ++ [last s ++ ":" ++ str])
             where getText c= containerGetChildren (castToContainer c) >>= etext
                   etext (_:etrt:_) = entryGetText $ castToEntry etrt
                   etext _ = error "etext not receiving proper row"
stateFuncL [] s = return ([],s) -- TODO: think about this
-- |Extracting fat information from the fat entry, which consists of
-- two vboxes representing the fat-type and fat-amount rows, respectively,
-- which in turn consist of subwidgets, the second one being the text entry
-- field of interest. This is actually used as getFatInfoM :: VBox -> IO String.
getFatInfoM :: ContainerClass self => self -> IO [[Char]]
getFatInfoM b = containerGetChildren b >>= \cl -> execStateT (extractor cl >>= extractor) []
  where extractor  = StateT . stateFuncL

-- |action for ok-button: read the contents of the fat-entry children
-- Note: these should be fed into an option-checker rather than a mere
-- putStrLn . show, with a complaint-dialog if things go wrong
-- TODO:this doesn't work yet the list going into processFatInfo should
-- be of the form ["--fatspec","fat1:0.5","--fatspec","fat2:2.0", ...]
-- because getArgs splits at unquoted whitespace, so this needs to be folded
-- This has to be changed in the stateFunc
okReadAction :: Builder -> IO ()
okReadAction b = do
  entries <-builderGetObject b castToContainer "entryBoxContainer"
  top <- builderGetObject b castToWindow "mainWindow"
  containerGetChildren entries >>= foldM foldFatInfo [] >>= processFatInfo top
  return ()

-- handleParseResult is a bit radical because it terminates the whole thing
-- in case of parse errors, it should just complain
-- TODO: reimplement handleParseResult so error messages can be used
-- for error dialogs and correct parses for calculations
processFatInfo parent = processParseResult parent . execParseStrings
  where
    processParseResult p (Success a) = messageDialogNew (Just p) [DialogModal] MessageInfo ButtonsOk "Calculating Ingredients" >>= \dlg -> dialogRun dlg >> widgetDestroy dlg
    processParseResult p (Failure pf) =messageDialogNew (Just p) [DialogModal] MessageWarning ButtonsOk (show pf) >>= \dlg -> dialogRun dlg >> widgetDestroy dlg
    -- processParseResult (Success a) = putStrLn $ "Parse ok: got " ++ show a
    -- processParseResult (Failure pf) = putStrLn "Parse is no good"
    -- processParseResult (Failure pf) = putStrLn (show pf)
    -- processParseResult (Failure (ParserFailure ph)) = putStrLn (show (getParserHelper ph))
-- ph :: String-> (ParserHelp, ExitCode, Integer) use as ph "error message"
-- exitFailure :: String -> (ParserHelp, ExitCode,Integer) where the
-- String is what comes after the Usage:, its default being "<program>".
getParserHelper pex = let fst3 (f,_,_) = f in
                         fst3 (pex "Failure String")
-- |Function to foldM an IO [Children] to concatenate the IO [String] results
-- from getFatInfoM. Since containerGetChildren returns widgets, they have
-- to be cast back to containers in order to extract their children in return.
-- TODO: this should probably be replaced by concatMapM and getFatInfoM
foldFatInfo strl box = getFatInfoM (castToContainer box) >>= \nstrl -> return (strl++nstrl)
