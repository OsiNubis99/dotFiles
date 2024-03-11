module Custom.MyWorkspaces where

import qualified Data.Map as M
import Data.Maybe

myWorkspaces :: [String]
myWorkspaces = ["Editor", "Web", "Game", "File", "NSP"]

myWorkspaceIcons :: M.Map String String
myWorkspaceIcons =
  M.fromList $
    zip
      myWorkspaces
      ["\61595", "\62057", "\57879", "\61563", "\57871", "\57969", ""]

myWorkspaceIndices :: M.Map String Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 ..]

clickable :: String -> String
clickable wsName =
  "<action=xdotool key super+"
    ++ show i
    ++ "><fc=#ffaa00><fn=1>"
    ++ wsIcon
    ++ "</fn> </fc>"
    ++ wsName
    ++ "</action>"
  where
    i = fromJust $ M.lookup wsName myWorkspaceIndices
    wsIcon = fromJust $ M.lookup wsName myWorkspaceIcons
