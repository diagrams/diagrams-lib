import           Data.List                 (isSuffixOf)
import           Distribution.Simple
import           Distribution.Simple.Setup (Flag (..), HaddockFlags,
                                            haddockDistPref)
import           Distribution.Simple.Utils (copyFiles)
import           Distribution.Text         (display)
import           Distribution.Verbosity    (normal)
import           System.Directory          (getDirectoryContents)
import           System.FilePath           ((</>))

-- Ugly hack, logic copied from Distribution.Simple.Haddock
haddockOutputDir :: Package pkg => HaddockFlags -> pkg -> FilePath
haddockOutputDir flags pkg = destDir
   where
     baseDir = case haddockDistPref flags of
                      NoFlag -> "."
                      Flag x -> x
     destDir = baseDir </> "doc" </> "html" </> display (packageName pkg)

diagramsDir = "diagrams"

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
         { postHaddock = \args flags pkg lbi -> do
             dias <- filter ("svg" `isSuffixOf`) `fmap` getDirectoryContents diagramsDir
             copyFiles normal (haddockOutputDir flags pkg)
               (map (\d -> ("", diagramsDir </> d)) dias)
             postHaddock simpleUserHooks args flags pkg lbi
         }
