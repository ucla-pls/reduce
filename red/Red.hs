-- The red application

-- text
import qualified Data.Text as Text

-- optparse-applicative
import Options.Applicative


data Format = Format
  { flag :: !Char
  , description :: !Text.Text
  }

getConfigParser :: [ Format ] -> IO (Parser (IO x))
getConfigParser =
  undefined



main = do
  -- parseConfig <- getConfigParser formats
  config <- execParser $
    info (pure () <**> helper)
    ( fullDesc
    <> header "red"
    <> progDesc "A command line tool for reducing almost anything."
    )

  print config
