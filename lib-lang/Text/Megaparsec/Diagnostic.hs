module Text.Megaparsec.Diagnostic where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Writer (MonadWriter (tell))
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Spanned
import Text.PrettyPrint (Doc)
import Text.PrettyPrint qualified as P
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))

data DiagnosticMessage = DiagnosticMessage
  { source :: Text
  , general :: Maybe Doc
  , spanned :: [Spanned Doc]
  }
  deriving stock (Eq)

instance Ord DiagnosticMessage where
  compare _ _ = EQ

instance Pretty DiagnosticMessage where
  pPrint (DiagnosticMessage source g ms) =
    maybe mempty (<> ":") g
      P.$+$ P.nest 2 (foldl (P.$+$) mempty (showSpannedMsg <$> ms))
    where
      lns = T.lines source
      showSpannedMsg (Spanned msg ss)
        | takeSingleLine ss =
            text (lns !! (unPos lineNum - 1))
              P.$+$ P.nest (unPos start - 1) (P.text $ replicate (unPos end - unPos start) '^')
              P.$+$ msg
        | otherwise = msg <> " (" <> P.text (show ss) <> ")"
        where
          lineNum = sourceLine $ (.spanStart) $ NE.head ss
          start = sourceColumn $ (.spanStart) $ NE.head ss
          end = sourceColumn $ (.spanEnd) $ NE.head ss
          takeSingleLine = all \(Span s e) -> sourceLine s == lineNum && sourceLine e == lineNum
          text = P.text . T.unpack

instance ShowErrorComponent DiagnosticMessage where
  showErrorComponent = show . pPrint

mkDiagnostic :: MonadReader Text m => Maybe Doc -> [Spanned Doc] -> m DiagnosticMessage
mkDiagnostic g s = ask <&> \source -> DiagnosticMessage source g s

diagnostic :: (MonadReader Text m, MonadWriter [DiagnosticMessage] m) => Maybe Doc -> [Spanned Doc] -> m ()
diagnostic g s = tell . pure =<< mkDiagnostic g s

diagnosticThrow :: (MonadReader Text m, MonadError DiagnosticMessage m) => Maybe Doc -> [Spanned Doc] -> m b
diagnosticThrow g s = throwError =<< mkDiagnostic g s
