module Test.Input where

import Data.Functor.Identity(Identity(Identity))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Arbitrary ()
import Data.Void (Void)
import Lux.Input.Parsers
import Lux.Input.Types
import Lux.Matches ((=~))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Text.Megaparsec
import Text.Megaparsec.Spanned (Spanned)

parseSpanned :: MonadFail m => Text -> m (Pattern Spanned)
parseSpanned = either (fail . errorBundlePretty) pure . parse @Void (patternChoice <* eof) "tests"

(?=>) :: Text -> Pattern Identity -> TestTree
input ?=> p =
    testCase ("Parse " <> T.unpack input) $
        let errMsg = "parse " <> show input <> " /=~ " <> show p
         in assertBool errMsg . (=~ p) =<< parseSpanned input

tests :: TestTree
tests =
    testGroup
        "Lux.Input tests"
        [ "(ident)"
            ?=> PatternChoice
                { _piece = Identity $ PatternSequence
                    { _patSeqHead = Identity $ PatternModded
                        { _patPiece = Identity $ PBind
                            { _varName = Identity $ BindId "ident"
                            , _bindPat = Identity $ PatternChoice
                                { _piece = Identity $ PatternSequence
                                    { _patSeqHead = Identity $ PatternModded
                                        { _patPiece = Identity $ PIdent "ident"
                                        , _patMod = Nothing
                                        }
                                    , _patSeqTail = Nothing
                                    }
                                , _patternTail = Nothing
                                }
                            }
                        , _patMod = Nothing
                        }
                    , _patSeqTail = Nothing
                    }
                , _patternTail = Nothing
                }
        , "(var:dateTime)"
            ?=> PatternChoice
                { _piece = Identity $ PatternSequence
                    { _patSeqHead = Identity $ PatternModded
                        { _patPiece = Identity $ PBind
                            { _varName = Identity $ BindId "var"
                            , _bindPat = Identity $ PatternChoice
                                { _piece = Identity $ PatternSequence
                                    { _patSeqHead = Identity $ PatternModded
                                        { _patPiece = Identity $ PIdent "dateTime"
                                        , _patMod = Nothing
                                        }
                                    , _patSeqTail = Nothing
                                    }
                                , _patternTail = Nothing
                                }
                            }
                        , _patMod = Nothing
                        }
                    , _patSeqTail = Nothing
                    }
                , _patternTail = Nothing
                }
        , "(_:dateTime)"
            ?=> PatternChoice
                { _piece = Identity $ PatternSequence
                    { _patSeqHead = Identity $ PatternModded
                        { _patPiece = Identity $ PBind
                            { _varName = Identity BindDiscard
                            , _bindPat = Identity $ PatternChoice
                                { _piece = Identity $ PatternSequence
                                    { _patSeqHead = Identity $ PatternModded
                                        { _patPiece = Identity $ PIdent "dateTime"
                                        , _patMod = Nothing
                                        }
                                    , _patSeqTail = Nothing
                                    }
                                , _patternTail = Nothing
                                }
                            }
                        , _patMod = Nothing
                        }
                    , _patSeqTail = Nothing
                    }
                , _patternTail = Nothing
                }
        , "(var:pat+)"
            ?=> PatternChoice {
                _piece = Identity $ PatternSequence
                    { _patSeqHead = Identity $ PatternModded
                        { _patPiece = Identity $ PBind
                            { _varName = Identity $ BindId "var"
                            , _bindPat = Identity $ PatternChoice
                                { _piece = Identity $ PatternSequence
                                    { _patSeqHead = Identity $ PatternModded
                                        { _patPiece = Identity $ PIdent "pat"
                                        , _patMod = Just $ Identity ModSome
                                        }
                                    , _patSeqTail = Nothing
                                    }
                                , _patternTail = Nothing
                                }
                            }
                        , _patMod = Nothing
                        }
                    , _patSeqTail = Nothing
                    }
                , _patternTail = Nothing
                }
        , "(var:patA*|patB+|patC?)"
            ?=> PatternChoice {
                _piece = Identity $ PatternSequence
                    { _patSeqHead = Identity $ PatternModded
                        { _patPiece = Identity $ PBind
                            { _varName = Identity $ BindId "var"
                            , _bindPat = Identity $ PatternChoice
                                { _piece = Identity $ PatternSequence
                                    { _patSeqHead = Identity $ PatternModded
                                        { _patPiece = Identity $ PIdent "patA"
                                        , _patMod = Just $ Identity ModMany
                                        }
                                    , _patSeqTail = Nothing
                                    }
                                , _patternTail = Just $ Identity $ PatternChoice
                                    { _piece = Identity $ PatternSequence
                                        { _patSeqHead = Identity $ PatternModded
                                            { _patPiece = Identity $ PIdent "patB"
                                            , _patMod = Just $ Identity ModSome
                                            }
                                        , _patSeqTail = Nothing
                                        }
                                    , _patternTail = Just $ Identity $ PatternChoice
                                        { _piece = Identity $ PatternSequence
                                            { _patSeqHead = Identity $ PatternModded
                                                { _patPiece = Identity $ PIdent "patC"
                                                , _patMod = Just $ Identity ModOptional
                                                }
                                            , _patSeqTail = Nothing
                                            }
                                        , _patternTail = Nothing
                                        }
                                    }
                                }
                            }
                        , _patMod = Nothing
                        }
                    , _patSeqTail = Nothing
                    }
                , _patternTail = Nothing
                }
        , "(r:\"-\"|\"r\"),(w:\"-\"|\"w\"),(x:\"-\"|\"x\")"
            ?=> PatternChoice
                { _piece = Identity $ PatternSequence
                    { _patSeqHead = Identity $ PatternModded
                        { _patPiece = Identity $ PBind
                            { _varName = Identity $ BindId "r"
                            , _bindPat = Identity $ PatternChoice
                                { _piece = Identity $ PatternSequence
                                    { _patSeqHead = Identity $ PatternModded
                                        { _patPiece = Identity $ PText "-"
                                        , _patMod = Nothing
                                        }
                                    , _patSeqTail = Nothing
                                    }
                                , _patternTail = Just $ Identity $ PatternChoice
                                    { _piece = Identity $ PatternSequence
                                        { _patSeqHead = Identity $ PatternModded
                                            { _patPiece = Identity $ PText "r"
                                            , _patMod = Nothing
                                            }
                                        , _patSeqTail = Nothing
                                        }
                                    , _patternTail = Nothing
                                    }
                                }
                            }
                        , _patMod = Nothing
                        }
                    , _patSeqTail = Just (Identity SepComma, Identity $ PatternSequence {
                        _patSeqHead = Identity $ PatternModded
                            { _patPiece = Identity $ PBind
                                { _varName = Identity $ BindId "w"
                                , _bindPat = Identity $ PatternChoice
                                    { _piece = Identity $ PatternSequence
                                        { _patSeqHead = Identity $ PatternModded
                                            { _patPiece = Identity $ PText "-"
                                            , _patMod = Nothing
                                            }
                                        , _patSeqTail = Nothing
                                        }
                                    , _patternTail = Just $ Identity $ PatternChoice
                                        { _piece = Identity $ PatternSequence
                                            { _patSeqHead = Identity $ PatternModded
                                                { _patPiece = Identity $ PText "w"
                                                , _patMod = Nothing
                                                }
                                            , _patSeqTail = Nothing
                                            }
                                        , _patternTail = Nothing
                                        }
                                    }
                                }
                            , _patMod = Nothing
                            }
                        , _patSeqTail = Just (Identity SepComma, Identity $ PatternSequence
                            { _patSeqHead = Identity $ PatternModded
                                { _patPiece = Identity $ PBind
                                    { _varName = Identity $ BindId "x"
                                    , _bindPat = Identity $ PatternChoice
                                        { _piece = Identity $ PatternSequence
                                            { _patSeqHead = Identity $ PatternModded
                                                { _patPiece = Identity $ PText "-"
                                                , _patMod = Nothing
                                                }
                                            , _patSeqTail = Nothing
                                            }
                                        , _patternTail = Just $ Identity $ PatternChoice
                                            { _piece = Identity $ PatternSequence
                                                { _patSeqHead = Identity $ PatternModded
                                                    { _patPiece = Identity $ PText "x"
                                                    , _patMod = Nothing
                                                    }
                                                , _patSeqTail = Nothing
                                                }
                                            , _patternTail = Nothing
                                            }
                                        }
                                    }
                                , _patMod = Nothing
                                }
                            , _patSeqTail = Nothing
                            })
                        })
                    }
                , _patternTail = Nothing
                }
        , "(fsNodeType),(permissions:permissions{3})"
            ?=> PatternChoice
                { _piece = Identity $ PatternSequence
                    { _patSeqHead = Identity $ PatternModded
                        { _patPiece = Identity $ PBind
                            { _varName = Identity $ BindId "fsNodeType"
                            , _bindPat = Identity $ PatternChoice
                                { _piece = Identity $ PatternSequence
                                    { _patSeqHead = Identity $ PatternModded
                                        { _patPiece = Identity $ PIdent "fsNodeType"
                                        , _patMod = Nothing
                                        }
                                    , _patSeqTail = Nothing
                                    }
                                , _patternTail = Nothing
                                }
                            }
                        , _patMod = Nothing
                        }
                    , _patSeqTail = Just (Identity SepComma, Identity $ PatternSequence
                        { _patSeqHead = Identity $ PatternModded {
                        _patPiece = Identity $ PBind
                                { _varName = Identity $ BindId "permissions"
                                , _bindPat = Identity $ PatternChoice
                                    { _piece = Identity $ PatternSequence
                                        { _patSeqHead = Identity $ PatternModded
                                            { _patPiece = Identity $ PIdent "permissions"
                                            , _patMod = Just $ Identity $ ModRepeat 3
                                            }
                                        , _patSeqTail = Nothing
                                        }
                                    , _patternTail = Nothing
                                    }
                                }
                            , _patMod = Nothing
                            }
                        , _patSeqTail = Nothing
                        })
                    }
                , _patternTail = Nothing
                }
        ]
