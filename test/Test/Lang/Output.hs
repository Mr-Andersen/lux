module Test.Output where

import Data.Functor.Identity(Identity(Identity))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Arbitrary ()
import Data.Void (Void)
import Lux.Output.Parsers
import Lux.Output.Types
import Lux.Matches ((=~))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Text.Megaparsec
import Text.Megaparsec.Spanned (Spanned)

parseSpanned :: MonadFail m => Text -> m (Output Spanned)
parseSpanned = either (fail . errorBundlePretty) pure . parse @Void (outputChoice <* eof) "tests"

(?=>) :: Text -> OutputChoice Identity -> TestTree
input ?=> p =
    testCase ("Parse " <> T.unpack input) $
        let errMsg = "parse " <> show input <> " /=~ " <> show p
         in assertBool errMsg . (=~ p) =<< parseSpanned input

tests :: TestTree
tests =
    testGroup
        "Lux.Output tests"
        [ "\"a\" b"
            ?=> OutputChoice
                { _choiceHead = Identity $ OutputSequence
                    { _seqHead = Identity $ OutputString "a"
                    , _seqTail = Just $ Identity $ OutputSequence
                        { _seqHead = Identity $ OutputValue
                            { _input = Identity "b"
                            , _fieldPath = Nothing
                            }
                        , _seqTail = Nothing
                        }
                    }
                , _choiceTail = Nothing
                }
        , "a b|c"
            ?=> OutputChoice
                { _choiceHead = Identity $ OutputSequence
                    { _seqHead = Identity $ OutputValue
                        { _input = Identity "a"
                        , _fieldPath = Nothing
                        }
                    , _seqTail = Just $ Identity $ OutputSequence
                        { _seqHead = Identity $ OutputValue
                            { _input = Identity "b"
                            , _fieldPath = Nothing
                            }
                        , _seqTail = Nothing
                        }
                    }
                , _choiceTail = Just $ Identity $ OutputChoice
                    { _choiceHead = Identity $ OutputSequence
                        { _seqHead = Identity $ OutputValue
                            { _input = Identity "c"
                            , _fieldPath = Nothing
                            }
                        , _seqTail = Nothing
                        }
                    , _choiceTail = Nothing
                    }
                }
        , "(a b)|c"
            ?=> OutputChoice
                { _choiceHead = Identity $ OutputSequence
                    { _seqHead = Identity $ OutputBraced $ OutputSequence
                        { _seqHead = Identity $ OutputValue
                            { _input = Identity "a"
                            , _fieldPath = Nothing
                            }
                        , _seqTail = Just $ Identity $ OutputSequence
                            { _seqHead = Identity $ OutputValue
                                { _input = Identity "b"
                                , _fieldPath = Nothing
                                }
                            , _seqTail = Nothing
                            }
                        }
                    , _seqTail = Nothing
                    }
                , _choiceTail = Just $ Identity $ OutputChoice
                    { _choiceHead = Identity $ OutputSequence
                        { _seqHead = Identity $ OutputValue
                            { _input = Identity "c"
                            , _fieldPath = Nothing
                            }
                        , _seqTail = Nothing
                        }
                    , _choiceTail = Nothing
                    }
                }
        , "(a.x.y b.x.y)|c.x.y"
            ?=> OutputChoice
                { _choiceHead = Identity $ OutputSequence
                    { _seqHead = Identity $ OutputBraced $ OutputSequence
                        { _seqHead = Identity $ OutputValue
                            { _input = Identity "a"
                            , _fieldPath = Just $ Identity $ FieldPath
                                { _fieldPathHead = Identity $ NamedField "x"
                                , _fieldPathTail = Just $ Identity $ FieldPath
                                    { _fieldPathHead = Identity $ NamedField "y"
                                    , _fieldPathTail = Nothing
                                    }
                                }
                            }
                        , _seqTail = Just $ Identity $ OutputSequence
                            { _seqHead = Identity $ OutputValue
                                { _input = Identity "b"
                                , _fieldPath = Just $ Identity $ FieldPath
                                    { _fieldPathHead = Identity $ NamedField "x"
                                    , _fieldPathTail = Just $ Identity $ FieldPath
                                        { _fieldPathHead = Identity $ NamedField "y"
                                        , _fieldPathTail = Nothing
                                        }
                                    }
                                }
                            , _seqTail = Nothing
                            }
                        }
                    , _seqTail = Nothing
                    }
                , _choiceTail = Just $ Identity $ OutputChoice
                    { _choiceHead = Identity $ OutputSequence
                        { _seqHead = Identity $ OutputValue
                            { _input = Identity "c"
                            , _fieldPath = Just $ Identity $ FieldPath
                                { _fieldPathHead = Identity $ NamedField "x"
                                , _fieldPathTail = Just $ Identity $ FieldPath
                                    { _fieldPathHead = Identity $ NamedField "y"
                                    , _fieldPathTail = Nothing
                                    }
                                }
                            }
                        , _seqTail = Nothing
                        }
                    , _choiceTail = Nothing
                    }
                }
        ]
