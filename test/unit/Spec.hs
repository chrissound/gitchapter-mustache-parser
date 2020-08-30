{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where


import           Control.Applicative   ((<$>), (<*>))
import           Data.Either
import           Data.Function         (on)
import           Data.Monoid
import qualified Data.Text             as T
import           System.IO.Unsafe      (unsafePerformIO)
import           Test.Hspec
import           Text.Mustache
import           Text.Mustache.Compile
import           Text.Mustache.Parser
import           Text.Mustache.Types


escaped :: Bool
escaped = True
unescaped :: Bool
unescaped = False


parserSpec :: Spec
parserSpec =
  describe "mustacheParser" $ do
    let lparse = parse "testsuite"
    let returnedOne = return . return

    let text = "hmm"

    it "parses text" $
      lparse text `shouldBe` returnedOne (TextBlock text)

    it "parses a variable" $
      lparse "{{name}}" `shouldBe` returnedOne (Variable escaped (NamedData ["name"]))

    it "parses a variable with whitespace" $
      lparse "{{ name  }}" `shouldBe` returnedOne (Variable escaped (NamedData ["name"]))

    it "allows '-' in variable names" $
      lparse "{{ name-name }}" `shouldBe`
        returnedOne (Variable True (NamedData ["name-name"]))

    it "allows '_' in variable names" $
      lparse "{{ name_name }}" `shouldBe`
        returnedOne (Variable True (NamedData ["name_name"]))

    it "parses a variable unescaped with {{{}}}" $
      lparse "{{{name}}}" `shouldBe` returnedOne (Variable unescaped (NamedData ["name"]))

    it "parses a variable unescaped with {{{}}} with whitespace" $
      lparse "{{{  name     }}}" `shouldBe`
        returnedOne (Variable False (NamedData ["name"]))

    it "parses a variable unescaped with &" $
      lparse "{{&name}}" `shouldBe` returnedOne (Variable unescaped (NamedData ["name"]))

    it "parses a variable unescaped with & with whitespace" $
      lparse "{{&  name  }}" `shouldBe`
        returnedOne (Variable False (NamedData ["name"]))

    it "parses a partial" $
      lparse "{{>myPartial}}" `shouldBe`
        returnedOne (Partial (Just "") "myPartial")

    it "parses a partial with whitespace" $
      lparse "{{>  myPartial }}" `shouldBe`
        returnedOne (Partial (Just "") "myPartial")

    it "parses the an empty section" $
      lparse "{{#section}}{{/section}}" `shouldBe`
        returnedOne (Section (NamedData ["section"]) mempty)

    it "parses the an empty section with whitespace" $
      lparse "{{#   section }}{{/     section }}" `shouldBe`
        returnedOne (Section (NamedData ["section"]) mempty)

    it "parses a delimiter change" $
      lparse "{{=<< >>=}}<<var>>{{var}}" `shouldBe`
        return [Variable True (NamedData ["var"]), TextBlock "{{var}}"]

    it "parses a delimiter change with whitespace" $
      lparse "{{=<<   >>=}}<< var   >>{{var}}" `shouldBe`
        return [Variable True (NamedData ["var"]), TextBlock "{{var}}"]

    it "parses two subsequent delimiter changes" $
      lparse "{{=((  ))=}}(( var ))((=--  $-=))--#section$---/section$-" `shouldBe`
        return [Variable True (NamedData ["var"]), Section (NamedData ["section"]) []]

    it "propagates a delimiter change from a nested scope" $
      lparse "{{#section}}{{=<< >>=}}<</section>><<var>>" `shouldBe`
        return [Section (NamedData ["section"]) [], Variable escaped (NamedData ["var"])]

    it "fails if the tag contains illegal characters" $
      lparse "{{#&}}" `shouldSatisfy` isLeft

    -- it "parses a nested variable" $
      -- lparse "{{ name.val }}" `shouldBe` returnedOne (Variable escaped (NamedData ["name.val"]))

    it "parses a variable containing whitespace" $
      lparse "{{ val space }}" `shouldBe` returnedOne (Variable escaped (NamedData ["val space"]))


substituteSpec :: Spec
substituteSpec =
  describe "substitute" $ do

    let toTemplate ast' = Template "testsuite" ast' mempty

    it "substitutes a html escaped value for a variable" $
      substitute
        (toTemplate [Variable escaped (NamedData ["name"])])
        (object ["name" ~> ("\" ' < > &" :: T.Text)])
      `shouldBe` "&quot; &#39; &lt; &gt; &amp;"

    it "substitutes raw value for an unescaped variable" $
      substitute
        (toTemplate [Variable unescaped (NamedData ["name"])])
        (object ["name" ~> ("\" ' < > &" :: T.Text)])
      `shouldBe` "\" ' < > &"

    it "substitutes a section when the key is present (and an empty object)" $
      substitute
        (toTemplate [Section (NamedData ["section"]) [TextBlock "t"]])
        (object ["section" ~> object []])
      `shouldBe` "t"

    it "substitutes a section when the key is present (and 'true')" $
      substitute
        (toTemplate [Section (NamedData ["section"]) [TextBlock "t"]])
        (object ["section" ~> True])
      `shouldBe` "t"

    it "substitutes a section once when the key is present and a singleton list" $
      substitute
        (toTemplate [Section (NamedData ["section"]) [TextBlock "t"]])
        (object ["section" ~> ["True" :: T.Text]])
      `shouldBe` "t"

    it "substitutes a section twice when the key is present and a list with two items" $
      substitute
        (toTemplate [Section (NamedData ["section"]) [TextBlock "t"]])
        (object ["section" ~> (["True", "False"] :: [T.Text])])
      `shouldBe` "tt"

    it "substitutes a section twice when the key is present and a list with two\
    \ objects, changing the scope to each object" $
      substitute
        (toTemplate [Section (NamedData ["section"]) [Variable escaped (NamedData ["t"])]])
        (object
          [ "section" ~>
            [ object ["t" ~> ("var1" :: T.Text)]
            , object ["t" ~> ("var2" :: T.Text)]
            ]
          ])
      `shouldBe` "var1var2"

    it "does not substitute a section when the key is not present" $
      substitute
        (toTemplate [Section (NamedData ["section"]) [TextBlock "t"]])
        (object [])
      `shouldBe` ""

    it "does not substitute a section when the key is present (and 'false')" $
      substitute
        (toTemplate [Section (NamedData ["section"]) [TextBlock "t"]])
        (object ["section" ~> False])
      `shouldBe` ""

    it "does not substitute a section when the key is present (and null)" $
      substitute
        (toTemplate [Section (NamedData ["section"]) [TextBlock "t"]])
        (object ["section" ~> Null])
      `shouldBe` ""

    it "does not substitute a section when the key is present (and empty list)" $
      substitute
        (toTemplate [Section (NamedData ["section"]) [TextBlock "t"]])
        (object ["section" ~> ([] :: [T.Text])])
      `shouldBe` ""

    it "substitutes a lambda by applying lambda to contained text" $
      substitute
        (toTemplate [Section (NamedData ["lambda"]) [TextBlock "t"]])
        (object ["lambda" ~> (overText T.toUpper)])
      `shouldBe` "T"

    it "substitutes a lambda by applying lambda to the nested substitution results" $
      substitute
        (toTemplate [Section (NamedData ["lambda"]) [TextBlock "t", Variable escaped (NamedData ["inner"])]])
        (object [ "lambda" ~> (overText T.toUpper)
                , "inner" ~> ("var" :: T.Text)
                ])
      `shouldBe` "TVAR"

    -- it "substitutes a lambda used directly as if applied to empty block" $
      -- substitute
        -- (toTemplate [Variable escaped (NamedData ["lambda"])])
        -- (object ["lambda" ~> (Lambda $ \[] -> return [TextBlock "T"])])
      -- `shouldBe` "T"

    it "substitutes a nested section" $
      substitute
        (toTemplate [Variable escaped (NamedData ["outer", "inner"])])
        (object
          [ "outer" ~> object ["inner" ~> ("success" :: T.Text)]
          , "inner" ~> ("error" :: T.Text)
          ]
        )
      `shouldBe` "success"


converterSpec :: Spec
converterSpec =
  describe "toMustache" $
    it "converts a String" $
      toMustache ("My String" :: String) `shouldSatisfy` \case (String "My String") -> True; _ -> False

-- This is a one-off instance to define how we want the Spec to compare templates
instance Eq Template where
  (==) = (==) `on` ast


main :: IO ()
main = hspec $ do
  parserSpec
  substituteSpec
  converterSpec
