module Main where

import Ordn
import Ordn.Config
import Ordn.Periodic

main :: IO [()]
main = do
  let
    tests =
      [ getTodosForTodayReturnsEmptyWithEmptyDoc
      , getTodosForTodayReturnsDailyItems
      , getTodosForTodayReturnsDailyExceptItems
      , getTodosForTodayReturnsDailyExceptMultipleDaysItems
      , getTodosForTodayDoesNotReturnDailyItemsForExceptedDays
      , getTodosForTodayDoesNotReturnDailyItemsForInvalidExceptedDays
      ]

    failedTests = filter (not . snd) tests
    resultMessages = map (\ft -> "FAIL: " ++ (fst ft)) failedTests

  if null resultMessages
    then pure <$> putStrLn "PASS"
    else sequenceA $ map putStrLn resultMessages


getTodosForTodayReturnsEmptyWithEmptyDoc :: (String, Bool)
getTodosForTodayReturnsEmptyWithEmptyDoc =
  ( "getTodosForToday returns empty list with empty document"
  , let
      document = Document []
      got = getTodosForToday defaultEnvironment document
      expected = []
    in
      got == expected
  )

getTodosForTodayReturnsDailyItems :: (String, Bool)
getTodosForTodayReturnsDailyItems =
  ( "getTodosForToday returns daily items"
  , let
      expected =
        [ ChecklistItem [StringLiteral "item 1"] False
        , ChecklistItem [StringLiteral "item 2"] False
        , ChecklistItem [StringLiteral "item 3"] False
        ]

      document = Document
        [ Heading 1 [StringLiteral "Daily"]
        , Checklist expected
        ]

      got = getTodosForToday defaultEnvironment document

    in
      got == expected
  )

getTodosForTodayReturnsDailyExceptItems :: (String, Bool)
getTodosForTodayReturnsDailyExceptItems =
  ( "getTodosForToday returns daily except items"
  , let
      expected =
        [ ChecklistItem [StringLiteral "item 1"] False
        , ChecklistItem [StringLiteral "item 2"] False
        , ChecklistItem [StringLiteral "item 3"] False
        ]

      document = Document
        [ Heading 1 [StringLiteral "Daily except on Monday"]
        , Checklist expected
        ]

      cfg = defaultEnvironment { today = Date 2024 06 08 }
      got = getTodosForToday cfg document

    in
      got == expected
  )

getTodosForTodayReturnsDailyExceptMultipleDaysItems :: (String, Bool)
getTodosForTodayReturnsDailyExceptMultipleDaysItems =
  ( "getTodosForToday returns daily except multiple days items"
  , let
      expected =
        [ ChecklistItem [StringLiteral "item 1"] False
        , ChecklistItem [StringLiteral "item 2"] False
        , ChecklistItem [StringLiteral "item 3"] False
        ]

      document = Document
        [ Heading 1 [StringLiteral "Daily except on Monday, Wednesday"]
        , Checklist expected
        ]

      cfg = defaultEnvironment { today = Date 2024 06 08 }
      got = getTodosForToday cfg document

    in
      got == expected
  )

getTodosForTodayDoesNotReturnDailyItemsForExceptedDays :: (String, Bool)
getTodosForTodayDoesNotReturnDailyItemsForExceptedDays =
  ( "getTodosForToday does not return items for excepted days"
  , let
      document = Document
        [ Heading 1 [StringLiteral "Daily except on Monday, Saturday"]
        , Checklist
            [ ChecklistItem [StringLiteral "item 1"] False
            , ChecklistItem [StringLiteral "item 2"] False
            , ChecklistItem [StringLiteral "item 3"] False
            ]
        ]

      cfg = defaultEnvironment { today = Date 2024 06 08 } -- Saturday
      got = getTodosForToday cfg document

      expected = []
    in
      got == expected
  )

getTodosForTodayDoesNotReturnDailyItemsForInvalidExceptedDays :: (String, Bool)
getTodosForTodayDoesNotReturnDailyItemsForInvalidExceptedDays =
  ( "getTodosForToday does not return items for invalid excepted days"
  , let
      document = Document
        [ Heading 1 [StringLiteral "Daily except on invalid, Monday"]
        , Checklist
            [ ChecklistItem [StringLiteral "item 1"] False
            , ChecklistItem [StringLiteral "item 2"] False
            , ChecklistItem [StringLiteral "item 3"] False
            ]
        ]

      cfg = defaultEnvironment { today = Date 2024 06 08 }
      got = getTodosForToday cfg document

      expected = []
    in
      got == expected
  )
