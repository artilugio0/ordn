module Ordn.Markdown
  where

import Ordn.Document

parse :: String -> Maybe Document
parse = fst . runParser documentParser

newtype Parser a = Parser
  { runParser :: String -> (Maybe a, String)
  }

instance Semigroup a => Semigroup (Parser a) where
  (<>) (Parser p1) (Parser p2) = Parser (
    \s ->
      case p1 s of
        (Nothing, _) -> (Nothing, s)
        (Just r1, firstRest) -> case p2 firstRest of
            (Nothing, _) -> (Nothing, s)
            (Just r2, secondRest) -> (Just (r1 <> r2), secondRest))

instance Monoid a => Monoid (Parser a) where
  mempty = Parser (\s -> (Just mempty, s))
  mappend = (<>)

instance Functor Parser where
  fmap f (Parser p) = Parser (\s ->
    let (result, rest) = p s
    in (fmap f result, rest))

instance Applicative Parser where
  pure a = Parser (\s -> (Just a, s))

  (Parser f) <*> p = Parser (\s ->
    case f s of
      (Just a, rest) ->
        case runParser (a <$> p) rest of
          (Nothing, _) -> (Nothing, s)
          x -> x
      (Nothing, _) -> (Nothing,s ))


charParser :: Char -> Parser String
charParser c = charConditionParser (== c)


charConditionParser :: (Char -> Bool) -> Parser String
charConditionParser shouldInclude = Parser (\s ->
  case s of
    []                         -> (Nothing, "")
    (c:rest) | shouldInclude c -> (Just [c], rest)
    _                          -> (Nothing, s))


alternativeParser :: Parser a -> Parser a -> Parser a
alternativeParser (Parser p1) (Parser p2) = Parser (\s ->
  case p1 s of
    (Just x, rest) -> (Just x, rest)
    (Nothing, _) -> p2 s)


discard :: Monoid a => Parser a -> Parser a
discard = fmap (const mempty)


unconsume :: Monoid a => Parser a -> Parser a
unconsume (Parser p) = Parser (\s ->
  case p s of
    (Nothing, _) -> (Nothing, s)
    _ -> (Just mempty, s))


optional :: Monoid a => Parser a -> Parser a
optional (Parser p) = Parser (\s ->
  case p s of
    (Nothing, _) -> (Just mempty, s)
    x -> x)


whileParser :: (Char -> Bool) -> Parser String
whileParser shouldInclude =
  charConditionParser shouldInclude
  <> alternativeParser (whileParser shouldInclude) mempty


variableNameParser :: Parser String
variableNameParser = whileParser (\c -> elem c $ ['a'..'z'] ++ "_")


variableParser :: Parser StringValueAtom
variableParser =
  Variable
    <$> discard (charParser '{')
      <> discard (charParser '{')
      <> variableNameParser
      <> discard (charParser '}')
      <> discard (charParser '}')


stringValueParser :: Parser StringValue
stringValueParser =
  alternativeParser
    ((pure <$> variableParser) <> (optional stringValueParser))

    (alternativeParser
      (((\s -> [StringLiteral s]) <$> whileParser (\c -> c /= '\n' && c /= '{'))
        <> (optional stringValueParser))

      ((\s -> [StringLiteral s]) <$> whileParser (/= '\n')))


multiLineStringValueParser :: Parser StringValue
multiLineStringValueParser =
  stringValueParser
  <> (optional (((\s -> [StringLiteral s]) <$> (discard $ charParser '\n'))
                <> (unconsume ((\s -> [StringLiteral s]) <$>
                                charConditionParser (\c -> (not . elem c) "#-\n")))
                <> stringValueParser))


paragraphParser :: Parser Element
paragraphParser = Paragraph <$> multiLineStringValueParser


headingParser :: Parser Element
headingParser = Parser (\s ->
  case runParser (length <$> prefixParser) s of
    (Just n, rest) -> runParser ((Heading n) <$> stringValueParser) rest
    (Nothing, _) -> (Nothing, s))
  where
    prefixParser = (whileParser (== '#') <> (optional . discard $ charParser ' '))


checklistItemStateParser :: Parser Bool
checklistItemStateParser =
  (== "x")
  <$> (discard $ charParser '[')
      <> (alternativeParser (charParser ' ') (charParser 'x'))
      <> (discard $ charParser ']')


checklistItemParser :: Parser ChecklistItem
checklistItemParser =
  (\_ s _ v -> ChecklistItem v s)
  <$> ((discard $ charParser '-') <> (discard $ charParser ' '))
  <*> checklistItemStateParser
  <*> (discard $ charParser ' ')
  <*> stringValueParser


checklistParser :: Parser Element
checklistParser = Parser (\s ->
  case runParser checklistItemParser s of
    (Just i, rest) ->
      case runParser (charParser '\n') rest of
        (Just _, rest2) ->
          case runParser checklistParser rest2 of
            (Just (Checklist items), rest3) -> (Just (Checklist (i:items)), rest3)
            _ -> (Just (Checklist [i]), rest)
        (Nothing, _) -> (Just (Checklist [i]), rest)
    (Nothing, _) -> (Nothing, s))


emptyParser :: a -> Parser a
emptyParser d = Parser (\s ->
  if null s
    then (Just d, s)
    else (Nothing, s))


ignoreEmptyLines :: Parser a -> Parser a
ignoreEmptyLines p =
  (\_ x -> x)
  <$> optional (whileParser (== '\n'))
  <*> p


elementsListParser :: Parser [Element]
elementsListParser =
  (:)
  <$> (alternativeParser
        (ignoreEmptyLines headingParser)
        (alternativeParser
          (ignoreEmptyLines checklistParser)
          (ignoreEmptyLines paragraphParser)))

  <*> (alternativeParser
        (ignoreEmptyLines elementsListParser)
        (ignoreEmptyLines (emptyParser [])))


documentParser :: Parser Document
documentParser = Document <$> elementsListParser
