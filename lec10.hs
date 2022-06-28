-------------------------------------------------
-- Конспект лекции 10 от 19.04.2021
-------------------------------------------------

-- Для полноты некоторые типы и функции из Prelude определены в данном файле

import Prelude hiding
  (Maybe, Nothing, Just, Either, Left, Right, lookup, Monad, (>>=), return)
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import Data.List (stripPrefix)

-- Этот материал основан на "Лекциях по функциональному
-- программированию" (выложены на source.unn.ru).

-- Задача, описанная ниже, описывает доисторическую эпоху, когда все
-- телефоны были стационарными, а не мобильными. Дана база данных имен
-- и адресов, а также база данных адресов и номеров телефонов. Задача:
-- определить номер телефона по имени.

type Name = String
type Address = String
type Phone = Integer
type AddressDB = [(Name, Address)]
type PhoneDB = [(Address, Phone)]

-- Некоторые имена могут соотвествовать нескольким адресам

addressDB :: AddressDB
addressDB = [("Smith", "address1"), ("Smith", "address2"), ("Jones", "address3")]

-- Некоторые адреса могут соотвествовать нескольким номерам телефонов,
-- а некоторые адреса могут не иметь телефонов.

phoneDB :: PhoneDB
phoneDB = [("address1", 1234567), ("address2", 7654321), ("address2", 5555555)]

-- Задача: получить номер телефона по имени

-------------------------------------------------
-- Вариант 1 (с возвращаемым типом Maybe)
-------------------------------------------------

-- Из Prelude

data Maybe a  =  Nothing | Just a deriving (Eq, Ord, Show)

-- Также из Prelude: поиск в ассоциативном списке (таблице базы данных)

lookup                  :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup _key []          =  Nothing
lookup  key ((x,y):xys)
    | key == x          =  Just y
    | otherwise         =  lookup key xys

-- Композиция двух lookup

nameToPhone1 :: Name -> Maybe Phone
nameToPhone1 name =
  case (lookup name addressDB) of
  Just address -> lookup address phoneDB
  Nothing -> Nothing

-- Постараемся вынести детали композиции, имеющие дело с Maybe,
-- в отдельную функцию thenMaybe

thenMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
thenMaybe (Just x) f = f x
thenMaybe Nothing _ = Nothing

-- Тогда функцию nameToPhone1 можно упростить

nameToPhone1' :: Name -> Maybe Phone
nameToPhone1' name =
  lookup name addressDB `thenMaybe` (\address -> lookup address phoneDB)

-- Примеры
-- nameToPhone1 "Smith" (первый из двух телефонов)
-- nameToPhone1 "Jones" (нет телефона)
-- nameToPhone1 "Brown" (нет адреса)

-------------------------------------------------
-- Вариант 2 (с возвращаемым типом Either Error)
-------------------------------------------------

-- Из Prelude

data Either a b  =  Left a | Right b deriving (Eq, Ord, Show)

type Error = String

-- Делаем так, чтобы lookup возвращал сообщение об ошибке, а не просто Nothing

nameToAddress :: Name -> Either Error Address
nameToAddress name =
  case lookup name addressDB of
  Just address -> Right address
  Nothing -> Left "Address not found"

addressToPhone :: Address -> Either Error Phone
addressToPhone address =
  case lookup address phoneDB of
  Just phone -> Right phone
  Nothing -> Left "Phone not found"

-- Композиция nameToAddress и addressToPhone.
-- Возвращается сообщение о первой возникшей ошибке

nameToPhone2 :: Name -> Either Error Phone
nameToPhone2 name =
  case nameToAddress name of
  Right address -> addressToPhone address
  Left error -> Left error

-- Примеры
-- nameToPhone2 "Smith" (первый из двух телефонов)
-- nameToPhone2 "Jones" (нет телефона)
-- nameToPhone2 "Brown" (нет адреса)

-- Вынесем детали композиции, имеющие дело с Either, в отдельную функцию

thenEither :: Either e a -> (a -> Either e b) -> Either e b
thenEither (Left e) _ = Left e
thenEither (Right x) f = f x

nameToPhone2' :: Name -> Either Error Phone
nameToPhone2' name =
  nameToAddress name `thenEither` (\address -> addressToPhone address)

-------------------------------------------------
-- Вариант 3 (возвращается список всех телефонов,
-- соответствующих имени)
-------------------------------------------------

lookupList :: Eq a => a -> [(a, b)] -> [b]
lookupList key xys = [snd xy | xy <- xys, fst xy == key]

-- Композиция двух lookupList

nameToPhone3 :: Name -> [Phone]
nameToPhone3 name =
  let addresses = (lookupList name addressDB)
      phoneLists = map (`lookupList` phoneDB) addresses
  in concat phoneLists

-- Из Prelude:
-- concat :: [[a]] -> [a]
-- concatMap f = concat . map f

thenList :: [a] -> (a -> [b]) -> [b]
thenList xs f = concatMap f xs
-- Или:
-- thenList = flip concatMap

nameToPhone3' :: Name -> [Phone]
nameToPhone3' name =
  lookupList name addressDB `thenList` (\address -> lookupList address phoneDB)

-- Примеры
-- nameToPhone3 "Smith" (все телефоны)
-- nameToPhone3 "Jones" (нет телефона)
-- nameToPhone3 "Brown" (нет адреса)

-------------------------------------------------
-- Обобщение трех подходов, описанных выше
-------------------------------------------------

-- Еще раз рассмотрим типы разных версий then, а также функции,
-- использующие их.

-- thenMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- thenEither :: Either e a -> (a -> Either e b) -> Either e b
-- thenList :: [a] -> (a -> [b]) -> [b]

-- nameToPhone1' name =
--   lookup name addressDB `thenMaybe` (\address -> lookup address phoneDB)
--  
-- nameToPhone2' name =
--   nameToAddress name `thenEither` (\address -> addressToPhone address)
--  
-- nameToPhone3' name =
--   lookupList name addressDB `thenList`
--     (\address -> lookupList address phoneDB)

-- Что общего между типами thenMaybe, thenEither и thenList? В каждом
-- используется конструктор типов: Maybe, Either Error и [ ],
-- соответственно. Сами по себе это не типы: например, нельзя объявить
-- функцию с аргументом x :: Maybe. Это конструкторы типов, потому что
-- они принимают и возвращают тип. Так, для любого типа a выражения
-- Maybe a, Either Error a и [a] являются типами. В этом смысле это
-- функции из типов в типы. (На языке теории категорий такие функции
-- называются функторами.)

-- Итак, если Maybe, Either Error или [ ] обозначить через m, то типы
-- thenMaybe, thenEither и thenList имеют следующий вид.

-- then :: m a -> (a -> m b) -> m b.

-- Поэтому (then v f) похоже на применение f к v, но v является не
-- просто значением, а значением в контексте. В книге Миран Липовача
-- "Изучай Haskell во имя добра!" такие значения называются "fancy
-- values" ("причудливыми значениями"). Так, вместо простого значения
-- 1 аргумент v может быть Just 1 или Nothing. Функция then должна
-- сначала распаковать значение в контексте, достать из него обычное
-- значение и применить к нему f, а также что-то сделать с контекстом
-- (возможно, изменить его).

-- В Haskell можно объявлять не только классы типов, но и классы
-- конструкторов типов. Объявим класс конструкторов, поддерживающих
-- операцию then. Традиционно эта операция обозначается >>= и
-- используется как инфиксная связка. Конструктор типов, который
-- поддерживает операцию >>= с этип типом, называется монадой.

class Monad m where
  return :: a -> m a -- эта функция будет рассмотрена позже
  (>>=) :: m a -> (a -> m b) -> m b

instance Monad Maybe where
  return = Just
  (>>=) = thenMaybe

instance Monad (Either e) where
  return = Right
  (>>=) = thenEither

instance Monad [] where
  return x = [x]
  (>>=) = thenList

-- Теперь можно использовать одну и ту же запись >>=, а Haskell
-- выберет нужную операцию на основании типа левой части.

nameToPhone1'' :: Name -> Maybe Phone
nameToPhone1'' name =
  lookup name addressDB >>= (\address -> lookup address phoneDB)

nameToPhone2'' :: Name -> Either Error Phone
nameToPhone2'' name =
  nameToAddress name >>= (\address -> addressToPhone address)

nameToPhone3'' name =
  lookupList name addressDB >>= (\address -> lookupList address phoneDB)

-------------------------------------------------
-- Дополнительные примеры
-------------------------------------------------

-- 1. Напишите функцию sumMaybe :: [Maybe Int] -> Maybe Int. Если все
-- элементы списка имеют вид Just x, функция должна возвращать сумму
-- этих x. Если хотя бы один элемент есть Nothing, возвращается
-- Nothing. Например:

-- sumMaybe [Just 1, Just 2, Just 3] = Just 6
-- sumMaybe [Just 1, Nothing, Just 3] = Nothing

sumMaybe :: [Maybe Int] -> Maybe Int
sumMaybe [] = Just 0
sumMaybe (x : xs) = x >>= (\x' -> sumMaybe xs >>= (\s -> Just (x' + s)))

-- Скобки вокруг \x' -> ... и \xs' -> ... не обязательны.
-- Каковы типы x, x', xs и s в определении выше?

-- Функцию sumMaybe можно обобщить на любую монаду.

sumM :: Monad m => [m Int] -> m Int
sumM [] = return 0
sumM (x : xs) = x >>= (\x' -> sumM xs >>= (\xs' -> return (x' + xs')))

-- sumM [Right 1, Right 2, Right 3] = Right 6
-- sumM [Right 1, Right 2, Left "Error"] = Left "Error"

-- sumM [[1,4], [5], [10,11]] = [16,17,19,20]
-- 1 + 5 + 10 = 16
-- 1 + 5 + 11 = 17
-- 4 + 5 + 10 = 19
-- 4 + 5 + 11 = 20

-- 2. Напишите функцию threeSteps :: [(Int, Int)] -> Int -> [Int],
-- которая возвращает список вершин ориентированного графа, достижимых
-- из данной вершины за три шага. В возвращаемом списке возможны
-- повторения, если некоторые вершины достижимы более, чем одним
-- способом.
-- Первый аргумент — граф, заданный списком ребер.
-- Второй аргумент — начальная вершина.

threeSteps :: [(Int, Int)] -> Int -> [Int]
threeSteps graph v =
  step v  >>= \v1 ->
  step v1 >>= \v2 ->
  step v2
    where step u = lookupList u graph

-- 3. Напишите функцию nSteps :: [(Int, Int)] -> Int -> Int -> [Int],
-- которая возвращает список вершин ориентированного графа, достижимых
-- из данной вершины за заданное количество шагов.
-- Первый аргумент -- граф, заданный списком ребер.
-- Второй аргумент -- начальная вершина.
-- Третий аргумент -- количество шагов.

nSteps :: [(Int, Int)] -> Int -> Int -> [Int]
nSteps _ v 0 = [v]
nSteps graph v n = lookupList v graph >>= \u -> nSteps graph u (n-1) 

-------------------------------------------------
-- Домашнее задание 10
-------------------------------------------------

-- 1. Изучите примеры выше и поймите, как они работают.

-- 2. Изучите документацию по следующим функциям.
-- isJust :: Maybe a -> Bool в модуле Data.Maybe
-- readMaybe :: Read a => String -> Maybe a в модуле Text.Read
-- stripPrefix :: Eq a => [a] -> [a] -> Maybe [a] в модуле Data.List

-- 3. С помощью функции readMaybe напишите функцию
-- digitToIntMaybe :: Char -> Maybe Int
-- которая возвращает Just n, если аргумент есть символ,
-- представляющий цифру n, и Nothing в противном случае.
-- Функция пишется в одну строчку.

-- 4. Рассмотрим следующий формат строк. Строка может либо быть
-- пустой, либо начинаться с одной цифры n, за которой следуют n
-- символов 'a'. Оставшийся суффикс удовлетворяет тому же
-- формату. Например, строки
-- 3aaa2aa, 9aaaaaaaaa, 0, 001a, 2aa2aa удовлетворяют формату, а
-- 3aaa2a, 10aaaaaaaaaa, 1, 100a, 2bb2bb не удовлетворяют.

-- Напишите функцию goodString :: String -> Bool, которая проверяет,
-- удовлетворяет ли строка описанному формату. Функция должна
-- использовать монаду Maybe и быть написана в монадном стиле (то есть
-- использовать >>= вместо if). Можно также использовать функцию replicate.

-- goodString :: String -> Bool
-- goodString = isJust . go
--   where go :: String -> Maybe Bool
--         -- На самом деле вместо Bool можно написать другие типы: String, Int и т.п.
--         -- Замените undefined на нужную правую часть
--         go _ = undefined
