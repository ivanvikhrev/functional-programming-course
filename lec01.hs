-- Домашнее задание 1

-- 1. Установите Haskell Platform по ссылке ниже.

-- 2. Прочитайте конспект лекции и определите, что делают описанные
-- функции (take, replicate и т.д.), опираясь на их имя, тип и примеры
-- использования. Можно также смотреть документацию по следующим
-- ссылкам.
-- http://haskell.ru/standard-prelude.html
-- https://www.stackage.org/haddock/lts-17.4/base-4.14.1.0/Prelude.html

-- 3. В Haskell есть две функции для целочисленного деления: quot и
-- div, а также две функции для взятия остатка: rem и mod. Как
-- отличается их поведение, когда один из аргументов является
-- отрицательным числом?

-- 4. Напишите функцию типа Float -> Float -> Float -> Float, которая
-- вычисляет наименьший из корней уравнения |x - a| + |x - b| = c,
-- если он существует.

-- 5. Напишите функцию Integer -> Integer вычисляющую произведение
-- последних трех десятичных цифр числа.

-- 6. Напишите функцию Integer -> Char, которая переводит цифру в символ.
-- Функция должна вызывать error, если аргумент не из отрезка [0..9].

-------------------------------------------------
-- Конспект лекции 1 от 15.02.2021
-------------------------------------------------

-- Установка Haskell Platform: https://www.haskell.org/downloads/#platform
-- На Windows нужно установить сначала менеджер пакетов Chocolatey.
-- По ссылке приведены конкретные команды для PowerShell.

-- Интерпретатор называется ghci. С ним можно работать из командной
-- строки, а программу писать в любом текстовом редакторе. Однако
-- удобнее использовать оболочку WinGHCI
-- (https://github.com/haskell/winghci). Можно также использовать VS Code
-- с расширением Haskell Syntax Highlighting. В нем можно запустить
-- командную строку, а в ней ghci.

-- Интерпретатор работает в режиме REPL (read, evaluate,
-- print, loop)
-- :? информация
-- :cd /path/to/files изменить рабочий
-- каталог (каталог без кавычек)
-- :show paths
-- :! dir исполнить команду
-- shell
-- :load file.hs,
-- :l file.hs загрузить файл (имя файла без
-- кавычек)
-- :r повторно загрузить последний загруженный файл
-- :list
-- fact напечатать определение функции из загруженного файла

-- Проверьте, что выдают следующие примеры в командной строке.

-- Основные типы данных и их значения
-- 5
-- True
-- 'a'
-- 5 + 6
-- 5 :: Integer -- произвольная длина
-- 5 :: Int -- [-2^29 .. 2^29-1]
-- 3.14 :: Float -- одинарная точность
-- 2.718281828459045 :: Double -- двойная точность
-- 3 % 7 :: Rational -- 3/7, требует модуль Data.Ratio
-- True :: Bool
-- 'a' :: Char
-- :t (+) напечатать тип выражения
-- :set +t включить режим печатания типа
-- :unset +t включить режим печатания типа
-- :info (+), :i (+) напечатать информацию об идентификаторе
-- Арифметические операции
-- (+), (-), (*), (/), div, mod, (^), 
-- sum, product, max, min, maximum, minimum, even, odd, gcd, lcm
-- Предикаты и логические операции
-- (>), (<), (==), (/=), (>=), (<=), (&&), (||), not, and, or
-- Кортеж (упорядоченная последовательность): (1, True, 'a')
-- Функции, возвращающие первый и второй элементы упорядоченной пары: fst, snd
-- Список: [1, 2, 3, 4, 5]
-- Все кортежи одного типа имеют фиксированную длину, но могут содержать элементы разных типов
-- Списки одного типа могут иметь разную длину, но содержат элементы одного типа
-- Список списков: [[1, 2], [], [3, 1]]
-- Строка есть не отдельный тип, а список символов: "hello" == ['h', 'e', 'l', 'l', 'o']
-- Полиморфизм: многие функции на списках принимает списки элементов любого типа
-- length [1, 2, 3, 4]
-- head [3, 2, 1]
-- head "hello"
-- tail [3, 2, 1]
-- Пустой список: []
-- Проверка списка на пустоту: null
-- (:) конструирование списка из головы и хвоста
-- Другие функции на списках
-- last, init, length, (!!), (++), concat, take, drop, reverse, elem, replicate
-- см. тип и примеры или см. описание в Prelude

-- Дополнительные функции на списках: добавить в начале файла
-- import Data.List
-- Эквивалентно: в командной строке
-- :m + Data.List
-- Чтобы выгрузить модуль
-- :m - Data.List

-- Запись f :: a1 -> ... -> an -> b
-- означает, что функция f принимает n аргументов типов a1, ..., an и возвращает
-- значение типа b

-- Аппликация (применение) функции к аргументам записывается
-- f e1 ... en. Функция и аргументы отделяются пробелами.
-- Скобки следует использовать только там, где необходимо.
-- Аппликация имеет самый большой приоритет, поэтому f 3 + 4 интерпретируется как
-- (f 3) + 4. Если f применяется к 3 + 4, то нужно писать f (3 + 4).

fact :: Integer -> Integer
fact 0 = 1
fact x = x * fact (x-1)

-- Ограничения (guards)
sign :: Double -> Double
sign x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

-- Локальные определения
-- Решение квадратного уравнения. Возвращает список из 0 или 2 корней
solve :: Double -> Double -> Double -> [Double]
solve a b c
  | d < 0 = []
  | otherwise = [(-b + sqrt d)/2/a, (-b - sqrt d)/2/a]
    where
      d = b * b - 4 * a * c

-- Другой вариант локального определения
-- Вместо ограничений используется if
-- Важно: if возвращает значение, а не вызывается ради побочного эффекта
solve1 :: Double -> Double -> Double -> [Double]
solve1 a b c =
  let d = b * b - 4 * a * c in
    if d < 0 then [] else [(-b + sqrt d)/2/a, (-b - sqrt d)/2/a]

-- В строке интерпретатора (в отличие от текста программы)
-- команды, занимающие несколько строк следует заключать в
-- :{ ... :}, а определение функций начинать с let.
-- Это используется редко. Например:
-- :{
-- let fact 0 = 1
--     fact x = x * fact (x - 1)
-- :}

-- В качестве аргументов функции можно указывать не только переменные,
-- но и образцы (patterns).
-- Они определяются рекурсивно:
-- []
-- pat1 : pat2
-- [pat1, pat2 ... , patn]
-- (pat1, pat2 ... , patn)
-- идентификатор (переменная)
-- _ (если аргумент не используется в правой части)
-- константа

safediv :: Rational -> Rational -> Rational
safediv _ 0 = 0
safediv x y = x / y

-- Функция с одним аргументом, являющемся кортежем из двух элементов
plus :: (Int, Int) -> Int
plus (x, y) = x + y
-- Менее идиоматично
-- plus z = (fst z) + (snd z)

-- Функция от двух аргументов, являющихся числами
plus1 :: Int -> Int -> Int
plus1 x y = plus (x, y)

-- Для кортежей длины 3 нет стандартных функций
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- Бинарный оператор, заключенный в скобки, является обычной функцией
-- двух аргументов и записывается перед своими аргументами
-- (+) 1 2 == 1 + 2
-- Обычная функция двух аргументов, заключенная в обратные апострофы,
-- является бинарным оператором и записывается между аргументами
-- 7 `mod` 2 == mod 7 2

-- Частично примененная функция
-- Функция, примененная к части своих аргументов, возвращает функцию от остальных аргументов
inc :: Int -> Int
inc = (+) 1

-- Сечения
-- Бинарному оператору можно дать один из аргументов. Возвращается функция от
-- оставшегося аргумента
-- (2^) == (^) 2
-- (2^) 3 == 2^3
-- (^2) нельзя представить в виде применения (^), поскольку отсутствует первый аргумент
-- (^2) 3 == 3^2
-- Исключение: (-2) не функция, которая вычитает 2, а число -2
-- (subtract 2) -- функция, вычитающая 2
