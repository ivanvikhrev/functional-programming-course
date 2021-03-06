-- Домашнее задание 2

-- 0. Изучите конспект лекции ниже.

-- 1. Напишите функцию, вычисляющую n-ое число Фибоначчи. Используйте
-- стандартное математическое рекурсивное определение и тип Integer,
-- позволяющий работать с целыми числами произвольной величины.
-- Экспериментальным путем найдите n, для которого вычисление занимает
-- больше 10 секунд. Напишите другую реализацию функции с двумя
-- дополнительными аргументами и хвостовой рекурсией. Вычислите 2n-ное
-- число Фибоначчи, где n — номер, найденный выше.

-- 2. Напишите функцию merge :: [Int] -> [Int] -> [Int], которая сливает
-- два упорядоченных списка в один. Например,

-- > merge [2, 5, 6] [1, 3, 4]
-- [1, 2, 3, 4, 5, 6]

-- 3. Напишите функцию halve :: [a] -> [([a], [a])], которая разбивает
-- данный список на две половины, длины которых отличаются не более,
-- чем на единицу. В определении можно использовать стандартные
-- функции. См. особенно функции take, drop и splitAt в описании Prelude.

-- 4. Используя merge и halve, напишите функцию msort :: [Int] -> [Int],
-- реализующую сортировку слиянием. Она разбивает список пополам,
-- рекурсивно сортирует половины и затем сливает в один список. Когда
-- уместно, используйте синтаксические конструкции языка: образцы,
-- ограничения, локальные определения и т.д.

-------------------------------------------------
-- Конспект лекции 2 вместо занятия 22.02.2021
-------------------------------------------------

-- Если нужно переопределить функции из Prelude,
-- поместите следующую строчку в начале файла

import Prelude hiding ((^), (^^), even, odd)

-- Теперь можно переопределить функцию из Prelude

even n = mod n 2 == 0

-- Образцы можно использовать не только при определении функции, но и в let
-- let (x : xs, 3, y) = ([1, 2, 3], 3, "abc")
-- Чтобы использовать сопоставление с образцом в любом месте,
-- а не только в левой части определения функции или в let и where,
-- нужна конструкция case (см. ниже)

-- Как написать head, tail?
-- Штрих (апостроф) можно использовать в идентификаторах

head' (x : xs) = x
tail' (x : xs) = xs

-- Рекурсия
-- Эта тема, как и другие темы из первой части курса,
-- хорошо описана в книге Г.А. Макеева на с. 26-41.
-- Ссылка выложена на source.unn.ru.

-- Следующая функция увеличивает каждый элемент списка на 1.
-- Есть несколько стилей определения функции.

-- Наиболее идиоматичное определение

inclist [] = []
inclist (x : xs) = x + 1 : inclist xs

inclist1 [] = []
inclist1 xs = head xs + 1 : inclist1 (tail xs)

inclist2 xs = if null xs then [] else (head xs) + 1 : inclist2 (tail xs)

inclist3 l =
  case l of
  [] -> []
  (x : xs) -> x + 1 : inclist3 xs

lngth [] = 0
lngth (_ : xs) = 1 + lngth xs

product' [] = 1
product' (x : xs) = x * product' xs

-- Обращение списка
-- Нехвостовая рекурсия (результат рекурсивного вызова не является
-- окончательным значением функции), квадратичная временная сложность
-- (почему?)

reverse' [] = []
reverse' (x : xs) = reverse xs ++ [x]

addToEnd :: a -> [a] -> [a]
addToEnd x [] = [x]
addToEnd x (y : ys) = y : addToEnd x ys

-- Можно определить
-- addToEnd x xs = xs ++ [x]

reverse'' [] = []
reverse'' (x : xs) = addToEnd x (reverse'' xs)

-- Хвостовая рекурсия (результат рекурсивного вызова является
-- окончательным значением функции), линейная сложность
-- reverse2 l1 l2 добавляет обращение l1 слева к l2

reverse2 :: [a] -> [a] -> [a]
reverse2 [] ys = ys
reverse2 (x : xs) ys = reverse2 xs (x : ys)

-- Трассировка. Будем писать r вместо reverse2
-- r [1, 2, 3] [4, 5, 6] =
-- r [2, 3] (1 : [4, 5, 6]) =
-- r [3] [2, 1, 4, 5, 6] =
-- r [] [3, 2, 1, 4, 5, 6] =
-- [3, 2, 1, 4, 5, 6]

rev xs = reverse2 xs []

-- Определения функций foo1 и foo2 ниже выглядят похоже,
-- но есть большая разница в поведении и в сложности.

-- foo1 есть append (конкатенация списков, или ++)

foo1 [] acc = acc
foo1 (x : xs) acc = x : foo1 xs acc

-- foo1 [1, 2, 3] [4, 5, 6] =
-- 1 : foo1 [2, 3] [4, 5, 6] =
-- 1 : 2 : foo1 [3] [4, 5, 6] =
-- 1 : 2 : 3 : foo1 [] [4, 5, 6] =
-- 1 : 2 : 3 : [4, 5, 6] =
-- [1, 2, 3, 4, 5, 6]

-- Контекст (1 : 2 : 3 : _), то есть операция, которую необходимо
-- применить к результату рекурсивного вызова, в терминологии
-- функционального программирования называется продолжением
-- (continuation). Продолжение хранится на стеке, который может
-- переполниться при особенно глубокой рекурсии.

-- foo2 есть reverse

foo2 [] acc = acc
foo2 (x : xs) acc = foo2 xs (x : acc)

-- факториал с нехвостовой рекурсией

fact 0 = 1
fact n = n * fact (n - 1)

-- факториал с хвостовой рекурсией

fact1 0 m = m
fact1 n m = fact1 (n - 1) (n * m)

-- fact1 3 5 =
-- fact1 2 15 =
-- fact1 1 30 =
-- fact1 0 30 =
-- 30 = 3! * 5
-- fact1 n m = n! * m

fact2 n = fact1 n 1

-- Функции с нехвостовой рекурсией часто имеют более естественное
-- определение, но функции с хвостовой рекурсией более эффективны,
-- так как эквивалентны циклам. Преобразование функций первого
-- класса во второй является интересной задачей. Это можно сделать
-- всегда, но не всегда можно достичь экономии памяти, поскольку
-- информацию о рекурсивных вызовах не стеке приходится хранить
-- явным образом.
