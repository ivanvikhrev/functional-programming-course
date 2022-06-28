import Data.Semigroup
import Data.Monoid
import Control.Monad.Writer

-- 1. Какое из выражений Haskell, приведенных ниже, соответствует
-- выражению chores(mow(lawn), repair(leaking + faucet)) на языках
-- C, Java или Python?

-- (а) chores (mow (lawn)) repair (leaking + faucet)
-- (б) chores (mow lawn) repair (leaking + faucet)
-- (в) chores (mow lawn) (repair (leaking + faucet))
-- (г) chores (mow lawn) (repair leaking + faucet)

-- Ответ: в)

-- 2. Существует ли выражение e и типы t1, t2, такие что (e :: t1) и
-- (e :: t2) возвращают разные значения? Если да, приведите пример и
-- напишите, когда определяется способ вычисления значения e: во время
-- компиляции или во время исполнения программы. Если таких значений и
-- типов не существует, объясните, почему.

-- Да, существует:
-- 3 <> 3 :: Sum Int
--      Sum {getSum = 6}
-- 3 <> 3 :: Product Int
--      Product {getProduct = 9}
-- Это выражение может вычисляться по-разному в зависимости от своего типа, способ вычисления
-- определяется во время компиляции.

-- 3. Рассмотрим терм uncurry (flip (,)).
-- (а) Найдите его тип.
-- (б) Подставьте в терм определения функций uncurry и flip из Prelude
-- и напишите цепочку редукций, которая заканчивается нормальной формой.
-- (в) Объясните, что делает функция, представленная исходным термом.

-- Напоминание: (,) :: a -> b -> (a, b) есть конструктор упорядоченных
-- пар.
-- Ответ:
  --а) uncurry (flip (,)) :: (b, a) -> (a, b)

  --б) Выпишем типы используемых функций:
  -- uncurry :: (a -> b -> c) -> (a, b) -> c
  -- flip :: (a -> b -> c) -> b -> a -> c
  -- (,) :: a -> b -> (a, b)

  -- Подстановка по шагам:
  -- 1. flip (,) :  (a -> b -> (a, b)) -> b -> a -> (a, b)
  -- это редуцируется до flip (,) : b -> a -> (a, b) - исключается тип для (,)
  -- 2. uncurry (flip (,)) : (b -> a -> (a, b)) -> (b, a) -> (a, b)
    -- далее рудуцируется часть для flip (,) : b -> a -> (a, b)
  -- 3. Получаем
            -- uncurry (flip (,)) : (b, a) -> (a, b)
      -- нормальную форму

  --в) Принимает на вход кортеж (b, a), на выходе получается перевернутый кортеж (a, b)

-- 4. Большинство студентов сделало задание 16 в Formula.hs
-- неправильно. В связи с этим рассмотрим следующую задачу. Тип
-- деревьев с произвольным коэффициентом ветвления задается так.

data Tree i a = Leaf a | Node (i -> Tree i a)

-- Тип i выступает в роли индексного множества. Поддеревья каждого
-- внутреннего узла индексируются элементами i. Так, если i = Bool,
-- то это двоичное дерево, если i задано определением
-- data I = I1 | I2 | I3
-- то это троичное дерево, а если i = Int, то коэффициент ветвления
-- равен бесконечности.

-- Объявите конструктор типов Tree i членом класса Foldable,
-- определив функцию foldMap.

instance (Bounded i, Enum i) => Foldable (Tree i) where
  foldMap f (Leaf x) = f x
  foldMap f (Node n) = foldr1 (<>) (map (foldMap f . n) [minBound..maxBound])

-- Напишите функцию fringe, которая возвращает список элементов,
-- хранящихся в листьях дерева. Функция fringe не должна явно
-- использовать рекурсию, а вместо этого должна использовать foldMap с
-- моноидом эндоморфизмов на типе списков. Обратите внимание, что это
-- не то же самое, что моноид списков.

fringe :: (Bounded i, Enum i) => Tree i a -> [a]
fringe t = appEndo (foldMap(Endo . (\x y->[x] ++ y)) t) []

-- Объявим бинарное дерево
type BinaryTree = Tree Bool Int

insert :: BinaryTree -> BinaryTree -> Bool -> BinaryTree
insert left right idx
            |idx == True = left
            |otherwise = right

tree = Node (insert(Leaf 1) (Leaf 2))
tree2 = Node (insert (Node (insert (Leaf 1) (Leaf 2))) (Node (insert (Leaf 3) (Leaf 4))) )

-- 5. Почти все студенты неправильно поняли определение монотонной
-- булевой функции и соответственно неправильно написали функцию
-- monotone в проекте. В связи с этим напишите аналогичную функцию,
-- которая принимает булеву функцию f от n аргументов и проверяет,
-- является ли она монотонной. Определение монотонной функции и
-- алгоритм проверки на монотонность см. в книге:
-- Гаврилов Г.П., Сапоженко А.А. Задачи и упражнения по дискретной
-- математике. М.: Физматлит, 2005
-- на с. 75, а определение используемого при этом частичного порядка на
-- наборах аргументов — на с. 10.

allEnvs:: Int -> [[Bool]]
allEnvs 0 = [[]]
allEnvs n = [x:xs | x <- [False, True], xs <- allEnvs (n-1)]

compareXY :: ([Bool], [Bool]) -> Bool
compareXY ([], []) = True
compareXY (x:xs, y:ys) =
    not (x && not y ) && compareXY (xs, ys)

monotone' :: [Bool] -> Bool
monotone' [x] = True
monotone' lst =
    let halves = splitAt (length lst `div` 2) lst
    in compareXY halves && monotone' (fst halves) && monotone' (snd halves)

monotone :: Int -> ([Bool] -> Bool) -> Bool
monotone n_args func =
    let values = map func (allEnvs n_args)
    in monotone' values

-- 6. Напишите функцию

avg :: [Int] -> Float
avg lst =
  let num_and_sum = snd (runWriter (numSum lst))
      num = getSum (fst num_and_sum )
      sum = getSum (snd num_and_sum )
  in fromIntegral sum / fromIntegral num

-- которая возвращает среднее арифметическое списка. Эта функция
-- должна использовать функцию
-- numSum :: [Int] -> Writer (..., ...) ()

numSum :: [Int] -> Writer (Sum Int, Sum Int) ()
numSum [] = writer (mempty , (0, 0))
numSum (x:xs) = do
  numSum xs
  tell (1, Sum x)
  return mempty

-- с некоторыми типами вместо многоточий, которая вычисляет количество
-- и сумму элементов в списке. Функции numSum и avg должны быть
-- аналогичны одноименным методам в следующей программе на Java, за
-- исключением того, что прохождение списка должно, как обычно,
-- использовать рекурсию, а не цикл for.

-- public class Avg {
--   private int s;
--   private int n;
--
--   private void update(int x) { s += x; n++; }
--
--   private void numSum(int[] a) {
--     for (int i = 0; i < a.length; i++)
--       update(a[i]);
--   }
--
--   public float average(int[] a) {
--     s = 0; n = 0;
--     numSum(a);
--     return (float)s/n;
--   }
--
--   public static void main(String[] args) {
--     Avg avg = new Avg();
--     int[] a = {1, 3, 2, 6, 4, 5};
--     System.out.println("The average is " + avg.average(a));
--   }
-- }

-- Другой способ сделать avg с монадой writer :
avg2 :: [Int] -> Float
avg2 lst =
  let sum_num = runWriter (numSum2 lst)
      sum = fst sum_num
      num = getSum (snd sum_num)
  in fromIntegral sum / fromIntegral num

numSum2 :: [Int] -> Writer (Sum Int) Int
numSum2 [] = return 0
numSum2 (x:xs) = do
  t <- numSum2 xs
  tell 1
  return (x + t)

-- 7. Напишите функцию allSums, которая возвращает суммы всех
-- подпоследовательностей списка-аргумента. Элементы
-- подпоследовательности могут быть разделены в исходном списке.
-- Порядок сумм неважен, и некоторые суммы могут совпадать. Например,

-- > allSums []
-- [0]
-- > allSums [2]
-- [2,0]
-- > allSums [1,3,4]
-- [8,4,5,1,7,3,4,0]

-- Используйте монаду списков и do-нотацию. Может быть полезным
-- использовать список [True, False]. Библиотечные функции (кроме (+)
-- и return) в основном решении использовать нельзя, но можно написать
-- альтернативные решения с функциями из Control.Monad.

allSubsets :: (Monad m) => (m Bool) -> [Int] -> m [Int]
allSubsets _ [] = return []
allSubsets p (x:xs) =  do
                    flg <- p
                    ys  <- allSubsets p xs
                    return (if flg then x:ys else ys)

allSums :: [Int] -> [Int]
allSums xs = [subsetSum s| s <- all_subsets]
            where all_subsets = allSubsets [True, False] xs
                  subsetSum :: [Int] -> Int
                  subsetSum [] = 0
                  subsetSum (x:xs) = x + subsetSum xs

--Решение с использованием библиотечных функций:
allSums2 xs = map sum $ filterM(const [True, False]) xs