module Formula where

import Data.List
import Data.Semigroup
import BooleanSyntax
  (Op, AssocType(FA, LA, RA, NA), Domain, arity, prec, noOp, opText, assoc, evalOp)

-- Ограничения на импорт описаны в лекции 3. Данный модуль
-- предназначен для работы с функциями любой сигнатуры, то есть с
-- любым набором связок, а не только булевыми формулами. Это
-- достигается тем, что из модуля BooleanSyntax импортируется сам тип
-- Op, но не его конструкторы T, F, Neg, And и т.д. Чтобы
-- импортировались также конструкторы Op, нужно добавить их в скобках
-- после типа, как в случае AssocType, или вообще убрать ограничение
-- на импорт.

-- Пока ограничения временно закомментированны, но поскольку этот
-- модуль предназначен для формул любой сигнатуры, его функции должны
-- работать с ограничениями на импорт из BooleanSyntax.

-- C означает "compound", т.е. "составная".

data Formula a = V a | C Op [Formula a]

-- Тип a в Formula a есть тип имен переменных. Это может быть,
-- например, Char, String или Int.

-------------------------------------------------
-- 1. Примеры формул
-------------------------------------------------

-- Функции ниже, которые отображают формулы в строки, предполагают,
-- что тип a является элементом класса Show. Таким образом, Haskell
-- знает, как печатать имена переменных. Однако show, примененная к
-- символам или строчкам возвращает строки с одинарными или двойными
-- кавычками. Например, show 'c' возвращает "'c'", а show "abc" —
-- "\"abc\"" (строка из 5 символов). Вообще show x обычно возвращает
-- такую строку, что при вводе ее в интерпретаторе снова получается x.

-- Чтобы формула печаталась без кавычек, создадим новый тип и
-- определим его членство в Show по-другому.

newtype Var = Var { varName :: Char }

instance Show Var where
  show (Var c) = [c]

-- Примеры формул ниже принимаются интерпретатором, если не
-- ограничивать импорт из модуля BooleanSyntax.

--form1 = x v y -> ~z
-- form1 :: Formula Var
-- form1 = C If [C Or [V (Var 'x'), V (Var 'y')], C Neg [V (Var 'z')]]
-- -- form2 = ~z
-- form2 :: Formula Var
-- form2 = C Neg [V (Var 'z')]
-- -- form3 = z
-- form3 :: Formula Var
-- form3 = V (Var 'z')
-- -- form4 = ~(x v y)
-- form4 :: Formula Var
-- form4 = C Neg [C Or [V (Var 'x'), V (Var 'y')]]
-- -- form5 = x y + z <-> x v z
-- form5 :: Formula Var
-- form5 = C Iff [C Xor [C And [V (Var 'x'), V (Var 'y')], C Neg [V (Var 'z')]],
--                C Or [V (Var 'x'), V (Var 'z')]]
-- form6 :: Formula Var
-- form6 =  C And [V (Var 'x'), V (Var 'y')]
-- form7 :: Formula Var
-- form7 =  C Or [V (Var 'x'), V (Var 'y')]


-- Задание 1. Напишите функцию correctArity, которая проверяет, что
-- арность каждого оператора, объявленная в модуле BooleanSyntax,
-- совпадает с действительным количеством его аргументов в формуле.

-- Обратите внимание, что correctArity не следует включать в другие
-- функции, описанные ниже, тем более в рекурсивные. Эти другие функции
-- работают в предположении, что формула составлена правильно.

correctArity :: Formula a -> Bool
correctArity (V var) = True
correctArity (C op formula)
  | arity op == length formula = all ((True ==) . correctArity) formula
  | otherwise = False
-------------------------------------------------
-- 2. Текстовое представление формул
-------------------------------------------------

-- Вспомогательная функция, которую можно вызывать в функциях
-- fullParen и showFormula ниже, если встречаются операции с арностью,
-- отличной от 0, 1 или 2.

arityError = error "Arity other than 0, 1 or 2"

-- Задание 2. Напишите функцию fullParen, которая возвращает текстовое
-- представление формулы, где каждая составная подформула с
-- положительным числом аргументов окружена скобками. Переменные и
-- константы (то есть нульарные функции) окружать скобками не нужно.

fullParen :: Show a => Formula a -> ShowS
fullParen (V var) = shows var
fullParen (C op []) = opText op
fullParen (C op formula) =
  showChar '(' . inside_formula . showChar ')'
  where [f] = formula
        [f1, f2] = formula
        inside_formula  = if (length formula == 1) then opText op . fullParen f
                                                   else fullParen f1 . opText op . fullParen f2 

-- Вариант, учитывающий приоритет и ассоциативность операций

-- Скобки вокруг констант (операций арности 0) не ставятся.
-- Операции арности 1 являются префиксными или отображаются
-- специальным образом. Например, C Neg [f] отображается как ~f
-- в тексте и \overline{f} в LaTeX.

-- Инфиксные операции

-- Пусть данная формула (второй аргумент функции ниже) является левым
-- аргументом операции opExt, а главная операция формулы есть opInt.
-- Скобки вокруг формулы ставятся тогда и только тогда, когда
-- 1) приоритет opExt строго больше приоритета opInt, или
-- 2) приоритет opExt равен приоритету opInt и
-- 2а) opExt <> opInt, или
-- 2б) opExt = opInt имеет ассоциативность RA или NA.

-- Если данная формула является правым аргументом opExt, то в пункте 2б)
-- нужно заменить RA на LA.

-- Задание 3. Напишите функцию showFormula, которая возвращает текстовое
-- представление формулы, где вставляются только необходимые скобки
-- согласно описанию выше.
-- Первый аргумент: оператор, находящийся непосредственно снаружи формулы
--   (внешний оператор)
-- Второй аргумент: является ли формула левым (True) или правым (False)
--   аргументом внешнего оператора
-- Третий аргумент: формула, которую нужно напечатать

placeBracket :: Op -> Op -> Bool -> Char -> ShowS
placeBracket opExt opInt side bracket
  | (prec opExt > prec opInt) || (prec opExt == prec opInt && (not_eq || eq_with_right_assoc)) = showChar bracket
  | otherwise = showString ""
  where not_eq = opExt /= opInt
        eq_with_right_assoc = opExt == opInt && (assoc opExt == NA || assoc opExt == if side then RA else LA)


showFormula :: Show a => Op -> Bool -> Formula a -> ShowS
showFormula _ _ (V var) = shows var
showFormula _ _ (C op []) = opText op
showFormula opExt side (C opInt formula) =
  left_br . inside_formula . right_br
  where [f] = formula
        [f1, f2] = formula
        inside_formula  = if (length formula == 1) then opText opInt . showFormula opInt False f 
                                                  else showFormula opInt True f1 . opText opInt . showFormula opInt False f2
        left_br = placeBracket opExt opInt side '('
        right_br = placeBracket opExt opInt side ')'
  
-- После написания fullParen или showFormula раскоментируйте соответствующий
-- вариант объявления членства типа Formula в классе Show

instance Show a => Show (Formula a) where
  --show f = fullParen f ""
  show f = showFormula noOp True f ""

-- Например, примеры формул form1 и form2 выше должны печататься так,
-- как они записаны в комментариях перед определением.

-------------------------------------------------
-- 3. Значение формулы типа Formula Int
-------------------------------------------------

-- Значения переменных берутся из окружения (environment). Окружение
-- можно рассматривать как набор значений аргументов в одной строке
-- табличного определения функции (таблицы истинности в случае
-- двузначной логики).

-- Если f :: Formula Int, то переменные кодируются целыми числами. В
-- этом случае значения переменной с номером i (i >= 0) есть i-й
-- элемент окружения.

type Environment = [Domain]

-- Задание 4. Напишите функцию lookupVar, возвращающую значение переменной
-- с данным номером в окружении.

lookupVar :: Environment -> Int -> Domain
lookupVar = (!!)

-- Задание 5. Напишите функцию eval, возвращающую значение формулы
-- типа Formula Int в окружении. Значение операций определяются функцией
-- evalOp, определенной в модуле BooleanSyntax.

eval :: Environment -> Formula Int -> Domain
eval env (V var) = lookupVar env var
eval env (C op formula) = evalOp op $ map (eval env) formula

-------------------------------------------------
-- 4. Компиляция Formula a в Formula Int
-------------------------------------------------

-- Чтобы получить значения формулы на всех возможных окружениях,
-- поступим следующим образом.
-- 1. Найдем список всех переменных, встречающихся в формуле. Каждая
-- переменная должна входить в список по одному разу. Обозначим длину
-- этого списка через n.
-- 2. Преобразуем формулу в Formula Int, заменив каждую переменную на
-- ее индекс в списке из п. 1.
-- 3. Составим список всех окружений длины n.
-- 4. Вычислим значение формулы в каждом окружении с помощью функции
-- eval.

-- Задание 6. Напишите функцию collectVars1, которая возвращает список
-- переменных, входящих в формулу. Каждая переменная входит в список
-- не более одного раза. Можно использовать функцию nub из Data.List.

-- нужно определить для тестирования функций
instance Eq Var where
    x == y = varName x == varName y

collectVars1 :: Eq a => Formula a -> [a]
collectVars1 (V var) = [var]
collectVars1 (C op formula) = nub $ concatMap collectVars1 formula

-- Задание 7. Напишите функцию varsToInt, которая принимает список
-- переменных и формулу и возвращает формулу типа Formula Int, где
-- каждая переменная заменена на ее индекс в списке. Если переменной
-- из формулы нет в списке, нужно вызывать функцию error с сообщением
-- "varsToInt: Variable occurs in the formula but not in the list".
-- Можно использовать функции из Data.List для поиска в списке.

lst = [Var 'x', Var 'y', Var 'z']

varsToInt :: Eq a => [a] -> Formula a -> Formula Int
varsToInt varList (V var) = 
    case elemIndex var varList of
       Just i  -> V i
       Nothing -> error "varsToInt: Variable occurs in the formula but not in the list"
varsToInt varList (C op formula) = C op $  map (varsToInt varList) formula

-- Задание 8. Напишите функцию compileFormula с аргументом f :: Formula a.
-- Пусть vars есть список всех переменных f без повторений. Тогда
-- compileFormula возвращает пару (length vars, varsToInt vars f).

compileFormula :: Eq a => Formula a -> (Int, Formula Int)
compileFormula formula = (length vars, varsToInt vars formula)
    where vars = collectVars1 formula

-------------------------------------------------
-- 5. Значения формулы на всевозможных окружениях
-------------------------------------------------

-- Задание 9. В предположении, что тип Domain является членом классов
-- Enum и Bounded, определите список domain всех элементов типа
-- Domain. Следует использовать синтаксис [x..y] для определения
-- последовательностей. Может оказаться полезным описание классов
-- Enum и Bounded в Prelude.

domain :: [Domain]
domain = [ minBound..maxBound ]

-- Задание 10. Напишите функцию allEnvs, которая принимает число n
-- и возвращает список всех окружений длины n в лексикографическом
-- порядке. Порядок на компонентах окружения определяется списком
-- domain.

allEnvs :: Int -> [Environment]
allEnvs 0 = [[]]
allEnvs n = [x:xs | x <- domain, xs <- allEnvs (n-1)]

-- Задание 11. Напишите функцию formulaValues, которая возвращает
-- значения формулы на всех наборах аргументов. В случае двузначной
-- логики это последний столбец таблицы истинности.

formulaValues :: Eq a => Formula a -> [Domain]
formulaValues formula = 
  let (n, int_formula) = compileFormula formula
  in [eval env int_formula | env <- allEnvs n]

-- Задание 12. Напишите функцию isConstant c f, которая определяет, является
-- ли формула f константой, принимающей значение c на всех окружениях

isConstant :: Eq a => Domain -> Formula a -> Bool
isConstant c formula = all ( == c ) $ formulaValues formula

-------------------------------------------------
-- 6. Варианты collectVars
-------------------------------------------------

-- Задание 13. Напишите функцию collectVars2, аналогичную
-- collectVars1, но использующую списочную монаду. Список уже
-- определен в Prelude как член класса Monad, поэтому можно
-- использовать функции return и >>=.

collectVars2 :: Eq a => Formula a -> [a]
collectVars2 = nub . collectVars2'
  where collectVars2' (V var) = return var
        collectVars2' (C op formula) = do 
          f <- formula 
          collectVars2' f


-- Задание 14. Разностные списки описаны в лекции 8. Напишите функцию
-- collectVars3, аналогичную collectVars1, но использующую разностные
-- списки.

collectVars3 :: Eq a => Formula a -> [a]
collectVars3 formula = 
  let collectVars3' (V var) x = var : x
      collectVars3' (C op formula) x = foldr1 (.) (map collectVars3' formula) x
  in nub $ collectVars3' formula []

-- Задание 15. Сделайте конструктор типов Formula членом класса Foldable,

-- Задание 15. Сделайте конструктор типов Formula членом класса Foldable,
-- определив функцию foldMap.

instance Foldable Formula where
  foldMap formula (V var) = formula var
  foldMap formula (C op []) = mempty
  foldMap formula (C op [f1]) = foldMap formula f1
  foldMap formula (C operator [f1, f2]) = foldMap formula f1 <> foldMap formula f2

-- Задание 16. Напишите функцию collectVars4, аналогичную collectVars1,
-- которая явно не использует рекурсию, а вместо этого использует
-- foldMap с моноидом эндоморфизмов на типе списков переменных.

collectVars4 :: Eq a => Formula a -> [a]
collectVars4 formula = nub $ foldMap (:[]) formula 
