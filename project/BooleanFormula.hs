module BooleanFormula where

import BooleanSyntax
import Formula
import Data.Bits
import Data.List

-- Этот модуль посвящен работе с булевыми формулами в отличие от
-- Formula.hs, который работает с формулами общего вида.

-- Литература

-- Гаврилов Г.П., Сапоженко А.А. Задачи и упражнения по дискретной
-- математике. М.: Физматлит, 2005.

-- Проверка на сохранение констант: с. 72.

-- Проверка на самодвойственность: с. 64.

-- Проверка на монотонность: с. 76.

-- Проверка на линейность: с. 53, п. 2: "Метод [построения полинома
-- Жегалкина], базирующийся на преобразовании вектора значений функции".
-- Альтернативно см. https://ru.wikipedia.org/wiki/Полином_Жегалкина.
-- Раздел "Метод БПФ" описывает тот же метод, что и в задачнике.
-- Можно также использовать метод треугольника.

-- Примеры формул form1 и form2, определенные в модуле Formula,
-- доступны и здесь при условии, что импорт из BooleanSyntax в модуле
-- Formula не ограничен, то есть строка после "import BooleanSyntax" в
-- Formula.hs остается закомментированной.

-- Задание 1. Напишите функцию, которая возвращает True тогда и только
-- тогда, когда булева функция, определяемая формулой-аргументом,
-- сохраняет значение False. Обратите внимание, что сохранять False не
-- то же самое, что принимать False на всех аргументах.
-- form6 = x v y

preservesFalse :: Eq a => Formula a -> Bool
preservesFalse formula =
    let (n, int_formula) = compileFormula formula
    in not (eval (replicate n False) int_formula)

-- Задание 2. Напишите функцию, которая возвращает True тогда и только
-- тогда, когда булева функция, определяемая формулой-аргументом,
-- сохраняет значение True. Обратите внимание, что сохранять True не
-- то же самое, что принимать True на всех аргументах.

preservesTrue :: Eq a => Formula a -> Bool
preservesTrue formula =
    let (n, int_formula) = compileFormula formula
    in eval (replicate n True) int_formula

-- Задание 3. Напишите функцию, которая возвращает True тогда и только
-- тогда, когда булева функция, определяемая формулой-аргументом,
-- самодвойственна.

selfDualFormula1 = V (Var 'x')
selfDualFormula2 = C Or [C Or [C And [V (Var 'x'), V (Var 'y')], C And [V (Var 'x'), V (Var 'z')]], C And [V (Var 'y'), V (Var 'z')]]
nonSelfDualFormula= C Or [V (Var 'x'), V (Var 'y')]

selfDual :: Eq a => Formula a -> Bool
selfDual formula =
    let (n, int_formula) = compileFormula formula
        values = allEnvs n
        inverted_values = [map not val | val <- values]
    in [eval env int_formula | env <- values] == [not (eval env int_formula) | env <- inverted_values]

-- Задание 4. Напишите функцию, которая возвращает True тогда и только
-- тогда, когда булева функция, определяемая формулой-аргументом,
-- монотонна.

monotonicFormula = C And [V (Var 'x'), V (Var 'y')]
nonMonotonicFormula1 = C Neg [C And [V (Var 'x'), V (Var 'y')]]
nonMonotonicFormula2 = C Xor [V (Var 'x'), V (Var 'y')]

monotone :: Eq a => Formula a -> Bool
monotone formula =
    let (n, int_formula) = compileFormula formula
        value_pairs = zip (allEnvs n) (tail (allEnvs n))
    in all ( ==  True ) [eval ls int_formula <= eval gr int_formula | (ls, gr) <- value_pairs]

-- Задание 5. Напишите функцию, которая возвращает True тогда и только
-- тогда, когда булева функция, определяемая формулой-аргументом,
-- линейна.

---Воспользуемся методом треугольника:
linearFormula1 = C Xor [V (Var 'x'), V (Var 'y')]
linearFormula2 =  C Xor [V (Var 'x'), V (Var 'y')]
linearFormula3 = V (Var 'x')
nonLinearFormula1 = C And [V (Var 'x'), V (Var 'y')]
nonLinearFormula2 = C Xor [C Xor [V (Var 'x'), V (Var 'y')], C And [V (Var 'x'), V (Var 'y')]]

pascalStep :: [Bool] -> [Bool]
pascalStep [x] = [x]
pascalStep row = zipWith xor (tail row) (init row)

pascalTriangle :: [Bool] -> [[Bool]]
pascalTriangle [a] = [[a]]
pascalTriangle row = row : pascalTriangle (pascalStep row)

linear :: Eq a => Formula a -> Bool
linear formula = 
    let n = length (collectVars1  formula)
        non_linear_lst = [length (filter ( ==  True) env) > 1 | env <- allEnvs n]
        coeffs = map head (pascalTriangle $ formulaValues formula)
    in notElem True $ zipWith (&&) non_linear_lst coeffs

