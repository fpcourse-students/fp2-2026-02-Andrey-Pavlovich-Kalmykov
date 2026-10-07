-- Инстанс Functor для Vec из "Defs" объявляется здесь как задача 2.3, поэтому он orphan.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Домашка 2. Параметрический полиморфизм: уровень 2.
module Level2 where

import Data.Kind (Type)
import Defs
import GHC.TypeLits (Symbol)
import MetaUtils (todo)

-- 2.1. Формулы: продвижение вручную
--
-- Поднимите на уровень типов синтаксис пропозициональных формул Prop вручную, без DataKinds
-- и TypeData: объявите пустые типы Var', Not' и типовые операторы ::\/, ::/\, ::->
-- (по одному на конструктор Prop). Пропозициональные переменные — пустые типы A и B.
-- Запишите типом PropExample формулу (a \/ b) -> (~a -> b) ровно с такой расстановкой
-- скобок; приоритеты своим операторам можно не назначать.

data A

data B

-- Здесь ваши объявления Var', Not', ::\/, ::/\ и ::->.
data Var' name

data Not' expr

data left ::\/ right

data left ::/\ right

data left ::-> right

type PropExample = Var' A ::\/ Var' B ::-> (Not' (Var' A) ::-> Var' B)

-- 2.2. Формулы: DataKinds
--
-- Запишите ту же формулу типом PropDataExample, используя продвинутые конструкторы Prop
-- и строки уровня типов (кайнд Symbol, переменные "a" и "b"). Припишите PropDataExample
-- точный кайнд явно — сейчас в заглушке стоит неверный.

type PropDataExample :: Prop Symbol -- Здесь ваш кайнд.
type PropDataExample = Var "a" :\/ Var "b" :-> (Not (Var "a") :-> Var "b")

-- 2.3. Функтор
--
-- Реализуйте Functor для вектора. Заметьте, что типы полностью обеспечивают выполнение
-- законов функтора.

instance Functor (Vec n) where
  fmap _ VNil = VNil
  fmap f (VCons el tail) = VCons (f el) $ fmap f tail

-- 2.4. Конкатенация
--
-- Реализуйте vconcat. Длину результата считает функция уровня типов NatPlus — закрытое
-- семейство типов: определение по образцу, как у обычной функции, только на типах.
-- Подробно семейства типов разбираются в главе 3, здесь достаточно прочитать определение.

type family NatPlus (n :: Nat) (m :: Nat) :: Nat where
  NatPlus Zero m = m
  NatPlus (Suc n) m = Suc (NatPlus n m)

vconcat :: Vec n a -> Vec m a -> Vec (NatPlus n m) a
vconcat VNil vec = vec
vconcat (VCons el tail) vec = VCons el $ vconcat tail vec

-- 2.5. Гетерогенный zip
--
-- Реализуйте hzip двух гетерогенных списков. Список типов результата считает семейство Zip:
-- определите его по аналогии с NatPlus так, чтобы оно было определено на любых двух списках,
-- в том числе разной длины (лишний хвост отбрасывается, как у обычного zip).

type family Zip (as :: [Type]) (bs :: [Type]) :: [Type] where
  Zip (a : as) (b : bs) = (a, b) : Zip as bs
  Zip '[] bs = '[]
  Zip as '[] = '[]

hzip :: HList as -> HList bs -> HList (Zip as bs)
hzip (HCons a atail) (HCons b btail) = HCons (a, b) $ hzip atail btail
hzip HNil _ = HNil
hzip _ HNil = HNil

-- 2.6. Полиморфизм в кайндах
--
-- Tagged полиморфен по кайнду тега. Добейтесь Temperature :: TemperatureUnit -> Type -> Type,
-- не используя оператор (::) ни в определении Temperature, ни в его параметрах.
-- Подсказка: посмотрите на кайнд Tagged и вспомните, как передать тип явно.
-- Затем реализуйте c2f: перевод из Цельсия в Фаренгейт, f = c * 1.8 + 32.

newtype Tagged (tag :: k) (a :: Type) = MkTagged a

data TemperatureUnit = Celsius | Fahrenheit | Kelvin

type Temperature = Tagged @TemperatureUnit

c2f :: Temperature Celsius Double -> Temperature Fahrenheit Double
c2f (MkTagged t) = MkTagged $ t * 1.8 + 32

-- 2.7. Числа Чёрча в обёртке
--
-- Тип числа Чёрча полиморфен, поэтому функции над такими числами имеют высший ранг.
-- Обёртка Church прячет квантор под конструктор: снаружи все функции первого ранга,
-- но чтобы построить значение Church, под конструктором приходится написать полиморфную
-- функцию. Реализуйте zero, suc, plus, mult и fromInt (для неотрицательных чисел);
-- toInt дан для проверки.

newtype Church = Church (forall a. (a -> a) -> a -> a)

toInt :: Church -> Int
toInt (Church n) = n (+ 1) 0

zero :: Church
zero = Church (\_ ini -> ini)

suc :: Church -> Church
suc (Church h) = Church (\f ini -> f $ h f ini)

plus :: Church -> Church -> Church
plus (Church h) (Church g) = Church (\f ini -> g f $ h f ini)

mult :: Church -> Church -> Church
mult (Church n) m = n (plus m) zero

fromInt :: Int -> Church
fromInt 0 = zero
fromInt n = suc $ fromInt (n - 1)

-- 2.8. Пара Чёрча
--
-- Pair — пара по Чёрчу, синоним с квантором внутри: функция, которая отдаёт обе компоненты
-- любому, кто скажет, что с ними делать. pair и pfst даны; реализуйте psnd и pswap.

type Pair a b = forall c. (a -> b -> c) -> c

pair :: a -> b -> Pair a b
pair x y f = f x y

pfst :: Pair a b -> a
pfst p = p const

psnd :: Pair a b -> b
psnd p = p $ flip const

pswap :: Pair a b -> Pair b a
pswap p = uncurry pair $ p $ flip (,)
