open import Data.Nat
open import Data.List
open import Data.Unit
open import Data.Empty
open import Function

_-_p≥_ : (a b : ℕ) (p : a ≥ b) → ℕ
zero - zero p≥ p = zero
suc a - .zero p≥ z≤n = suc a
suc a - (suc b) p≥ s≤s c = a - b p≥ c

data All {A : Set} : (P : A → Set) → List A → Set where
  [] : {P : A → Set} → All P []
  _∷_ : {x : A} {xs : List A} {P : A → Set} → P x → All P xs → All P (x ∷ xs)

data SubList {A : Set} : List A → Set where
  []   : SubList []
  _¬∷_ : {xs : List A} → (x : A) → SubList xs → SubList (x ∷ xs)
  _∷_  : {xs : List A} → (x : A) → SubList xs → SubList (x ∷ xs)

sub→list : {A : Set} {xs : List A} → SubList xs → List A
sub→list [] = []
sub→list (x ¬∷ xs) = sub→list xs
sub→list (x ∷ xs) = x ∷ sub→list xs

list-sub : {A : Set} {xs : List A} → SubList xs → List A
list-sub [] = []
list-sub (x ¬∷ xs) = x ∷ list-sub xs
list-sub (x ∷ xs) = list-sub xs

nonEmptySub : {A : Set} {xs : List A} → SubList xs → Set
nonEmptySub [] = ⊥
nonEmptySub (_ ¬∷ xs) = nonEmptySub xs
nonEmptySub (_ ∷ _) = ⊤

allList→allSub : {A : Set} {f : A → Set} {lista : List A} (sub : SubList lista)
  (allLista : All f lista) → All f $ list-sub sub
allList→allSub [] allLista = allLista
allList→allSub (_ ¬∷ sub) (y ∷ allLista) = y ∷ allList→allSub sub allLista
allList→allSub (_ ∷ sub) (_ ∷ allLista) = allList→allSub sub allLista

NonNil : ∀ {A : Set} → List A → Set
NonNil [] = ⊥
NonNil (_ ∷ _) = ⊤
