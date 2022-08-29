open import Level using (Level)
open import Data.Product
open import Data.Bool
open import Data.Nat as ℕ
open import Data.Nat.Properties
open import Data.Nat.Solver
open import Data.Fin hiding (_+_)
open import Data.Vec
open import Data.Vec.Properties

open import Relation.Binary.PropositionalEquality
open ≡-Reasoning

private
  variable
    ℓ : Level
    A : Set ℓ
    size amount totalAmount n : ℕ
    p q : Fin n
    p≢q : p ≢ q


record Ledger (size : ℕ) (totalAmount : ℕ) : Set where
  constructor ledgerC
  field
    vecLedger : Vec ℕ size
    sum≡totalAmount : sum vecLedger ≡ totalAmount

record LedgerView (size : ℕ) (totalAmount : ℕ) (p q : Fin size) (p≢q : p ≢ q) : Set where
  constructor ledgerViewC
  field
    ledger : Ledger size totalAmount
  open Ledger ledger

  field
    vP : ℕ
    vQ : ℕ

    vP≡loopkup : lookup vecLedger p ≡ vP
    vQ≡loopkup : lookup vecLedger q ≡ vQ

record LederViewWithAmount (size : ℕ) (totalAmount : ℕ)
  (p q : Fin size) (p≢q : p ≢ q) (amount : ℕ) : Set where

  constructor ledgerViewAmount
  field
    ledgerView : LedgerView size totalAmount p q p≢q

  open LedgerView ledgerView
  open Ledger ledger

  field
    vP-amount : ℕ
    vP-amount+amount≡vP : vP-amount + amount ≡ vP

sym≢ : {x y : A} → x ≢ y → y ≢ x
sym≢ x≢y y≡x = x≢y (sym y≡x)

sum≔≡ : ∀ (xs : Vec ℕ n) (p : Fin n) y → sum (xs [ p ]≔ y) + lookup xs p ≡ sum xs + y
sum≔≡ (x ∷ xs) zero y = helper _ _ x where
  open +-*-Solver

  helper : (x y z : ℕ) → x + y + z ≡ z + y + x
  helper = solve 3 (λ x y z → (x :+ y :+ z) , (z :+ y :+ x)) refl
sum≔≡ (x ∷ xs) (suc p) y = begin
  x + sum (xs [ p ]≔ y) + lookup xs p   ≡⟨ +-assoc x _ _ ⟩
  x + (sum (xs [ p ]≔ y) + lookup xs p) ≡⟨ cong (x +_) (sum≔≡ xs p y) ⟩
  x + (sum xs + y)                      ≡˘⟨ +-assoc x _ y ⟩
  x + sum xs + y ∎

makeLedgerWithAmount :
  (vecLedger : Vec ℕ size) (vP : ℕ) (vQ : ℕ) {vP-amount : ℕ}
  (sum≡totalAmount : sum vecLedger ≡ totalAmount)
  (vP-amount+amount≡vP : vP-amount + amount ≡ vP)
  (vP≡loopkup : lookup vecLedger p ≡ vP)
  (vQ≡loopkup : lookup vecLedger q ≡ vQ)
  {p≢q : p ≢ q}
  → LederViewWithAmount size totalAmount p q p≢q amount
makeLedgerWithAmount vecLedger _ _ sum≡totalAmount vP-amount+amount≡vP vP≡loopkup vQ≡loopkup =
  ledgerViewAmount (ledgerViewC (ledgerC vecLedger sum≡totalAmount) _ _ vP≡loopkup vQ≡loopkup)
  _ vP-amount+amount≡vP

pattern ledgerCons vecLedger sum≡totalAmount vP vQ vP≡loopkup
  vQ≡loopkup vP-amount vP-amount+amount≡vP
  = (ledgerViewAmount (ledgerViewC
    (ledgerC vecLedger sum≡totalAmount)
    vP vQ vP≡loopkup vQ≡loopkup) vP-amount vP-amount+amount≡vP)

transferFunds :
    LederViewWithAmount size totalAmount p q p≢q amount
  → LederViewWithAmount size totalAmount q p (sym≢ p≢q) amount
transferFunds {totalAmount = totalAmount} {p} {q} {p≢q} {amount}
  (ledgerCons vecLedger sum≡totalAmount vP vQ vP≡loopkup
    vQ≡loopkup vP-amount vP-amount+amount≡vP) =

    makeLedgerWithAmount (vecLedger [ p ]≔ vP-amount [ q ]≔ vQ+amount) vQ+amount vP-amount
    sum≡ refl lookupQ≡ lookupP≡

  where

  open +-*-Solver

  helper : (x y z : ℕ) → x + y + z ≡ x + z + y
  helper = solve 3 (λ x y z → (x :+ y :+ z) , (x :+ z :+ y)) refl

  helper₂ : (x y w z : ℕ) → x + y + (w + z) ≡ x + w + (y + z)
  helper₂ = solve 4 (λ x y w z → x :+ y :+ (w :+ z) , (x :+ w :+ (y :+ z))) refl

  vQ+amount = vQ + amount

  vQSame : vQ ≡ lookup (vecLedger [ p ]≔ vP-amount) q
  vQSame =  begin
    vQ                 ≡˘⟨ vQ≡loopkup ⟩
    lookup vecLedger q ≡˘⟨ lookup∘update′ (sym≢ p≢q) _ _ ⟩
    lookup (vecLedger [ p ]≔ vP-amount) q ∎

  lookupP≡ : lookup (vecLedger [ p ]≔ vP-amount [ q ]≔ vQ+amount) p ≡ vP-amount
  lookupP≡ = trans (lookup∘update′ p≢q _ _) (lookup∘update p _ _)

  lookupQ≡ : lookup (vecLedger [ p ]≔ vP-amount [ q ]≔ vQ+amount) q ≡ vQ+amount
  lookupQ≡ = lookup∘update q _ _

  sum≡' : sum (vecLedger [ p ]≔ vP-amount [ q ]≔ vQ+amount) + vQ + vP ≡ totalAmount + vQ + vP
  sum≡' = begin
    sum ((vecLedger [ p ]≔ vP-amount) [ q ]≔ vQ+amount) + vQ + vP ≡⟨ cong (λ H → sum ((vecLedger [ p ]≔ vP-amount) [ q ]≔ vQ+amount) + H + vP) vQSame ⟩
    sum ((vecLedger [ p ]≔ vP-amount) [ q ]≔ vQ+amount) + lookup (vecLedger [ p ]≔ vP-amount) q + vP  ≡⟨ cong (_+ vP) (sum≔≡ _ q _) ⟩
    sum (vecLedger [ p ]≔ vP-amount) + vQ+amount + vP  ≡⟨ helper (sum (vecLedger [ p ]≔ vP-amount)) vQ+amount vP ⟩
    sum (vecLedger [ p ]≔ vP-amount) + vP + vQ+amount ≡˘⟨ cong (λ H → sum (vecLedger [ p ]≔ vP-amount) + H + vQ+amount) vP≡loopkup ⟩
    sum (vecLedger [ p ]≔ vP-amount) + lookup vecLedger p + vQ+amount ≡⟨ cong (_+ vQ+amount) (sum≔≡ _ p _) ⟩
    sum vecLedger + vP-amount + (vQ + amount) ≡⟨ helper₂ (sum vecLedger) vP-amount vQ amount ⟩
    sum vecLedger + vQ + (vP-amount + amount) ≡⟨ cong (sum vecLedger + vQ +_) vP-amount+amount≡vP ⟩
    sum vecLedger + vQ + vP ≡⟨ cong (λ H → H + vQ + vP) sum≡totalAmount ⟩
    totalAmount + vQ + vP ∎

  sum≡ : sum (vecLedger [ p ]≔ vP-amount [ q ]≔ vQ+amount) ≡ totalAmount
  sum≡ = +-cancelʳ-≡ _ _ (+-cancelʳ-≡ _ _ sum≡')

LedgerWithAmount : (p : Fin size) (amount : ℕ) (ledger : Ledger size totalAmount) → Set
LedgerWithAmount p amount (ledgerC vecLedger _) = amount ℕ.≤ lookup vecLedger p

LedgerViewWithAmount : (amount : ℕ) (ledger : LedgerView size totalAmount p q p≢q) → Set
LedgerViewWithAmount {p = p} amount (ledgerViewC ledger _ _ _ _) = LedgerWithAmount p amount ledger

ledger→ledgerView : (p q : Fin size) (p≢q : p ≢ q) (ledger : Ledger size totalAmount)
  → LedgerView size totalAmount p q p≢q
ledger→ledgerView p q p≢q ledger = ledgerViewC ledger _ _ refl refl

ledgerView→ledgerViewAmount : (amount : ℕ) (ledger : LedgerView size totalAmount p q p≢q)
  (withAmount : LedgerViewWithAmount amount ledger) → LederViewWithAmount size totalAmount p q p≢q amount
ledgerView→ledgerViewAmount amount ledgerView@(ledgerViewC
  (ledgerC vecLedger sum≡totalAmount)
  vP vQ vP≡loopkup vQ≡loopkup) amount≤lookupP  =
  ledgerViewAmount ledgerView (vP ∸ amount) (m∸n+n≡m {n = amount}
  (subst (amount ℕ.≤_) vP≡loopkup amount≤lookupP))
