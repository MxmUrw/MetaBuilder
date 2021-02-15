
# MetaBuilder

A tool intended for streamlining the process of building larger, (possibly) multiple language projects.
Currently focused on building Agda projects, in particular on generating nice-looking documents from annotated Agda source code.

## Alternative Literate Agda
This tool offers the possibility to write literate Agda using docstring-like syntax. But note that the implementation is currently very hacky,
and is very specialized to my own requirements for writing my master's thesis.

Nevertheless, to show what is currently possible, look at the following example code:
```Agda
-- ===* Monoids
-- | Monoids are an important concept in both mathematics and computer science.

-- [Definition]
-- | A type |A| has the structure of a monoid,
record IMonoid (A : 𝒰 𝑖) : 𝒰 𝑖 where

-- | - if there is a special element [..], and a multiplication operation [..].
  field 𝟷    : A
        _⋅_  : A -> A -> A

-- | - Such that the operation is associative,
        assoc-⋅   : ∀{a b c : A} -> (a ⋅ b) ⋅ c ≡ a ⋅ (b ⋅ c)

-- |>  and |𝟷| is a left and right unit for it.
        unit-l-⋅  : ∀{a : A} -> 𝟷 ⋅ a ≡ a
        unit-r-⋅  : ∀{a : A} -> a ⋅ 𝟷 ≡ a

  infixl 50 _⋅_
-- //
```

it is rendered as follows in the PDF:

<img src="https://raw.githubusercontent.com/MxmUrw/MetaBuilder/main/Documentation/Screenshots/Monoids.png" width="700">



