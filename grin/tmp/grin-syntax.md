Syntactical extensions for GRIN
=========================

This document proposes new syntactic constructs for the GRIN IR, as well as modifications to some of the already existing ones.

Motivation
-----------

The current syntax of GRIN can pose some difficulties for analyses. As an example, the created-by analysis requires each binding to have a variable pattern. That is, each binding must have a _name_. Also, analyzing programs where every intermediate value has an explicit name is much easier.

Furthermore, the current Haskell definition of the syntax allows certain erroneous programs. Currently, we rely on the linter to reject these programs, but a more rigid definition of the syntax could catch these errors during parsing.

### Datalog based analysis

Currently, the analyses are implemented via compiled abstract interpretation, where the abstarct program is generated from Haskell and it can be run in both Haskell and C++. However, in the future, we plan to transition to Soufflé Datalog based analyses. In order to build a Datalog model for the GRIN AST, every single instruction must have a name. It is not a question of convenince, the model requires it. This is yet another reason to give an explicit name to everything in the AST.

_Note_: Soufflé has many advantages over our current implementation, but this is out of the scope of this proposal.


Naming
------

These syntactic constraints mainly deal with the naming of intermediate values. They make sure, that most of the currently available values can always be referred to by an explicit name. In other words, constants could only be introduced through the `pure` function. Everywhere else, we would be passing variables around. All of these constraints can be implemented trivially. Namely, we only have to change the below constructions so that they require a `Name` instead of a value.

  - named case scrutinees
  - named node fields
  - name function arguments
  - named `store` argument
  - named case alternatives (needed for the Soufflé Datalog AST model)

An observant reader might have noticed, that the pattern matching construct for bindings isn't addressed above. That is because we will deal with them separately. Also, note that `fetch` and `update` already only accept variable names as arguments.

New
---

These syntactic constructs are completely new to the GRIN language.

### @patterns

As mentioned earlier, each binding should have a name, the variable it binds. Currently, the syntax allows for pattern bindings, which do not bind a variable to the left-hand side computation. This problem could be fixed by introducing @patterns.

```haskell
(CInt k) @ v <- pure n
<both v and k can be referenced here>
```

@patterns combine the flexibility of value patterns with the rigidness of the naming convention. By removing value patterns from the language, and introducing @patterns, we could achieve the same expressive power as before, meanwhile making sure that everything can be referenced by an explicit name.

Note that unlike the @pattern in Haskell, here the variable name and the pattern are swapped. This is to keep the syntax consistent with named case alternatives.

Furthermore, we should restrict the available patterns only to nodes. Currently literals and even variable names are allowed. This is too general.

### Named case alternatives

To model the GRIN AST in Datalog, each syntactic construct must have an identifier. Currently, the alternatives in case expression don't have unique identifiers. We can solve this issue by naming each alternative.

```haskell
case v of
  (CNil)       @ v1 -> <code>
  (CCons x xs) @ v2 -> <code>
```

The syntax would be similar to that of the @patterns, but the variable name and the pattern would be swapped. This is to improve the readability of the code: with long variable names, the patterns can get lost. Also, there could be an arbitrary number of spaces between the @ symbol and the pattern/variable. This is also to improve readability through proper indentation. Readability is only important for unit tests, not for generated GRIN code.

#### Semantics

The names of the case alternatives would be the scrutinee restricted to the given alternative. For example, in the above example, we would know that `v1` must be a node costructed with the `CNil` tag. Similarly, `v2` is also a node, but is constructed with the `CCons` tag.

Named alternatives would make dataflow more explicit for case expressions. Currently, the analyses restrict the scrutinee when they are interpreting a given alternative (they are path sensitive), but with these changes, that kind of dataflow would be made visible.

No longer needed
----------------

By introducing the above mentioned syntactic modifications, some of the currently available constraints became redundant: `LPat`, `SimpleVal`. Both `LPat` and `SimpleVal` were just synonyms to `Val`, the type for representing GRIN values. `LPat` was used to represent binding patterns, and until now they could have been any value. Now only names and @patterns with nodes are allowed. `SimpleVal`, as its name suggests, was used to represent values with simple structure (eg.: literals). They were used to constrain the possible values for a node argument. Now nodes can only have named arguments, so `SimpleVal` became redudant as well.

Furthermore, some of the low-level GRIN constructs will deprecated too: `VarTag` node and indexed `Fetch`. `VarTag` was used to represent nodes with a variable for their tags, and indexed `Fetch` was used to fetch a given field of a node from the heap. These constructs were originally needed for RISC code generation. Since then, GRIN transitioned to LLVM, and over the years these constructs proved to be unnecessary.


Future
------

### Basic blocks

As for the future, we plan to introduce basic blocks into the language. This will bring its own syntactic modifications, but they will be mostly independent of the above-discussed changes.

### GADT-style AST

Also, GRIN will have a GADT-style based AST as well. This is for the front end users of GRIN who want to generate well-structured GRIN programs. It will make all syntactic restrictions explicit, hence it will force the user to build a syntactically sound AST.

Represesnting the AST with GADTs has a serious drawback though: recursion schemes don't support GADTs at the moment. This means that neither the currently implemented analyses, nor the transformations will work on the GADT-style AST. This is the reason why we will only use it for the GRIN code generation. The front end will generate a GADT-style GRIN AST, then we will transform it into a plain old ADT, so that we can continue working with it.


Appendix
=======

Old syntax
----------

Part of the old syntax that has been impacted by the modifications.

```haskell
-- * GRIN Value
type LPat      = Val -- ConstTagNode, VarTagNode, ValTag, Unit, Lit, Var
type SimpleVal = Val

data Val
  = ConstTagNode  Tag  [SimpleVal] -- complete node (constant tag) ; HIGH level GRIN
  | VarTagNode    Name [SimpleVal] -- complete node (variable tag)
  | ValTag        Tag
  | Unit                           -- HIGH level GRIN
  -- simple val
  | Lit Lit                        -- HIGH level GRIN
  | Var Name                       -- HIGH level GRIN
  | Undefined     Type


-- * Case Pattern

data CPat
  = NodePat Tag [Name]  -- HIGH level GRIN
  | LitPat  Lit         -- HIGH level GRIN
  | DefaultPat          -- HIGH level GRIN
  | TagPat  Tag

-- * GRIN Expression

type SimpleExp = Exp
type Alt       = Exp
type Def       = Exp
type Program   = Exp

data Exp
  = Program     [External] [Def]
  | Def         Name [Name] Exp
  -- Exp
  | EBind       SimpleExp LPat Exp
  | ECase       Val [Alt]
  -- Simple Exp
  | SApp        Name [SimpleVal]
  | SReturn     Val
  | SStore      Val
  | SFetchI     Name (Maybe Int) -- fetch a full node or a single node item in low level GRIN
  | SUpdate     Name Val
  | SBlock      Exp
  -- Alt
  | Alt CPat Exp
```


New Syntax
----------

Part of the new syntax that has been impacted by the modifications.

```haskell
-- * GRIN Value

data Val
  = ConstTagNode  Tag  [Name]
  | Unit
  -- simple val
  | Lit Lit
  | Var Name
  | Undefined     Type

-- * Case Pattern

data CPat
  = NodePat Tag [Name]
  | LitPat  Lit
  | DefaultPat


-- * Binding pattern
data BPat
  = VarPat { _bPatVar :: Name }
  | AsPat  { _bPatTag    :: Tag
           , _bPatFields :: [Name]
           , _bPatVar    :: Name
           }

-- * GRIN Expression

type SimpleExp = Exp
type Alt = Exp
type Def = Exp
type Program = Exp

data Exp
  = Program     [External] [Def]
  | Def         Name [Name] Exp
  -- Exp
  | EBind       SimpleExp BPat Exp
  | ECase       Name [Alt]
  -- Simple Exp
  | SApp        Name [Name]
  | SReturn     Val
  | SStore      Name
  | SFetch      Name
  | SUpdate     Name Name
  | SBlock      Exp
  -- Alt
  | Alt CPat Name Exp

```



