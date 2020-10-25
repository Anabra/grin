Syntactical extensions for GRIN
=========================

This document proposes new syntactic constructs for the GRIN IR, as well as modifications to some of the already existing ones.

Motivation
-----------

The current syntax of GRIN can pose some difficulties for analyses. As an example, the created-by analysis requires each binding to have a variable pattern. That is, each binding must have _name_. Also, analyzing programs where every intermediate value has an explicit name is much easier.

Furthermore, the current Haskell definition of the syntax allows certain erroneous programs. Currently, we rely on the linter to reject these programs, but a more rigid definition of the syntax could prevent errors statically.


Naming
------

These syntactic constraints mainly deal with the naming of intermediate values. They make sure, that most of the currently available values can always be referred to by an explicit name. In other words, constants could only be introduced through the `pure` function. Everywhere else, we would be passing variables around. All of these constraints can be implemented trivially. Namely, we only have to change the below constructions so that they require a `Name` instead of a value.

  - named case scrutinees
  - named node fields
  - name function arguments
  - named `store` argument

An observant reader might have noticed, that the pattern matching construct for bindings isn't addressed above. That is because we will deal with them separately. Also, note that `fetch` and `update` already only accept variable names as arguments.

New
---

These syntactic constructs are completely new to the GRIN language.

### @patterns

As mentioned earlier, each binding should have a name, the variable it binds. Currently, the syntax allows for pattern bindings, which do not bind a variable to the left-hand side computation. This problem could be fixed by introducing @patterns.

```haskell
v@(CInt k) <- pure n
<both v and k can be referenced here>
```

@patterns combine the flexibility of value patterns with the rigidness of the naming convention. By removing value patterns from the language, and introducing @patterns, we could achieve the same expressive power as before, meanwhile making sure that everything can be referenced by an explicit name.

### Function application

Currently, normal functions and externals share the same syntactic node for application. Externals are stored together with GRIN programs, and they are differentiated from normal functions by looking up the applied function's name in the stored external list. This makes analyzing function applications quite inconvenient.

We could introduce different syntactic nodes for normal functions and externals, but that would pose an unnecessary overhead on analyses and transformations in certain use cases. Instead, we will introduce different _types of names_ for functions and externals. The application node will differentiate functions from externals by wrapping their names in different data constructors.

```haskell
data AppName
  = Fun { appName :: Name }
  | Ext { appName :: Name }

Exp = ... | SApp AppName [Name] | ...
```

Structural
----------

These modifications impose certain structural constraints on GRIN programs.

### Last pure

Binding sequences should always end in a `pure`. This will make the control flow a little bit more explicit. However, this change could be impacted by the introduction of basic blocks. It might be wiser to delay this change.

### Program, function, definition

The above-mentioned notions should be different syntactic constructs. They should form a hierarchy, so it is always clear whether a transformation/analysis works on an entire GRIN program, a single function, or only on an expression.

Currently available, but has to be more precise
-----------------------------------------------

These modifications would turn currently linter-checked properties into static constraints. For example, only _real_ expressions could appear on the left-hand side of a binding, or the alternatives of a case expression could really only be alternatives.

No longer needed
----------------

By introducing the above mentioned syntactic modification, some currently available constraints become redundant: `LPat`, `SimpleVal`.

Questions
---------

### Parsing function applications

The current syntax does not differentiate normal function application frm external function application. The new syntax will differentiate them. This means, we must decide whether a name corresponds to a normal function or to an external while parsing. Two solutions that might work are the following

#### Use previoulsy parsed information

We could add some sort of state to the parsers that keeps track of the available externals. Since externals can be added through the `PrimOpsPrelude`, we would also need to pass those as an extra argument to the parser.

#### Naming convention for externals

We could introduce a new naming convention for externals. They would always begin with an undersocre.


###  Implicit parsing of unit patterns

Currently, the parser can implictly parse bindings that have a unit pattern. The most common example for this is `update`. The string `"update p v"` is parsed as if it was `"() <- update p v"`.  The new syntax does not allow unit patterns. We must think of an alternative way to express this. Maybe a wildcard pattern?


Future
------

As for the future, we plan to introduce basic blocks into the language. This will bring its own syntactic modifications, but they will be mostly independent of the above-discussed changes.

Prototype AST
--------------

The below examples only include the changes regarding to the naming convention.

### New constructs

```haskell
data BPat
  = VarPat { bPatVar :: Name }
  | AsPat  { bpatVar :: Name
           , bPatVal :: Val
           }

data AppName
  = Fun { appName :: Name }
  | Ext { appName :: Name }
```

### Changes

```haskell
data Val
  -- CHANGE: Name
  = ConstTagNode  Tag  [Name]
  -- CHANGE: Name
  | VarTagNode    Name [Name]
  | ValTag        Tag
  | Unit
  -- simple val
  | Lit Lit
  | Var Name
  | Undefined     Type

data Exp
  = Program     [External] [Def]
  | Def         Name [Name] Exp
  -- Exp
  -- CHANGE: BPat
  | EBind       SimpleExp BPat Exp
  -- CHANGE: Name
  | ECase       Name [Alt]
  -- Simple Exp
  -- CHANGE: Name
  | SApp        AppName [Name]
  | SReturn     Val
  -- CHANGE: Name
  | SStore      Name
  | SFetchI     Name (Maybe Int)
  -- CHANGE: Name
  | SUpdate     Name Name
  | SBlock      Exp
  -- Alt
  | Alt CPat Exp
```
