# Haskell Course

**This course is designed to teach students Haskell from zero to everything needed to work with Marlowe and Plutus.** The course itself doesn't contain content specific to Marlowe or Plutus. So, if you want to use it to learn Haskell for other purposes, you can! 😃

For a more detailed explanation, keep reading or watch the introduction video:
[![YouTube](https://img.shields.io/badge/YouTube-%23FF0000.svg?style=flat&logo=YouTube&logoColor=white)](https://www.youtube.com/channel/UCX9j__vYOJu00iqBrCzecVw)

## How much should I study if I only want to use Marlowe/Plutus?

In the [outline](#what-well-cover) below, there are clear stopping points (for both Marlowe and Plutus) where we deem you to know enough Haskell to effectively use the technology.

## How to read/watch the lessons

To go through the interactive lessons, go to your chosen lesson's outline inside "[What we'll cover](#what-well-cover)" and click on the button that looks like this:

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/input-output-hk/haskell-course/HEAD?labpath=%2Flessons%2F1-Introduction-to-haskell.ipynb)

And to see the video, click on the button that looks like this:

[![YouTube](https://img.shields.io/badge/YouTube-%23FF0000.svg?style=flat&logo=YouTube&logoColor=white)](https://www.youtube.com/channel/UCX9j__vYOJu00iqBrCzecVw)

## To do the homework

1. Clone this repository.
2. Create a [GitPod](https://www.gitpod.io/) account.
3. Click this button to create a remote dev environment: [![Visual Studio Code](https://img.shields.io/badge/Visual%20Studio%20Code-0078d7.svg?style=flat&logo=visual-studio-code&logoColor=white)](https://gitpod.io/#https://github.com/rober-m/haskell-bootcamp)
4. Select the `code/HomeworkXX` folder with the homework you want to complete.
5. Follow the instructions inside the app/Main.hs file.

#### Repository structure

    Haskell-Course
        |   |
        |   |---- code
        |          |
        |          |---- Homework01 (Homework for lesson 01)
        |          |---- Homework02 (Homework for lesson 02)
        |          ...
        |
        |-------- lessons (Lessons in Juptyer notebook format. Access through Binder.)
                   |
                   |---- 1-Introduction-to-haskell
                   |---- 2-Functions-Data-Types-and-Signatures

Everything else can be safely ignored

## What we'll cover

**This is a tentative outline. Changes can (and will) be made as we advance with the course and gather feedback from students.**

**If there are no buttons on a lesson, it means that it's not published yet.**

### 1. Intro and tools [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/input-output-hk/haskell-course/HEAD?labpath=%2Flessons%2F1-Introduction-to-haskell.ipynb) [![YouTube](https://img.shields.io/badge/YouTube-%23FF0000.svg?style=flat&logo=YouTube&logoColor=white)](https://www.youtube.com/channel/UCX9j__vYOJu00iqBrCzecVw)

- Intro to the course and lectures
  - What we’ll cover
  - Repository structure
- Intro to Haskell
  - How to open and use JupyterLab
  - Purely functional programming language
  - Basic syntax
  - Haskell Type system
  - Lazyness
  - GHC (and GHCi)
- GitPod
  - How to open and use GitPod
  - Example of how to complete a homework assignment.

### 2. Functions, Data types, and Signatures

- Intro to types and signatures
- Fraquently used types
- Function’s signatures
- Infix and prefix functions
- Types in depth
  - Numeric types
    - Int, Integer
    - Float, Double
    - Rational
  - Bool
  - Char
  - Lists
  - Strings
  - Tuples + Tuples VS Lists
- Variables in Haskell
  - Parameters in functions
  - Names/Definitions
- Polymorphic values and type variables

### 3. Pattern matching and how to write functions

- If-then-else
- What is pattern matching
- Where can I use pattern matching + nested pattern matching
  - Pattern matching on function implementations
  - Pattern matching on on lists
  - Pattern matching on tuples
- Case
- Guards
- Let and where
- When to pattern match, if, case, and guards

### 4. Improving and combining functions

- Higher order functions
- Curried functions
- Partial application
- Composing and applying functions (`.` and `$` operators)
- Lambda functions

### 5. Recursion

- Concept
- Examples

### 6. Dealing with lists

- `zip`
- `map`
- `foldl`, `foldr`
- `scan`

### 7. Intro to Type Classes

- What are type classes?
- Common type classes
  - `Eq`
  - `Ord`
  - `Integral`
  - `Floating`
  - `Num`
  - Mentioning `Read`, `Show`, `Enum`, `Bounded`, and `Foldable`.
- Class constraints with examples

### 8. Creating Types

- Type synonyms
  - How to define type synonyms
  - Why use type synonyms
- Defining new types
  - `data`
  - Value parameters
  - Pattern matching types
  - Record syntax
- Parameterizing types
  - Parameterizing type synonyms
  - Parameterizing new types
- Honorable mention of `newType`

### 9. Creating Type Classes and Instances

- Revisiting Type Classes
- The `Eq` type class
  - Defining the `Eq` type class
  - Defining an instance for the `Eq` type class
  - Improving our `Eq` type class (minimal complete definition)
  - Defining an instance for a parameterize type.
- The `Ord` type class
  - Exploring `Ord` type class (Subclassing)
- Deriving
- Complete example

### 10. Basic IO

- We need side effects
- What is IO
- main + putStrLn + composing other functions
- `>>`
- `>>=`
- do notation
  - `do`
  - `<-`
  - `let`
- Some examples
- Read/Write to console
- Read/Write to file

### 11. Bits and bytes

- Grouping bits and bytes
- Haskell and bytes
- Lazy and strict byte strings
- Example

### 12. Pragmas, Modules, and Cabal

- Prelude
- pragmas/extensions
- Overview of base modules
- Importing base modules
- A few modules
  - Data.Char
  - Data.Tuple
  - Data.Array
- Creating our own modules
- Cabal
  - What is it and why we use it
  - Cabal file
  - Using external libraries with Cabal

### 13. Learning on you own and Map

- Using GHCi to find out more
- Hoogle
- HaskellWiki
- Walking through while teaching Map module

### 14. Maybe and Either (only practical use)

- Maybe
  - Why and when to use Maybe
  - Syntax
  - Examples
- Either
  - Why and when to use Either
  - Syntax
  - Examples
- Project using Maybe and IO

### 15. Aeson and final project for Marlowe students

- Aeson
- Final project recapping all the concepts

---

#### YOU'RE READY FOR MARLOWE! 🥳🎉 (Keep going for Plutus.)

---

### 16. Identifying the pattern

    - #TODO

### 17. Monads (most likely will split in several lessons)

    - #TODO
