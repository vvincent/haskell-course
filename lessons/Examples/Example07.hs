{-
    What is a Type Class?
    A type class is a way to define a set of functions that can operate on different types.
    It allows you to define a common interface for different types, enabling polymorphism.
    Type classes are similar to interfaces in other programming languages, but they are more powerful because they allow you to define default implementations for functions.
    A type class defines a set of functions that can be implemented by different types.
-}

{-
What are the differences between type classes in Haskell and interfaces in object oreriented language like c#?
    Type classes in Haskell are a way to define a set of functions that can operate on different types, allowing for polymorphism.
    They are similar to interfaces in object-oriented languages like C#, but they have some key differences:
    1. Type classes allow for ad-hoc polymorphism, meaning you can define functions that work with multiple types without needing to create a new type for each combination.
    2. Type classes can be extended retroactively, allowing you to add new instances for existing types without modifying the original type definition.
    3. Type classes can have default implementations, which allows you to provide a common behavior for all instances of the type class.

Great question! While Haskell's type classes and C#'s interfaces both provide a form of polymorphism, they differ significantly in their approach and capabilities. Here are the key differences:

## 1. **Implementation Location**

**Type Classes (Haskell):**
- Implementations are defined **separately** from the type definition
- You can add type class instances to existing types retroactively
- Multiple people can define instances for the same type/class combination

```haskell
-- Define a type
data Person = Person String Int

-- Later, add Show instance
instance Show Person where
  show (Person name age) = name ++ " (" ++ show age ++ ")"
```

**Interfaces (C#):**
- Must be implemented **at the type definition site**
- Cannot retroactively add interface implementations to existing types
- Sealed once defined

```csharp
// Must implement interface when defining the class
public class Person : IComparable<Person>
{
    public int CompareTo(Person other) { ... }
}
```

## 2. **Retroactive Implementation**

**Type Classes:**
- Can make existing types instances of new type classes
- Can make new types instances of existing type classes
- This is called the "expression problem" solution

```haskell
-- Make existing Int type instance of a new class
class Drawable a where
  draw :: a -> String

instance Drawable Int where
  draw x = "Drawing: " ++ show x
```

**Interfaces:**
- Cannot add interfaces to existing types without modifying source code
- Would need wrapper classes or extension methods (limited)

## 3. **Multiple Dispatch vs Single Dispatch**

**Type Classes:**
- Support true multiple dispatch
- Can constrain multiple type parameters independently

```haskell
class Convert a b where
  convert :: a -> b

instance Convert String Int where
  convert = read

instance Convert Int String where  
  convert = show
```

**Interfaces:**
- Single dispatch only (method called on one object)
- Limited ability to express relationships between multiple types

## 4. **Type Safety and Inference**

**Type Classes:**
- Integrated with Haskell's type inference system
- Compiler automatically infers constraints
- Compile-time resolution

```haskell
-- Compiler infers: (Num a, Ord a) => a -> a -> a
myFunc x y = if x > y then x + 1 else y
```

**Interfaces:**
- Explicit implementation required
- Runtime dispatch (virtual method calls)
- Less sophisticated type inference

## 5. **Coherence**

**Type Classes:**
- **Coherent**: Only one instance of a type class for a given type
- Prevents ambiguity but limits flexibility
- Global uniqueness enforced by compiler

**Interfaces:**
- Can have multiple implementations through different inheritance paths
- Explicit interface implementation can resolve conflicts
- More flexible but potentially ambiguous

## 6. **Associated Types vs Generic Constraints**

**Type Classes:**
- Can have associated types and functional dependencies
- Express complex type relationships

```haskell
class Collection c where
  type Element c
  empty :: c
  insert :: Element c -> c -> c
```

**Interfaces:**
- Limited to generic type parameters
- Cannot express as complex type relationships

## 7. **Default Implementations**

**Type Classes:**
- Can provide default implementations
- Can define minimal complete definitions

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  -- Default implementations
  x == y = not (x /= y)
  x /= y = not (x == y)
```

**Interfaces (modern C#):**
- Can provide default implementations (C# 8.0+)
- Similar capability but newer feature

## Summary

Type classes are more powerful and flexible than interfaces, offering:
- **Retroactive implementation**
- **Better type inference integration**
- **Multiple dispatch capabilities**
- **More sophisticated type relationships**

However, interfaces are:
- **More familiar to OOP developers**
- **Simpler conceptually**
- **Part of a different paradigm (object-oriented vs functional)**

Type classes represent a more advanced approach to ad-hoc polymorphism that's particularly well-suited to functional programming's emphasis on composition and type safety.
-}

{-
    For example, the `Bool` type. To see the type classes to which the `Bool` type belongs, you can use the `:i` (info) command in ghci. If we run `:i Bool`, we get:
    
    -- Bool
    type Bool :: *
    data Bool = False | True
        -- Defined in ‘GHC.Types’
    instance Eq Bool -- Defined in ‘GHC.Classes’
    instance Ord Bool -- Defined in ‘GHC.Classes’
    instance Enum Bool -- Defined in ‘GHC.Enum’
    instance Show Bool -- Defined in ‘GHC.Show’
    instance Read Bool -- Defined in ‘GHC.Read’
    instance Bounded Bool -- Defined in ‘GHC.Enum’

    -- Int
    type Int :: *
    data Int = GHC.Types.I# GHC.Prim.Int#
            -- Defined in `GHC.Types'
    instance Bounded Int -- Defined in `GHC.Enum'
    instance Enum Int -- Defined in `GHC.Enum'
    instance Integral Int -- Defined in `GHC.Real'
    instance Num Int -- Defined in `GHC.Num'
    instance Ord Int -- Defined in `GHC.Classes'
    instance Read Int -- Defined in `GHC.Read'
    instance Real Int -- Defined in `GHC.Real'
    instance Show Int -- Defined in `GHC.Show'
    instance Eq Int -- Defined in `GHC.Classes'

    -- Eq, List of instances of Eq type class
    ...
    -- Defined in `GHC.Classes'
    instance Eq Bool -- Defined in `GHC.Classes'
    instance Eq Char -- Defined in `GHC.Classes'
    instance Eq Double -- Defined in `GHC.Classes'
    instance Eq Float -- Defined in `GHC.Classes'
    instance Eq Int -- Defined in `GHC.Classes'
    instance Eq a => Eq [a] -- Defined in `GHC.Classes'
    instance Eq Ordering -- Defined in `GHC.Classes'
    instance Eq a => Eq (Solo a) -- Defined in `GHC.Classes'
    instance Eq Word -- Defined in `GHC.Classes'
    instance (Eq a, Eq b) => Eq (Either a b)
    -- Defined in `Data.Either'
    instance Eq a => Eq (Maybe a) -- Defined in `GHC.Maybe'
-}

{-
    Eq is a type class. in C# it is an interface. 
    interface Eq<T>
    {
        bool Equals(T other);
        bool NotEquals(T other);
    }

    if Bool is an instance of Eq, it means that Bool implements the methods defined in the Eq type class.
    in C# it would mean that the Bool type provides an implementation for the Equals method.
    For example, in C#, you might define an instance of Eq for the Bool type like this:

    class BoolInstance : Eq<Bool>
    {
        public bool Equals(Bool other)
        {
            // Implementation here
        }
        public bool NotEquals(Bool other)
        {
            // Implementation here
        }
    }

    class IntInstance : Eq<Int>
    {
        public bool Equals(Int other)
        {
            // Implementation here
        }
        public bool NotEquals(Int other)
        {
            // Implementation here
        }
    }
-}

{-
    Common type classes
    - `Eq`: Defines equality (`==`) and inequality (`/=`) operations.
    - `Ord`: Defines ordering operations (`<`, `<=`, `>`, `>=
    - `Show`: Defines how to convert a type to a string representation.
    - `Read`: Defines how to parse a string into a value of the type.
-}

{-
    The Ord is a type class that defines a total ordering for types. It allows you to compare values of a type and determine their relative order.
    The Ord type class provides methods for comparing values, such as `<`, `<=`, `>`, and `>=`. It is used to define a total ordering on types, which means that for any two values of the type, you can determine whether one is less than, equal to, or greater than the other.
    The Ord type class is a subclass of the Eq type class, which means that any type that is an instance of Ord must also be an instance of Eq. This is because you need to be able to check for equality before you can determine the order of two values.
-}

{-
    The Num type class is a type class that defines numeric operations for types that can be treated as numbers. It provides a common interface for numeric types, allowing you to perform arithmetic operations like addition, subtraction, multiplication, and division.
    The Num type class includes methods for basic arithmetic operations, such as `+`, `-`, `*`, and `/`. It also includes methods for negation (`negate`), absolute value (`abs`), and conversion to a floating-point type (`fromInteger`).
    The Num type class is a subclass of the Eq and Ord type classes, which means that any type that is an instance of Num must also be an instance of Eq and Ord. This is because you need to be able to check for equality and order before you can perform arithmetic operations.
    The Num type class is used to define numeric types in Haskell, such as Int, Integer, Float, and Double. These types can be used in arithmetic expressions and can be manipulated using the methods defined in the Num type class.
    Int is an instance of the Num type class, which means that it provides implementations for the methods defined in the Num type class. This allows you to perform arithmetic operations on Int values, such as addition, subtraction, multiplication, and division.
    For example, you can use the `+` operator to add two Int values together, like this:

    ```haskell
    let x = 5
    let y = 10
    let z = x + y
    ```
-}

{-
    What's the signature of this function?

    fToC :: Float -> Float
    fToC x = (x - 32)*5/9

    -- The most general valid type signature for this function is:
    fToC :: (Fractional a) => a -> a
-}

{-
    Multiple constraints for the same type variable
    In Haskell, you can specify multiple constraints for the same type variable in a type signature. This allows you to define functions that can operate on types that satisfy multiple type class constraints.
    
    For example, consider the following function:
    f :: (Eq a, Num a) => a -> a -> Bool
    f x y = (x + 1) == (y + 1)
    -- This function takes two arguments of the same type a, which must be an instance of both the Eq and Num type classes.
    -- The function adds 1 to each argument and then checks if the results are equal using the (==) operator from the Eq type class.
-}

{-
    Constraints for multiple type variables  
    In Haskell, you can also specify constraints for multiple type variables in a type signature. This allows you to define functions that can operate on types that satisfy different type class constraints for different type variables.
    
    For example, consider the following function:
    isXBigger :: (Ord a, Num p) => a -> a -> p
    isXBigger x y = if x > y then 1 else 0
    -- This function takes two arguments of type a, which must be an instance of the Ord type class, and returns a value of type p, which must be an instance of the Num type class.
    -- The function compares the two arguments using the (>) operator from the Ord type class and returns 1 if the first argument is greater than the second, and 0 otherwise.
-}