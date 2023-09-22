+++
title = "An Introduction to Haskell"
date = "2023-09-16"
description = "Haskell: 0 to Advent of Code"
+++

A couple of days ago, having a bit of free time and with no chance of spending it
outside because of the weather, I decided to freshen up my Haskell by going through some
of last year's [Advent of code](https://adventofcode.com) exercises.
Since I've had lots of fun, I've decided to write this post in an attempt to
strengthen my knowledge of Haskell, as well as hopefully entice you
gentle-reader into trying Haskell yourself.

Starting off with the very basics, we are going to learn everything we need to
solve [AoC-2022 day 9](https://adventofcode.com/2022/day/9): reading data from a
file, parsing it into a custom defined data type and writing a slick and
readable solution.
Brew yourself some coffee and sit tight because this is gonna be one hell of a
ride.

# What the hell Haskell even is
For those who have never heard of it before, Haskell is a **purely functional
programming language**, which is just a fancy way of saying that it involves
lots of functions that pretty much behave like mathematical functions; they take
some inputs and return an output, that's it, nothing more. In fact, Haskell's
functions don't have **side-effects**, therefore you are guaranteed to always
obtain the same result with the same inputs, no matter what. This cute little
property is called **referential transparency** and besides sounding sick as
fuck, it allows you to better reason about the correctness of your code.

Furthermore Haskell is **statically typed**. The compiler ensures the type
safety of your programs, catching lots of bugs at compile time and freeing you
from the hassle and pain of unexpected runtime errors (at least most of them).
To top it all off, you don't have to worry anymore about `null` dereferences,
they are just not possible in Haskell.

Haskell is **lazy**; functions aren't executed until their result is needed,
which among other things, enables us to write **infinite** data structures.

If this sounds cool enough for you, let's start off by dipping our toes in some
Haskell code!

> **_NOTE:_** If you wish to follow along, get your favourite text editor ready
> and install GHC with [ghcup](https://www.haskell.org/ghcup/). Done that, you
> can start an interactive session with `ghci` and load your code with
> `:l path/to/file.hs`. All set!
# The basics
Being a functional programming language, functions are unsurprisingly Haskell's
bread and butter. This means that there's nothing we can do but to start with
them.

> Wait, what?! No "Hello, World"?! Is this guy legit?!
>
> -- _An impatient reader_

Well, as I've told you before, Haskell's functions don't have side-effects and
printing something on the screen is definitely a side-effect. This doesn't mean
Haskell can't print to the screen, but its a bit more convoluted than a simple
`print "Hello, World!"`. We'll get there eventually, pinky promise!

## Functions
Here is the full definition of a function that takes two integers and
returns their sum:
```haskell
mySum :: Int -> Int -> Int
mySum a b = a + b
```
I know the syntax might seem daunting at first, but I can assure you will get
used to it and you might even end up liking it a lot as it comes with some nice
little perks. But first, let's break it down.

The first line is the function's type declaration. It's optional, you don't have
to write it if you don't want to. The compiler can infer it for you, but it's
usually good practice to always write it down since it's useful to better
understand your code.

Functions' type signatures are **right-associative**. In the case of `mySum` it
means it can be rewritten as:
```haskell
mySum :: Int -> (Int -> Int)
```
which can be read as "`mySum` takes an integer and returns a function that 
takes an integer and returns another integer". In Haskell-lingo we say that
functions are **curried**, that is functions of multiple arguments are
transformed into functions that take a single argument and return a function of
one argument that returns a function of one argument that ... and so on until
they return a value. This sounds pretty dumb but it's actually very powerful as
it allows for extreamely simple partial application of functions. You just have
to omit the remaining arguments! Here's an example you can try in ghci:
```haskell
map (1 +) [1, 2, 3] -- returns [2, 3, 4]
```

Here we have just applied the function `(1 +)` to every element of the list,
building a new list from it. The function `+` has type `Int -> Int -> Int` but 
we partially apply it, obtaining a function with type `Int -> Int`, that we can
use to map over a list.

> **_NOTE:_** You can check the type of any expression with ghci just by typing
> `:t <expr>`.

Besides partial applications, this example shows us how functions are invoked:
just by following the function's name with all its arguments. Moreover we have
seen how to pass functions to other functions as arguments. That is, we 
discovered that **functions are first-class citizens**!

Something's strange though. How come the `+` function has one argument on its
left and the other on its right? Aren't arguments supposed to go after the
function's name? Usually that's the case, but in order to make some function
applications more readable, for instance with operators such as `+, -, *, &&, ||,
    ...`, Haskell allows to define functions that are **infix** by default. `+` is
one such case. Anyway, every Haskell binary function can be written in infix
style by wrapping its name in back-quotes: ``1 `mySum` 2`` and all infix
functions can be used prefix style like this `(+) 1 2`.

## Type classes and type constraints
Another thing an astute reader might call me out for is saying that the type of 
the `+` function is not `Int -> Int -> Int`, and she would be perfectly right!
Let's check it with ghci:
```haskell
ghci> :t (+)
(+) :: (Num a) => a -> a -> a
```
Weird new syntax alert, but nothing too crazy to worry about. A
lowercase letter in a type signature is called a **type parameter**, as it's not
a **concrete type** like `Int`, `Double` or `Char`, but a placeholder for any
concrete type. The `(Num a) => ` part is a **type constraint** that states that type
parameter `a` can be any type that is an instance of the `Num` **type class**.

A type class is just a mechanism that ensures certain functions are available
for any given type implementing that class. The `Num` type class for instance
provides all the functions we need to operate on numbers, while the `Eq`
type class assures that two values of a type can be compared for equality.

Haskell has lots of type classes, some of them will be discussed later, some
won't. The point is: type classes are a very powerful tool, that
enables us to write polymorphic code that is easy to reason about.

## Working with Functions
We have seen how to define and invoke simple functions in Haskell, but that's
just the very tip of the iceberg. Let's delve deeper!

### Pattern matching and function's guards
Let's have a look at a possible definition for a factorial function:
```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)
```
Here we see how Haskell allows us to split a function's definition in multiple
"branches" using patter-matching on its arguments. The first branch will be
called whenever `factorial`'s argument will match patter `0`, that is when the
argument is `0`. The other branch will be called in all other cases.

Another thing we notice is that `factorial` is defined as a recursive function,
the result is calculated by repeatedly calling the function, until a base case
is hit. Recursion is Haskell's main tool to achieve "loops". Remember: Haskell
is purely functional, it doesn't have side-effects, thus it has no concept of
variables nor assignments, hence you can't write a loop as you normally would in
an imperative language such as C, Java or Rust. Thinking in terms of recursive
functions might be hard at first, but you shouldn't get discouraged. Moreover,
most of the time, the recursiveness of a function's definition will be
abstracted away using other fuctions. For instance, our `factorial` function
can be rewritten as:
```haskell
factorial :: Int -> Int
factorial x = product [1..x]
```
Wow, this definition is even more concise and readable than the previous one.
Sweet!

Split function's definition in multiple branches based on
the "shape" of the function's arguments with pattern-matching is not always
what one wants. We can do something similar but using the
value of the arguments instead of their shape. This mechanism is called
**function's guards** and it looks like this:
```haskell
fib :: Int -> Int
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)
```
Each branch is introduced by a pipe `|` and a predicate (a function that returns
a boolean value), and is invoked only if the predicate is true. The default
case, `otherwise`, is used when none of the predicates are true.

### Higher-order functions, lambdas and point-free style
Previously we've said that Haskell's functions are first class citizens and that
they can be passed around as arguments to other functions. These functions, the
ones that either take other functions as their arguments or that returns them,
are called **higher-order functions**.

> Dude! How many times did you write "function" in two phrases?!

Here's a simple example:
```haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
```
`applyTwice`, as the name suggests, takes a function `f` of type `a -> a`, an
argument `x` of type `a`, and returns the result of applying `f` twice to `x`.
Let's see it in action:
```haskell
ghci> doubleMe x = x * 2
ghci> applyTwice doubleMe 2
8
```
Wouldn't it be nice to have a way to pass to `applyTwice` a function without having
to define it first, like we did with `doubleMe`? Well, Javascript (_bane of my
existance™_) plus plenty of other languages can define anonymous functions, so
Haskell should too, right? You bet it does, you little twerp. Haskell has
lambdas and they can be used like this:
```haskell
ghci> applyTwice (\x -> x * 2) 2
8
```
Lambdas are introduced with the `\` character, because it looks like the letter λ (in
your dreams it does, lol), followed by the arguments declaration and finally the
lambda's body after an arrow. They work the same way normal functions do, but
are a little less verbose to write. You should be somewhat accustomed to them,
unless you are a Python programmer, in that case, _oh poor fellow_.

Usually, Haskell programmers, writes their functions as a composition of other
functions, without explicitely mentioning the actual arguments they will be
applied to. This is usually refered to as **point-free style**. Basically, it
consists of using lots of `.` to make your code look cool af. Here's an example:
```haskell
add2Mult3 :: a -> a
add2Mult3 = (3*) . (2+)
```
This function simply compose two other functions, one adds `2` to its argument
and the other multiply its argument by `3`. To compose the two functions the
function composition operator `(.)` is used. Its definition looks like this:
```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
```
Later on, we'll make extensive use of this.
## Working on lists
If functions are the number one tool to a Haskell programmer, **lists** come close
second. Haskell's lists store many, possibly infinite, values of a single type,
therefore lists are a **homogenous** data structure.

Lists have type `[a]` where `a` can be any concrete type. The most simple way to
create a list is to simply write it down like so: `[1, 2, 3, 4, 5]`; this
creates a list of `Int`s. Interestingly enough, Haskell's `String`s are
themselves lists! For instance, `"hello!"` is the same thing as `['h', 'e',
'l', 'l', 'o', '!']`. Let's have a look at some basic operation we can do on
lists.

The `(++)` operator puts two lists together:
```haskell
ghci> :t (++)
(++) :: [a] -> [a] -> [a]
ghci> [1, 2, 3] ++ [4, 5, 6]
[1, 2, 3, 4, 5, 6]
```

The `(:)` operator prepends an element to the head of a list:
```haskell
ghci> :t (:)
(:) :: a -> [a] -> [a]
ghci> 0 : [1, 2, 3]
[0, 1, 2, 3]
```
Moreover, the `(:)` operator can be used to pattern-match lists:
```haskell
head :: [a] -> a
head (x:_xs) = x
```
Here we re-define function `head` (which is already provided by Haskell) using
`:` to extract the very first element from the list's remaining elements. This
is possible because the syntax `[1, 2, 3]` is just syntactic-sugar for
`1:2:3:[]`.

Similar to `head` we have:
```haskell
ghci> let xs = [1, 2, 3]
ghci> tail xs
[2, 3]
ghci> last xs
3
ghci> init xs
[1, 2]
ghci> length xs
3
ghci> null xs
False
ghci> null []
True
ghci> 2 `elem` xs
True
ghci> 4 `elem` xs
False
```
And many many more. Have a look at the
[Data.List](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-List.html)
module for a complete list of every list operations.

Another way to create lists is using **ranges**. Haskell's ranges are pretty
smart, they enable you to to write both finite and infinite lists, even allowing
for an optional step. Here they are in action:
```haskell
ghci> [1..5]
[1, 2, 3, 4, 5]
ghci> [2, 4 .. 10]
[2, 4, 6, 8, 10]
ghci> [3, 6 .. 10]
[3, 6, 9]
ghci> [1..]
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, ..
```

If ranges aren't enough, how about their "on steroid" counterpart, **list
comprehension**. Here's how you can use list comprehension to get a list of
tuples representing the position of every white cell in a chess board:
```haskell
ghci> [(x, y) | x <- ['a'..'h'], y <- [1..8], odd ((ord x - ord 'a') + y + 1)]
[('a',2),('a',4),('a',6),('a',8),('b',1),('b',3),('b',5),('b',7),('c',2),
('c',4),('c',6),('c',8),('d',1),('d',3),('d',5),('d',7),('e',2),('e',4),('e',6),
('e',8),('f',1),('f',3),('f',5),('f',7),('g',2),('g',4),('g',6),('g',8),
('h',1),('h',3),('h',5),('h',7)]
```
Cool stuff!

To conclude this section on lists, we ought to talk about all those higher-order
functions that operate on lists, such as `map`, `filter` and `fold`s. These
functions are nowadays commonplace in almost all programming languages, therefore
I won't discuss them in details. In fact, `map` and `filter` behave analogously
to their Javascript, Rust, or Python counterpart. Slightly more interesting is
`fold`, which in Haskell is not just a single function but rather a family of
functions. Indeed we have `foldl`, `foldl'`, `foldl1`, `foldr`, `foldr'`, and
`foldr1`. That's a lot of folds! Unsurpisingly, the `foldl` family
folds the list from the left, while the `foldr` family does so from the right.
`fold*1` doesn't take a starting value for the accumulator and `fold*'` forces
strict evaluation forsaking lazyness. `fold`s are the most powerful
higher-order functions on lists, in fact every other function can be defined in
term of folds. Let's see an example:
```haskell
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []
```
Lastly we have the `scan` family of functions, which is foreign to both
Javascript and Python. They work similarly to `fold` but
return all the intermediate accumulator states in the form of a list. For
instance, let's solve this simple problem:
> We have a balanced string of parenthesis (only '(' or ')') and we want to check how
> deeply they nest. As an example: "((()()))" has 3 nesting levels, and
"(()((())))" has 4.
```haskell
maxDepth :: String -> Bool
maxDepth = maximum
  . scanl (+) 0
  . map (\p -> if p == '(' then 1 else -1)
```
If we test it with ghci:
```haskell
ghci> maxDepth "((()()))"
3
```
## The Maybe type
An astute reader may have realized that some of the functions' definitions
previously given out can fail in certain circumstances. Let's have a look at
`head` again:
```haskell
head :: [a] -> a
head (x:_) = x
```
`head` uses pattern matching to extract the first element of a list and returns
it, but what happens when the list has no elements? Does `head` return `null`?
But wait, you said that Haskell doesn't have `null`! Let's give it a try:
```haskell
ghci> head []
*** Exception: Prelude.head: empty list
```
Huh, it returns an error.. I mean, `null` would have been better than error,
wouldn't it? How does Haskell handle these cases?

As we previously said, Haskell doesn't have `null`, but guess what? It has
something way better: the `Maybe` type, which is defined like so:
```haskell
data Maybe a = Just a | Nothing
```
The `Maybe` type is a type with two variants, `Just a` when a value is present,
or `Nothing` when it's missing. Simple right? Let's give it a spin and implement
a `safeHead` function:
```haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

ghci> safeHead []
Nothing
ghci> safeHead [1, 2, 3]
Just 1
```
One would argue that this is no different than using `null`, with the only
advantage, albeit compelling, of the explicit possible absence of such a value 
in the function's signature. Rust does the same thing with the `Option<T>` type,
so do both Kotlin and Typescript with `T?`. So, what's the advantage here? Let's
go through an example: suppose we have a list of numbers and we want to find the
first value that is a multiple of `7`, when we find it we want to add `3` to it
and return the result. In Typescript it would look similar to this:
```typescript
const f = (xs: number[]): number? => {
  const x = xs.find(n => n % 7 == 0);
  return x ? x + 3 : undefined;
}
```
Grr, that's gnarly. And we only had two operations to combine; imagine how the
code would look with many nested operations on a possibly undefined result!

Haskell, now is your turn too shine:
```haskell
f :: [Int] -> Maybe Int
f = fmap (+3) . find (\x -> x `mod` 7 == 0)
```
Nice! So, Haskell gives us a well defined way to "map" a possible undefined
value through `fmap`. Its definition looks similar to this:
```haskell
fmap :: (a -> b) -> Maybe a -> Maybe b
fmap f (Just x) = Just (f x)
fmap _ Nothing = Nothing
```
If we apply `f` to a `Nothing` we get a `Nothing` back, otherwise, if we apply
`f` to a `Just x` we get a `Just (f x)`. In other words, `fmap` can "look
inside" the `Maybe` value and apply `f` only if something is "inside" it.

If you thing this is cool, get ready, because this is just the tip of the
iceberg! As we'll see in the next section, `Maybe` values are not the only thing
that can be `fmap`-ed over.
# Functors, Applicatives, and Monads
I'm a lier. Kinda.

If you try checking the type definition of `fmap` you'll see that is not the one
I gave you in the previous section. In fact:
```haskell
ghci> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
```
What's a **Functor**? It sounds cool!
## Functors
Since we have already seen that syntax, we can say with a certain degree of
certainty that `Functor` is a type-class. That's indeed right! Functor is a
type-class, specifically it's a type-class for types that can be "mapped" over.
It's definition looks like this:
```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b -- same as the infix operator (<$>)
```
We have already seen how `Maybe` is an instance of `Functor`, but are there any
other instances? Well, lists can be mapped over, right? In fact, lists are
indeed functors themselves:
```haskell
instance Functor [] where
  fmap = map
```
with `fmap` corresponding to `map`. Another instance of `Functor` that is not as
obvious as `[a]`, is the function type `((->) r)`. Wtf?! How do you map over a
function?! Let's try replacing `f` with `r ->` in `fmap`'s type definition:
```haskell
instance Functor ((->) r) where
fmap :: (a -> b) -> (r -> a) -> (r -> b)
```
Uhhm.. So, for functions `fmap` takes a function `a -> b`, a function `r -> a`
and returns a function `r -> b`. Strange, this feels familiar, doesn't it? Wait!
That's just the same as function composition `(.)`! If we put the two
definitions together and we use the same type parameters this is what we get:
```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
-- changing type parameters
(.) :: (a -> b) -> (r -> a) -> (r -> b)
```
That's indeed the same type! Moreover, it kinda makes sense right? If we open up
a function the thing we find inside it is its return value. If we want to map over
a funtion then, we take the return value of the function and apply another
function to it. If we write it as an expression:
* we open up the function: `f (x)`
* and we apply another function to what's inside it: `f (g x)`
This definetely smells like function composition! And that's indeed the case:
```haskell
instance Functor ((->) r) where
  fmap = (.)
```
Knowing that functions are functors too is not that helpful, but we now have a
better grasp on what makes a type a functor. Plus our brain got way more
wrinkly!

We have seen that Functors are things that can be mapped over, allowing to
change their content through `fmap`. In one's mind, this concept would very well
be modeled using a "box" analogy: we have a box, our functor, and by applying a
function to it we change its content, but not the box itself. This works great
in most cases, expecially to grasp the essence of functors at first, but it
might fall short in some more complex instances. This said, let's try a new
mental model to help us reason not only about functors, but about applicatives
and monads too. So, functors aren't boxes but **computational contexts** or
**effects**. Here's a couple examples:
* The `Maybe` type _carries the effect_ of possible failure.
* The list type `[]` carries the effect of choice, in fact its content can be
nothing, one value, many values or even infinite ones. Lists represents
_non-determinism_.
## Applicative Functors
Now with functors under our belt, we can level up our Haskell skill by getting
acquainted with **applicative functors**, or just applicatives for short.

If functors allow us to map an effectful value, applicatives give us the
superpower of threading a value into a minimal effectful context and the ability
to sequence computations on such contexts. Let's jump to `Applicative`'s class
definition:
```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```
Soooo, what do we have here.. `Functor f => Applicative f` tells us that an
instance of `Applicative` must be a `Functor` first; got that. `pure` puts a
value into the applicative context, that's easy peasy. And finally, `<*>`
applies a function wrapped in an applicative context to a value wrapped in the
same context. Mmm, sounds tricky. Let's try to contextualize it by taking a look
at the `Maybe` instance of `Applicative`:
```haskell
instance Applicative Maybe where
  pure = Just

  Just f <*> m = fmap f m
  Nothing <*> _ = Nothing
```
Ok, so now we can apply a function that is possibly not there to a value that is
itself possibly not there.. This doesn't sounds too exciting.
```haskell
ghci> Just signum <*> Just 3
Just 1
ghci> Just signum <*> Nothing
Nothing
ghci> Nothing <*> Just (-2)
Nothing
```
I mean, sure, very cool stuff, but I gotta say I'm not that impressed. Maybe it
has its use cases, but I don't see myself returning a `Maybe` function any time
soon. But wait! What if we use it to apply a function to two possibly undefined
values? That sounds more reasonable! To achieve this we can partially apply the
function to the first `Maybe` argument and then use `<*>` to get the final
value! For instance, let's say we want to return the sum of the first even number
and the first odd number in a list. That would look something like this:
```haskell
sumFirstEvenAndOdd :: [Int] -> Maybe Int
sumFirstEvenAndOdd xs = (+) <$> firstOdd <*> firstEven
  where
    firstOdd = find odd xs
    firstEven = find even xs
```
It makes sense. `(+) <$> firstOdd` gives us back either a `Just (n+)` or
`Nothing`, thus if we apply this to a `Just m` the operation completes with a
`Just (n+m)`. Cool! This doesn't just works with functions of two arguments but
can be used for any number of arguments. Since the pattern `f <$> x <*> y` is
used pretty often, Haskell provides us with the function `liftA2` which does the
same thing:
```haskell
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y
```
We also have `liftA3`, `liftA4`, and so on for functions of more than two
arguments.

What about lists though? Lists are functors, are they applicatives too? They
surely are! Let's give it a try:
```haskell
ghci> pure 1 :: [Int]
[1]
ghci> [(1+)] <*> [1, 2, 3]
[2, 3, 4]
ghci> [(1+)] <*> []
[]
ghci> [(1+),signum,(*3)] <*> [1,2,3]
[2, 3, 4, 1, 1, 1, 3, 6, 9]
```
What's wrong with the last one?! If you were expecting `[2,1,9]` as the result
don't worry, I'll explain now. Remember we said that the effect or context of
lists is _choice_? Well, choice means that they can't decide on a single result.
If we can't decide on a value, the only possible course of action is to not decide
at all. So, when we combine the effects of lists, we simply consider
all possible outcomes, that is we evaluate every possible combinations of the
pair `(f, x)`, hence the resulting list will have the same length as the product
of the two lists' lengths.

> **_NOTE:_** If your desired behaviour for the `<*>` operator is to perform its
>computations element-wise, what you are looking for is
>[ZipList](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Applicative.html#t:ZipList)
>which I won't cover in this article.

## Monads
The time has finally come. We wrinkled our brain with functors and applicative
functors and so only one step divides us from reaching the pinnacle of Haskell's
type trinity: **Monads**.

Monads are as powerful as they are dreaded. Even Scratch and Javascript
programmers fear them. Asking what a monad is, is taboo; the only sensible
response is "a monad is a monoid in the category of endofunctors, what's the
problem?".
Monads are truly incredible! But we are ready to tackle them.
Sorta. Kinda. Well, we really don't have to fully comprehend them, what we
actually need is to get to know them well enough to be able to harness them into
writing some sick af Haskell code. Let's go!

Diving right into the class definition we see:
```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```
Starting off with the easiest one, `return`, it kinda reminds of `pure`, doesn't
it? Well it's basically the same thing, it takes a value and wraps it into a
minimal effectful context, which this time has to be an instance of both Monad
and Applicative. Two more to go. The `>>=` operator, also called
"bind", is used to sequence effectful computations, threading the result of the
first into the second as its argument. To understand what `>>=` does, let's jump
into an example:
```haskell
ghci> safeDiv3 x = if x `mod` 3 == 0 then Just (x `div` 3) else Nothing
ghci> safeDiv3 21
Just 7
ghci> safeDiv3 5
Nothing
ghci> Just 21 >>= safeDiv3
Just 7
ghci> Just 5 >>= safeDiv3
Nothing
ghci> Nothing >>= safeDiv3
Nothing
ghci> Just 9 >>= safeDiv3 >>= safeDiv3
Just 1
```
Ok, unsurprisingly `Maybe` is a monad, that was to be expected. So, `>>=` allows
us to chain monadic computations in a simple way. Without it, we would have to
pattern match on the result of each application and decide if we can keep on
going. That's indeed handy.

The last function the `Monad` type class provides us is `(>>)`, somethimes
called "then". The `>>`
operator is quite simple, it takes two monadic values and combines their
effects, discarding any value from the first one:
```haskell
ghci> Just 3 >> Just "hello"
"hello"
ghci> Just 3 >> Nothing
Nothing
ghci> Nothing >> Just "hello"
Nothing
```
I want to stress on the fact that `>>` doesn't simply returns the right monad.
It first combines the two effects, then the right pure value is returned inside
the resulting effect. This is probably hard to understand as `Maybe` is quite
the simple monad. How about lists then? We have seen how lists are functors, and
applicative functors, so lists being monads shouldn't come as a surpise
```haskell
ghci> [1, 2, 3] >> [-1, 1]
[-1, 1, -1, 1, -1, 1]
```
We can now see a bit better how the effects gets combined. The non-determinism
of the first list is combined with the second's resulting in a list with more
choices. Then this resulting list gets filled with the values of only the second
one.

Let's have a look at both `Maybe` and `[]` instances of `Monad`:
```haskell
instance Monad Maybe where
  (Just x) >>= f = f x
  Nothing >>= _ = Nothing

instance Monad [] where
  xs >>= f = concat (fmap f xs)
```
The `Maybe`'s instance is simple enough, but the list's is a bit more tricky.
What's with the `concat`? The type signature for `>>=` on lists is the
following: `(>>=) :: [a] -> (a -> [b]) -> [b]` which we obtain simply replacing
`m` with `[]`. So, `f` is a function that takes a single value and returns a
list. Thus if we use `f` to map over list `xs` we obtain a list of lists:
```haskell
ghci> (\x -> [-x, x]) <$> [1, 2, 3]
[[-1, 1], [-2, 2], [-3, 3]]
```
`>>=`'s signature though states that the result should be a list of pure values
and not a lists of monads. For this reason, we use `concat` to "flatten" the
list:
```haskell
ghci> :t concat
concat :: [[a]] -> [a]
ghci> concat [[-1, 1], [-2, 2]]
[-1, 1, -2, 2]
```
The `>>=` operator is indeed powerful, but sometimes, expecially when we have to chain
many different computation, `>>=` gets a bit tedious to use:
```haskell
ghci> Just 12 >>= safeDiv3 >>= (\x -> Just " all good!" >>= (\y -> Just (show x ++ y)))
Just "4 all good!"
```
Luckily Haskell provides us with a very convenient way to work with multiple
effects in sequence: **do notation**. Let's see it in action:
```haskell
foo :: [Int] -> Maybe String
foo xs = do
  x <- find (>10) xs
  y <- safeDiv3 x
  return (show y ++ " cool!")
```
Ok, cool. This kinda resemble imperative code! If we had to write this function
without using do notation, it would look something like:
```haskell
foo xs = find (>10) xs >>= safeDiv3 >>= (\y -> Just (show y ++ " cool!"))
```
Which, we gotta say, it's not as readable. In kinda sucks to be honest.
### The IO monad
With our new shiny do notation we can work with monads in quite the simple way.
Just using `Maybe` and `[]` though is quite boring. Let's introduce another
monad, the one we have been waiting for all along. Guess it's finally time for
that `print "Hello, World!"` huh. Let's give it a try:
```haskell
ghci> print "Hello, World!"
"Hello, World!"
```
> Woooot?! You said it wasn't that easy! We had to go through all that shit but
> it actually WAS that easy!

Hold down your horses. Check the type signature of `print`:
```haskell
ghci> :t print
print :: Show a => a -> IO ()
```
See, not that simple. Type class `Show` is for types that can be made into
strings through the `show` function we have seen earlier. That `IO ()` at
the end is, guess what, a monad, where `()` is the empty tuple, usually called "unit"
and is basically the same thing as `void` in other languages. The `IO` monad is
used to carry out operations that requires to interact with the underlying
operating system. That is, reading and writing to and from a file, or reading
user input and printing to the terminal. All these operations are indeed
side-effects, and we have stressed already how Haskell's functions can't perform
side-effects. That is indeed true, and that's why we have to wrap them in an IO
monad.

`IO` is, like all other monads, an effectful computation, which in this
case perform a side-effect. Although `IO` carries a side-effect, that doesn't
mean every function that operates on `IO` can carry them out. In fact, they
can't. In Haskell the only function that can perform the effectfull computation
of an `IO` monad is the `main` function, which is the entry point for every
Haskell program. That's the only exception, plus `ghci`. All other functions can indeed work
with `IO` but its effect will only be carried out once the monadic `IO` value
reaches the `main` function. This means that `main` isolates _impure_ code from
pure one, allowing us to still benefit from referential transparency.

Let's play a bit with `IO`:
```haskell
greetUser :: IO ()
greetUser = do
 putStr "Welcome, what's your name good Sir?\n> "
 name <- getLine
 putStrLn ("A pleasure to meet you, " ++ name)

haskellIsAmazing :: Int -> String
haskellIsAmazing :: unlines . flip replicate "Haskell is amazing!"

main = do
 greetUser
 putStr "How many times would you like me to say that haskell is amazing?\n> "
 n <- read <$> getLine
 putStrLn (haskellIsAmazing n)
```
Running this we get:
```
Welcome, what's your name good Sir?
> Bruno
A pleasure to meet you, Bruno
How many times would you like me to say that Haskell is amazing?
> 3
Haskell is amazing!
Haskell is amazing!
Haskell is amazing!
```
Well well well, what do we have here? We finally managed to write a program a 6
year old could write in two minutes with Python! But we did it in Haskell, that
makes us like super cool. In your face kiddo!

Anyway, we now know how to tap into the _inpure_, making our program capable of
interacting with the outside world. We can use `putStr` and `putStrLn` to write
a string to `stdout` and `getLine` to grab a string from `stdin`.

`read` is the opposite of `show`, it enables us to parse a value out of a
string. It's a function provided by the `Read` type class and is implemented for
the most commonplace Haskell's types.
### The State monad
In imperative languages, keeping track of some state is as easy as pie. You just use variables,
changing their values as your program runs. For instance, suppose we have to
count the occurrences of even numbers in a list. In Javascript you could write
something like:
```javascript
function countEvenInList(list) {
  let count = 0;
  for (n in list) {
    if (n % 2  == 0) count++
  }
}
```
Besides the fact that it's not 2008 anymore and that we could easily rewrite this
piece of garbage as
```javascript
const countEvenInList = list => list.filter(n => n % 2 == 0).length
```
performing operations while keeping track of some kind of state is indeed
helpful.

We have seen how monads give us the ability to work around side-effects. So, can
they help us achieve stateful computations too, without forsaking pureness?
As always the answer is yes! What we need is a brand new monad, the **state
monad**. Let's try it first just to get the hang of it, then we can try to
unveil its inner workings.

Let's use again the parenthesis expression exercise. Our task is the same but
now we won't suppose the expression is balanced. We'll have to check that
ourselves.
Furthermore our input now won't only be comprised of parenthesis, but it will
be a full math expression, with numbers and operators. It will look something
like this `"(1 + (2 - 3) * 2 - (1 - (3 + 4)))"`. Let's do some cleanup first.
We just care about the parenthesis:
```haskell
cleanExpr :: String -> String
cleanExpr = filter (`elem` ['(', ')'])

ghci> cleanExpr "(1 + (2 - 3) * 2 - (1 - (3 + 4)))"
(()(()))
```
Easy. In an imperative language we could go through each element of the list
while keeping track of a stack of parenthesis. Every time we encounter `(` we
push it to the top of the stack.
When we encounter `)` instead, we pop the top
of the stack. When we are done traversing the entire list we check the
stack: if it's empty then the expression is balanced.
> **_NOTE:_** since the input has only one type of parenthesis, we could simply
> use a counter instead of going for a stack.
>
> **_Exercise for the reader:_** try solving this same exercise but with inputs
> that use all kind of parenthesis: `"{[()]}"`. The input is not guaranteed to
> have balanced parenthesis. You'll have to validate that for yourself. Don't
> forget that parenthesis have different priorities!

In Haskell we can do the
same thing, even without the `State` monad:
```haskell
pop :: [a] -> Maybe ([a], a)
pop [] = Nothing
pop (x:xs) = Just (xs, x)

push :: a -> [a] -> [a]
push = (:)

updateStack :: Char -> [Char] -> Maybe [Char]
updateStack p stack =
  if p == '(' then
    Just (push p stack)
  else
    fst <$> pop stack

maxDepth :: String -> Maybe Int
maxDepth expr = do
  stackSizes <- traverse (fmap length) 
    . scanl (\stack p -> stack >>= updateStack p) (Just [])
    $ cleanExpr expr
  let isBalanced = last stackSizes == 0
  if isBalanced then
    return $ maximum stackSizes
  else 
    Nothing
```
Hopefully you should be able to understand most of this code. Let's go
through it just in case. We clean our input, retaining only the parenthesis, with
`cleanExpr expr`. The `$` operator is just the same as function application but
with maximum precedence. It's basically the same as wrapping whatever is after
it in parenthesis. The resulting expression is scanned over, applying `updateStack`
at each step. `updateStack` takes the current parenthesis and the current state
of the stack and either push to it or pops it. Since `pop` might fail we have to
use `Maybe` where's necessary.

When `scanl` has done, we are left with a list
containing every stack state we went through. We use `traverse` to convert
`[Maybe [Char]]` into a `Maybe [Int]`, by applying `length` to each
element. Nice, we now have `stackSizes`, a list containing the lengths of all the
stacks' states we have encountered. In order to check whether the input
expression is balanced we make sure the last stack is empty. If that's the case,
we return the maximum value of the list of stack sizes. That's our result.

Not too shabby I would say! But we can do better, let's try to make some use of
the `State` monad to further increase the readability of our code.
```haskell
maxDepth :: String -> Maybe Int
maxDepth expr = do
  let (stack, depth) =
        flip runState 0
          . foldM step (Just [])
          $ cleanExpr expr
  isBalanced <- null <$> stack
  if isBalanced
    then Just depth
    else Nothing

step :: Maybe [Char] -> Char -> State Int (Maybe [Char])
step stack p = do
  let newStack = stack >>= updateStack p
  let currDepth = maybe 0 length newStack
  modify (max currDepth)
  return newStack
  where
    updateStack p stack =
      if p == '('
        then Just (push p stack)
        else fst <$> pop stack
```
Let's go through this. As we did with the other solution, we first cleanup the
expression. Then, this time, we fold over it instead of scanning. We can see
that the folding function used is `foldM`, which is basically the same as
`foldl` but with a monadic accumulator, in this case `State Int (Maybe [Char])`.
In the `step` function we compute the new stack and the current depth, which we
use to update our state with the `modify` function. `modify` computes the new
state value by applying to the current state value the function it receives as
argument. In this case, we
pass to `modify` the partially applied `max` function, which will set the state
to `max(currDepth, state)`. The result of `foldM` is passed to `runState` which
splits the result of our computation from the state itself.

> **_NOTE:_** I know the solution for this exercise might feel a bit
> contrived. If it wasn't for the sake of showcasing the `State` monad, I would
> have first parsed the solution, making sure the input is valid. Only then,
> I would calculate the max depth of the expression's nesting. The this is,
> `State` shines in cases where you need to compute something while keeping
> track of something else. This is why I had to validate and
> compute the solution "at the same time".

Let's have a look at how `State` is defined.
```haskell
data State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  (State h) >>= f = State $ \s -> let (a, newState) = h s
 				      (State g) = f a
 				  in g newState
```
We haven't seen this syntax yet, so I'll go through it very breafly now. `State`
is defined as a **record** with a single _field_ `runState`, a function that
unwraps the stateful computation represented by the `State` monad into the tuple
`(result, state)`. Records allow creating data types with named fields. Those
fields can be accessed with syntax `<field_name> <record value>`, since the
record's fields are just ordinary functions. Or they can be accessed with the
more familiar syntax `<record_value>.<field_name>`. Here's an example:
```haskell
data Person { name :: String, age :: Int }

ghci> let bruno = Person "bruno" 33 -- or Person { name = "bruno", age = 33 }
ghci> name bruno
"bruno"
ghci> bruno.age
33
```

For what concerns the `State`'s `Monad` instance, `return` takes a pure value and
makes a stateful computation that will return that value. `>>=` is slightly more
convoluted. 
> **_Remember:_** a stateful computation is just a
> function that takes an initial state and returns a result, together with a new
> state.

Let's break it down in smaller steps:
1. The input stateful computation `h` is run with input state `s`, resulting in
the computation's result `a` and a new state `newState`.
2. `f` is applied to `a` obtaining a new stateful computation `g`
3. `g` is run on `newState` obtaining our result
4. Everything is wrapped in `State`
```
g:                           state -> (finalResult, finalState)
f:                input -> g  ^
                    ^         |
h: initialState -> (a, newState)
     ^
     |
s: --+
```
This graph might help you see how the resulting stateful computation is
something that takes a state `s` and returns `(finalResult, finalState)`.

Let's try with another example:
```haskell
type Stack = [Int]

pop :: State Stack Int
pop = do -- or State $ \(x:xs) -> (x, xs)
  (x:xs) <- get
  put xs
  return x

push :: Int -> State Stack ()
push x = do -- or State $ \xs -> ((), x:xs)
  modify (x:)

ghci> runState (push 0 >> push 1 >> push 2 >> pop >> pop >> push 3) []
((), [3, 0])
ghci> runState (pop >>= (\x -> return $ show x ++ " kebabs")) [10, 9, 8]
("10 kebabs", [9, 8])
ghci> runState (pop >>= push) [42]
((), [42])
```
Let's consider the last example where we combine a `pop` with a `push`. If we
analyze what's going on step by step we see that the definiton for the bind
operator `>>=` actually makes sense.
1. The result has to be a stateful computation that takes a state `s` to run,
hence `State $ \s -> ...`.
2. We run `pop` on `s` obtaining `(head s, tail s)`
3. We pass `head s` to `push` which gives us stateful computation `g`. To run `g` we
pass it `tail s`, the state provided by `pop`, obtaining `((), (head s) : (tail s))`.
4. So, `pop >>= push` is the same as `\s -> ((), (head s) : (tail s))`. If we
substitute `s` with stack `[42]` we get
```haskell
((), (head [42]) : (tail [42]))
((), 42 : [])
((), [42])
```
The state monad is admitedly more complicated that the monads we have seen so
far, but hopefully you should be able to employ it your programs, even if you
haven't fully grasped it.
# AoC 2022 Day 9
We have finally made it to this final section. It's time to put our newly
acquired Haskell skills to the test by tackling [Advent of Code 2022 day
9](https://adventofcode.com/2022/day/9).

In this exercise we have to simulate the behaviour of a rope with two knots,
one at each side. The positions of the knots can be models
on a two-dimensional grid. The tail can never be further that one cell from
the head. When that happens the tail is pulled towards the head accordingly to
the rules specified in details in the exercise's description.
We are given as input a list of moves which tell us in which direction and for
how many cells the rope's head is pulled. Here's a sample input:
```
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
```
Our task is to find out how many unique cells the rope's tail visits during the
complete series of motions. Let's go!
## Data modeling
The first thing we wanna do is decide on our data representation.
```haskell
type Pos = (Int, Int)
type Rope = [Pos]

data Dir = U | D | L | R deriving (Show)
data Move = Move Dir Int deriving (Show)
```
We model 2D coordinates with `Pos`, an alias for `(Int, Int)`. Then, we can
model the rope as a list of `Pos`s. A `Move` is just a `Dir` (up, down, left, or
right) and the amount of cells. `deriving (Show)` is used to automatically make
our type an instance of `Show`.
## Parsing the input
For parsing we are gonna use
[attoparsec](https://hackage.haskell.org/package/attoparsec), a fast and
simple parser combinator library. Attoparsec provides us some primitive parsing
functions (such as `char` to parse a given character, or `endOfLine` to parse a
new line character) and some combinators (such as `many1` to apply a parser at
least one time). Through the combinations of parsing functions and parser
combinators we are going to define a complex parser that will consume the
exercise's input into a list of `Move`s, or will fail to do so. Let's give it a
go:
```haskell
import Data.Attoparsec.Text (..)
import Control.Applicative ((<|>))

inputParser :: Parser [Move]
inputParser = move `sepBy` endOfLine
  where
    move = liftA2 Move dir step
    step = char ' ' *> decimal
    dir = do
      c <- (char 'U' <|> char 'D' <|> char 'L' <|> char 'R')
      case c of
        'U' -> return U
        'D' -> return D
        'L' -> return L
        'R' -> return R
```
Hopefully it shouldn't be too hard to understand what's going on here.
`inputParser` is the parser we define to parse our input. It's defined using the
`sepBy` combinator, which applies the `move` parser as many times as it can,
consuming everytime a new line character as separator.

`sepBy` returns a list of all
the values returned by `move`, which is a parser that simply combines the
results of parsers `dir` and `step`, constructing a `Move`. `step` consumes a `'
'` and parses a decimal number. `dir` parses a direction out of a character.
Besides parsers and combinators we see some new faces in this code:
* `(*>)` is the same as `(>>)` but using a functorial context instead of a
monadic one. Is basically used to consume something and ignore it.
* `(<|>)` is the alternative operator. Returns its "successful" operand. For
instance:
```haskell
ghci> Nothing <|> Just 2
Just 2
ghci> Just 2 <|> Nothing
Just 2
ghci> Nothing <|> Nothing
Nothing
```
Let's wire up our parser to the code that retrieves the input:
```haskell
main :: IO ()
main = do
  rawInput <- pack . (++ " ") <$> readFile "./input.txt"
  let input = maybeResult $ parse inputParser rawInput
  case input of
    Just input -> print $ part1 input
    Nothing -> putStrLn "Failed to parse input!"
  return ()

part1 :: [Move] -> Int
part1 _ = 0
```
In our main function we read our whole input from file `input.txt`, then we append a
space at the very end and we `pack` it into a `Text` value. We use our
`inputParser` to parse our `Text` input, converting it into a `Maybe`. If we managed
to correctly parse the input we pass it to `part1` and we print its result.
## Moving the rope
Let's define some utility functions in order to make our life easier:
```haskell
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g x y = f (g x y)

dist :: Pos -> Pos -> Pos
dist (dx, dy) (sx, sy) = (dx - sx, dy - sy)

absDist :: Pos -> Pos -> Pos
absDist = absPos .: dist
  where
    absPos (x, y) = (abs x, abs y)

normDist :: Pos -> Pos -> Pos
normDist = normPos .: dist
  where
    normPos (x, y) = (signum x, signum y)

kingDistance :: Pos -> Pos -> Int
kingDistance = uncurry max .: absDist

mkRope :: Rope
mkRope = replicate 2 (0, 0)

movesToSteps :: [Move] -> [Pos]
movesToSteps moves = dirToPos <$> concatMap moveToSteps moves
  where
    moveToSteps (Move dir step) = replicate step dir

dirToPos :: Dir -> Pos
dirToPos dir = case dir of
  U -> (0, 1)
  D -> (0, -1)
  L -> (-1, 0)
  R -> (1, 0)

sumPos :: Pos -> Pos -> Pos
sumPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
```
Nothing too crazy going on. `(.:)` composes a function that takes one argument
with one that takes two. `movesToSteps` transforms a list of `Move`s into a list
of unit `Pos`s, splitting each `Move` into multiple unit steps. `kingDistance`
returns the number of moves a chess king would have to take to travel from one
position to another.

Let's move on to the heart of this problem. We have to move the rope and keep
track of all the unique cells the tail visits. To avoid counting duplicate cells
we are going to use a `Set`. Here we go:
```haskell
import qualified Data.Set as Set

part1 :: [Move] -> Int
part1 = simulateRope mkRope

simulateRope :: Rope -> [Move] -> Int
simulateRope rope moves =
  Set.size
    . flip evalState rope
    . foldM moveRope Set.empty
    $ movesToSteps moves

type TailPos = Set.Set Pos

moveRope :: TailPos -> Pos -> State Rope TailPos
moveRope tailPos dir = do
  rope <- get
  let (h : body) = rope
  let newHead = sumPos h dir
  let newRope = scanl takeStep newHead body
  put newRope
  return $ Set.insert (last newRope) tailPos
  where
    takeStep dst src =
      if kingDistance dst src > 1
        then sumPos src $ normDist dst src
        else src
```
Let's start with `moveRope`. From its type signature we see that it's a stateful
computation that returns the set of all the positions the tail visited, whilst
keeping track of the rope's state. To move the rope by `dir`, `moveRope`
calculates the new rope's head position and uses that to compute the position of
all other knots (in this case just another one, _wink wink_) with `scanl`.
The knots are moved with `takeStep` function, that encapsulate all the rules
provided by the exercise. After the new rope has been computed the tail's
position is added to the set.

Moving on to `simulateRope`. We map the list of moves to the lists of all unit
steps. We fold over this lists with `moveRope` starting with an empty set for
the tail's positions. The stateful computation returning from `foldM` is
evaluated by providing it the initial state of the rope. The result of the
computation is the set of all the unique positions the tail visited and the
result of our exercise is the size of this set. Nice!

Running the code on my input, I get the answer `6642` which earns me a ⭐.
## Part 2
The second part of the problem is gonna be a breeze. The rope now has `10` knots
instead of just `2`. Let's fix that:
```haskell
main :: IO ()
main = do
  rawInput <- pack . (++ " ") <$> readFile "./input.txt"
  let input = maybeResult $ parse inputParser rawInput
  case input of
    Just input -> do
      print (part1 input)
      print (part2 input) -- 👈 new
    Nothing -> putStrLn "Failed to parse input!"
  return ()

part1 :: [Move] -> Int
part1 = simulateRope (mkRope 2) -- 👈 updated

part2 :: [Move] -> Int
part2 = simulateRope (mkRope 10)

mkRope :: Int -> Rope
mkRope = flip replicate (0, 0)
```

`mkRope` now takes the length of the rope as argument, and builds the rope by
replicating the starting position `length`-times. That's it.
If you run your program now, you should be presented with solutions for both
`part1` and `part2`. Go get your second ⭐ champ!

# Conclusion
We did it! We started from scratch and by slowing working our way up, we managed
to write a pretty damn good solution for one of the problems of Advent of Code.

Although this article has skipped over lots of important Haskell stuff,
hopefully you should feel somewhat capable of solving most AoC's exercises on
your own. If you feel like delving deeper into Haskell, there's plenty of good 
resources out there. The first that comes to mind are
[Learn You a Haskell For Great Good](http://learnyouahaskell.com/) and
[Real World Haskell](https://book.realworldhaskell.org/).
