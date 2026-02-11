This document is for me to write down anything I want. It reduces cognitive load and helps me avoid blank-page paralysis. I do not care about structuring it so it is organized for anyone else to read. Its basically just a way for me to externalize thinking, Im thinking out the problem and writing down stuff before I move into coding.

I have ZERO experience working on complex big projects like this, especially in a language like Haskell, Ive solved some simple problems before for assignment 2 but thats all. Therefore, I am not going to re-invent the wheel, I am not going to come up with my own ways to define a tree structure or my own way to do good practice file seperation or other technical details. Of course I will search and discuss with LLMs to make decisions, I am using it as guidance, I am not going to copy paste anything here from it, but I am going to learn good practices from it and implement them.

Going forward, I am going to write my thoughts about the problem, there is no start, middle and end, lets be messy and connect the pieces later.

Ive already read the specifications of the problem twice so I have a very good understanding of how our turing machines work, its a fun language. I read through the more technical stuff too, though I cant say I fully understand them yet, after all, I have no experience, but I will figure this out as I implement the project, I have 11 days left.

In Haskell, we do not have variables that change, instead, we will have a large State object that we pass through funcion.

The Parser: will be a function that takes a String (a file) and returns a Map of Machines

The Tape: Instead of an array we will use two lists:
    leftSide = [2nd_cell, 1st_cell]
    rightSide = [current_cell, next_cell, ..]
    Moving Right: we "pop" the head of rightSide and "push" it ono lefSide

The Machine Representation: we will define a data type, it will look like a record:

data Machine = Machine { staes :: [String], init :: String, transitions :: Map (String, Char) Result }.

Day1: we are going to define our data types in a file called Types.hs and implement he Map (since we are not allowed to use import Data.Map) this is the foundation for both the parser and the execution

I need a refreshment in how haskell types work, since I didnt really need to use them at all in assignment2, I had studied them once but I forgot.

In Haskell we don't have classes with methods. We have Data Types. Think of data as a way to define a "shape" that he information can take

data Direction = MoveLeft | MoveRight

Direction: This is the Type, we use this in function signatures (e.g., "This function returns a Direction").

MoveLeft/MoveRigh: these are Value Constructors. They are the actual values we can pass around in our code

The class keyword defines a "Capability"

A class in Haskell is not like a class in Java. It's more like an Interface. It defines a set of functions that a type should be able to do.

Lets look at Eq:

class Eq a where
    (==) :: a -> a -> Bool

This simply says: "If a type a wants to be part of he Eq family, it must provide a way to check if two things of type a are equal"

The instance Keyword provides the "How-To"

Now, we have our "shape" (Direction) and our "interface" (Eq). The instance keyword is where we write the actual logic

If we wan to check if two directions are the same, we tell haskell how:

instance Eq Direction where
    MoveLeft  == MoveLeft  = True
    MoveRight == MoveRight = True
    _         == _         = False -- This cach-all means any other pair is False

Lets look at Ord:

class (Eq a) => Ord a where
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min              :: a -> a -> Bool
    compare               :: a -> a -> Ordering

Look at the first line: class (Eq a) => Ord a where. In Haskell, this is called a Class Constraint. It means: "You cannot make a type an instance of Ord unless it is already an instance of Eq."

Why? Because its logically impossible to say x < y or x > y if you havent first defined what it means for x to be equal to y

Then we can make our Direction instance of Ord since its already insteance of E

instance Ord Direction where
    -- We decide MoveLeft is "smaller" than MoveRight
    MoveLeft <= MoveRight = True
    MoveLeft <= MoveLeft  = True
    MoveRight <= MoveRight = True
    MoveRight <= MoveLeft  = False

Though I dont think we will need to do such comparisons in the project