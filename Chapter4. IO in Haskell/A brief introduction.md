## Separating the Pure from the Impure
By now, you’re used to the fact that Haskell is a purely functional language.
Instead of giving the computer a series of steps to execute, you give it definitions of what certain things are. In addition, a function isn’t allowed to have side effects. A function can give us back only some result based on the parameters we supplied to it. If a function is called two times with the same parameters, it must return the same result.
The fact that functions cannot change state—like updating global variables, for example—is good, because it helps us reason about our programs. However, there’s one problem with this: If a function can’t change anything in the world, how is it supposed to tell us what it calculated?
No matter what your program does, no matter what language it’s written in, I/O is a hugely important part of software. It’s the point where your code meets the real world. The problem is that using I/O inherently requires you to change the world. Take, for example, getting user input from the command line. Each time you have a program that requests user input, you expect the result to be different. But we spent a great deal of time talking about how important it is that all functions take an argument, return a value, and always return the same value for the same argument. If you read a file and write to another, your programs would be useless if you didn’t change the world somewhere along the way.


## So how does Haskell solve this problem?...