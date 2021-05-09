#Monocle library data structure
##Lens
Just like in java we have getter setters, we have similar construct in FP called lenses. Lenses allow you to get, update and modify small value in a big nested(not necessary) large immutable structure.
1. get 
2. set
3. Modify

##Prism
Similar to Lens which provide various operation to modify and get zoomed property, Prism also does the same with the caveat that its defined for sum type of class hierarchy e.g. Option, List etc. It is defined using get and reveseGet function. get takes Superclass object and return the subclass property. and reverseGet does the reverse operation of get.

##Plated
Plated allows us to instruct Monocle how to traverse over a recursive data structure.

##Traversal
Traversal is a type that instructs Monocle how to apply a function f to every item inside of our recursive Tree data structure.  

##Composing 
We can combine lens to through compose function to create a large lens.This prevents repeating same property path again and again in different lens structure. 
 