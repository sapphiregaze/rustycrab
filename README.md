# RustyCrab

## Assignment Questions

### 1: How are you storing identifiers in your tree? How might you be able to lookup these identifiers? Does your tree support "linking" an identifier to where it is defined? Should it? 

```
We decided to have identifiers in the tree be implementations of an Expression node within the tree. These nodes are specific implementation of a generic tree node structure with fields specific to their type. These identifiers can be looked up using an algorithm which searches from within the deepest scope depth outward until a matching identifier is found. The tree currently does not support linking identifiers to where they are defined. This would be useful to implement in a series of passes after the abstract syntax tree is created to be able to perform type checking, compatibility between expressions where the type is used, and output code generation.
```

### 2: Explain why you chose the grammar you did and the tree format you did.

```
We adapted the provided grammer from the provided yacc file and modified it to suit our implementation, a combinator style parser, which involved solving issues with left recursion. We choose the sexpr tree format because it was the simplest to implement and fit closely with the structure of the data coming out of the parser.
```

