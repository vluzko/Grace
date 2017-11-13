# Syntax Proposals

## Variables
### Immutable variable declaration
type identifier

### Mutable variable declaration
mut type identifier

## Variable assignment
identifier = expression

## Conditionals
if expression:
  code
else if expression:
  code
else:
  code

## Loops

### For iterator
#### Python style
for var in iterator(:)
  code

### Enumerated for
#### Python style #M: I prefer this one.
for i, var in enumerated(iterator)
  code

#### Special syntax
fore i, var in iterator
  code

for i, var ine iterator
  code

Some other variant of for/in

#### Inference
for var, i in iterator
  code

Doesn't work if we want to do variable splitting with ","


### While
#### Python style
while condition(:)
  code

## Functions

### Declaration
#M: I prefer one of these two.
func function\_name(arg1: arg1\_type): return\_type
  code

func return\_type function\_name(arg1: arg1\_type)
  code

#M: I like the look of these two less.  
#   -> looks like dereferencing a pointer and the last one
#   doesn't have arg1 and arg1_type next to each other.
func function\_name(arg1: arg1\_type) -> return\_type (:)
  code

function\_name :: arg1\_type -> return\_type
def function\_name(arg1)
  code

### Call
function\_name(arg1)

### Returns
func function\_name(arg1: arg1\_type): return\_type
  code
  return foo

func function\_name(arg1: arg1\_type): return\_type
  code
  return foo
  more code
  return bar





