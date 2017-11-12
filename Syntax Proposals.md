# Syntax Proposals

## Variables
### Immutable variable declaration
type identifier

### Mutable variable declaration
mut type identifier

## Variable assignment
identifier = expression

## Loops

### For iterator
#### Python style
for var in iterator(:)
  code

### Enumerated for
#### Python style
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
func function\_name(arg1: arg1\_type): return\_type
  code

func return\_type function\_name(arg1: arg1\_type)
  code

func function\_name(arg1: arg1\_type) -> return\_type (:)
  code

function\_name :: arg1\_type -> return\_type
def function\_name(arg1)
  code

### Call
function\_name(arg1)


