# Syntax Proposals

## Variables
### Immutable variable declaration
type identifier

### Mutable variable declaration
mut type identifier

## Variable assignment
identifier = expression

## Conditionals
if expression
  code
elif expression
  code
else
  code

## Flow Control

### For each
for var in iterator
  code

### Enumerated for
for i, var in enumerated(iterator)
  code

### While
while condition
  code

### Switch
switch varname
  value1
    code
  value2
    code
  value3
    code

## Functions

### Declaration
func function\_name(argtype argname, argtype2 argname2, ...vartype varargs, kwtype kwname=value, kwtype2 kwname2=value2) return\_type
  code

### Call
function\_name(arg1)

### Returns
func function\_name(argtype argname) return\_type
  code
  return foo

### Lambdas
(argtype1 argname1, argtype2 argname2) => expr

