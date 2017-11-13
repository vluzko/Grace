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

### Do While
do
  code
while condition

### Switch

switch varname
  value1
    code
  value2
    code
  value3
    code

### Match
match varname
  value1 => expr
  value2 => expr2
  default => dexpr

## Functions

### Declaration
func function\_name(argtype argname, argtype2 argname2, ...varargs, kwtype kwname=value, kwtype2 kwname2=value2, {...} kwargs) return\_type
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





