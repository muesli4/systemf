## Usage

Single terms can be passed to the executable for type checking. For the grammar see the listing below.

## Grammar

### Terms

```
term  ::= unit | var | abs | tabs | app | tapp
unit  ::= 'U'
var   ::= nat
abs   ::= 'abs' '#' type '.' term
tabs  ::= 'tabs' '.' term
app   ::= term+
tapp  ::= '[' term ' ' type ']'
```

### Types
```
type  ::= unit | var | poly | arrow
unit  ::= 'U'
var   ::= nat
poly  ::= 'forall' '.' type
arrow ::= type ('->' type)+
```

## Example terms

Monmorphic identity function:
```
abs#U.0
```

Monomorphic constant function:
```
abs#U.abs#U.1
```

Application of terms:
```
(abs#U.0) abs#U.0
```

Polymorphic identity function:
```
tabs.abs#0.0
```

Type application:
```
[(tabs.abs#0.0) U]
```

Higher-kinded parameter:
```
abs#U -> U -> U.0 U U
```

Polymorphic parameter:
```
abs#(forall.0 -> U).abs#forall.0.1 0
```

