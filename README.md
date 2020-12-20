# Unification

An implementation of unification for type-checking polymorphic functions.
Implemented with reference to Chapter 6 of the dragon book, edition 1.

##### Internal state of the unification algorithm

```
==inferring |deref(deref(ptr()))|==
	-{0} deref
	-{1} deref[» [π [Pointer [αti]]] [αti]]
	-{0} deref
	-{1} deref[» [π [Pointer [αzi]]] [αzi]]
	-{0} ptr
	-{1} ptr[» [π] [Pointer [Pointer [Int]]]]
	-{0} ptr[» [π] [Pointer [Pointer [Int]]]]():[αko]
	-{u} [» [π] [αko]]~[» [π] [Pointer [Pointer [Int]]]]
		-{u} [π]~[π]
		-{v} [π]~[π] | cons
		-{u} [αko]~[Pointer [Pointer [Int]]]
		-{v} [Pointer [Pointer [Int]]]~[Pointer [Pointer [Int]]] | lvar
	-{v} [» [π] [Pointer [Pointer [Int]]]]~[» [π] [Pointer [Pointer [Int]]]] | cons
	-{1} ptr[» [π] [Pointer [Pointer [Int]]]]():[Pointer [Pointer [Int]]]
	-{0} deref[» [π [Pointer [αzi]]] [αzi]](ptr[» [π] [Pointer [Pointer [Int]]]]():[Pointer [Pointer [Int]]]):[αno]
	-{u} [» [π [Pointer [Pointer [Int]]]] [αno]]~[» [π [Pointer [αzi]]] [αzi]]
		-{u} [π [Pointer [Pointer [Int]]]]~[π [Pointer [αzi]]]
			-{u} [Pointer [Pointer [Int]]]~[Pointer [αzi]]
				-{u} [Pointer [Int]]~[αzi]
				-{v} [Pointer [Int]]~[Pointer [Int]] | rvar
			-{v} [Pointer [Pointer [Int]]]~[Pointer [Pointer [Int]]] | cons
		-{v} [π [Pointer [Pointer [Int]]]]~[π [Pointer [Pointer [Int]]]] | cons
		-{u} [αno]~[Pointer [Int]]
		-{v} [Pointer [Int]]~[Pointer [Int]] | lvar
	-{v} [» [π [Pointer [Pointer [Int]]]] [Pointer [Int]]]~[» [π [Pointer [Pointer [Int]]]] [Pointer [Int]]] | cons
	-{1} deref[» [π [Pointer [Pointer [Int]]]] [Pointer [Int]]](ptr[» [π] [Pointer [Pointer [Int]]]]():[Pointer [Pointer [Int]]]):[Pointer [Int]]
	-{0} deref[» [π [Pointer [αti]]] [αti]](deref[» [π [Pointer [Pointer [Int]]]] [Pointer [Int]]](ptr[» [π] [Pointer [Pointer [Int]]]]():[Pointer [Pointer [Int]]]):[Pointer [Int]]):[αro]
	-{u} [» [π [Pointer [Int]]] [αro]]~[» [π [Pointer [αti]]] [αti]]
		-{u} [π [Pointer [Int]]]~[π [Pointer [αti]]]
			-{u} [Pointer [Int]]~[Pointer [αti]]
				-{u} [Int]~[αti]
				-{v} [Int]~[Int] | rvar
			-{v} [Pointer [Int]]~[Pointer [Int]] | cons
		-{v} [π [Pointer [Int]]]~[π [Pointer [Int]]] | cons
		-{u} [αro]~[Int]
		-{v} [Int]~[Int] | lvar
	-{v} [» [π [Pointer [Int]]] [Int]]~[» [π [Pointer [Int]]] [Int]] | cons
	-{1} deref[» [π [Pointer [Int]]] [Int]](deref[» [π [Pointer [Pointer [Int]]]] [Pointer [Int]]](ptr[» [π] [Pointer [Pointer [Int]]]]():[Pointer [Pointer [Int]]]):[Pointer [Int]]):[Int]
```
