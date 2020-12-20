decl product(a, b) -> Product<a , b>;
decl emptyList() -> List<a>;
decl lengthList(List<a>) -> Int;
decl emptyString() -> List<String>;

decl ptr() -> Pointer<Pointer<Int>>;
decl deref(Pointer<a>) -> a;

lengthList(emptyString());
deref(deref(ptr()));
lengthList;
