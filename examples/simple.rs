decl product(a, b) -> Product<a , b>;
decl emptyList() -> List<a>;
decl lengthList(List<a>) -> Int;
decl emptyString() -> List<String>;

lengthList(emptyString());
lengthList(emptyList());
product(emptyList(), emptyList());
