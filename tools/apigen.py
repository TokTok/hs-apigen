from dataclasses import dataclass
from enum import Enum
from typing import Optional


class Generated(Enum):
    GeneratedToString = 0
    GeneratedFromInt = 1


class LexemeClass(Enum):
    IdVar = 0
    IdConst = 1
    IdSueType = 2
    IdFuncType = 3
    LitInteger = 4


class CType:
    pass


@dataclass
class BasicType(CType):
    name: str


Void = BasicType("Void")
VoidPtr = BasicType("VoidPtr")
Bool = BasicType("Bool")
Char = BasicType("Char")
SizeT = BasicType("SizeT")
String = BasicType("String")


@dataclass
class SInt(CType):
    bitSize: int


@dataclass
class UInt(CType):
    bitSize: int


@dataclass
class Name:
    kind: LexemeClass
    ns: list[str]
    name: list[str]


class Decl:
    pass


class Type(Decl):
    pass


@dataclass
class Namespace(Decl):
    name: list[str]
    mems: list[Decl]


@dataclass
class ClassDecl(Decl):
    name: Name
    mems: list[Decl]


@dataclass
class EnumMember(Decl):
    name: Name


@dataclass
class Enumeration(Decl):
    funs: list[Generated]
    name: Name
    mems: list[EnumMember]


@dataclass
class Var(Decl):
    type: Type
    name: Name


@dataclass
class Method(Decl):
    const: bool
    ret: Decl
    name: Name
    params: list[Var]


@dataclass
class Function(Decl):
    ret: Decl
    name: Name
    params: list[Var]


@dataclass
class Constructor(Decl):
    name: Name
    params: list[Var]


@dataclass
class Destructor(Decl):
    name: Name
    params: list[Var]


@dataclass
class CallbackTypeDecl(Decl):
    name: Name
    params: list[Var]


@dataclass
class IdTypeDecl(Decl):
    name: Name


@dataclass
class TypeDecl(Decl):
    name: Name


@dataclass
class Define(Decl):
    name: Name


@dataclass
class Typename(Type):
    name: Name


@dataclass
class BuiltinType(Type):
    type: CType


@dataclass
class CallbackType(Type):
    type: Name


@dataclass
class PointerType(Type):
    type: Name


@dataclass
class ConstPointerType(Type):
    type: Name


@dataclass
class SizedArrayType(Type):
    type: Type
    size: Decl


@dataclass
class ArrayType(Type):
    type: CType


@dataclass
class UserArrayType(Type):
    type: Name


@dataclass
class ConstArrayType(Type):
    type: CType


@dataclass
class ConstType(Type):
    type: Type


@dataclass
class Prop(Decl):
    type: Type
    get: Optional[Method]
    set: Optional[Method]


@dataclass
class ValueProp(Prop):
    pass


@dataclass
class ArrayProp(Prop):
    size: Optional[Method]


@dataclass
class Property(Decl):
    name: Name
    prop: Prop


class Expr(Decl):
    pass


@dataclass
class Ref(Expr):
    name: Name


@dataclass
class Paren(Expr):
    expr: Expr


@dataclass
class IntVal(Expr):
    val: Name


@dataclass
class Abs(Expr):
    expr: Expr


@dataclass
class Max(Expr):
    arg1: Expr
    arg2: Expr


@dataclass
class Add(Expr):
    left: Expr
    right: Expr


@dataclass
class Sub(Expr):
    left: Expr
    right: Expr


@dataclass
class Mul(Expr):
    left: Expr
    right: Expr


@dataclass
class Div(Expr):
    left: Expr
    right: Expr


@dataclass
class Module:
    path: str
    decls: list[Namespace]


@dataclass
class Model:
    modules: list[Module]
