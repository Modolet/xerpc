use chumsky::prelude::*;
use chumsky::Parser;

use crate::token::Token;

// 定义 AST 节点类型
/// Root 解析器
pub fn root_parser() -> impl Parser<Token, Root, Error = Simple<Token>> {
    // 将所有顶级解析器组合在一起
    let service = service_parser().map(RootItem::Service);
    let enm = enum_parser().map(RootItem::Enum);
    let structure = struct_parser().map(RootItem::Struct);
    let constant = constant_parser().map(RootItem::Constant);
    let callback = callback_parser().map(RootItem::Callback);
    let interface = interface_parser().map(RootItem::Interface);

    // 解析顶级节点，忽略空行或注释
    let root_item = service
        .or(enm)
        .or(structure)
        .or(constant)
        .or(callback)
        .or(interface);

    // 重复解析所有顶级节点
    root_item.repeated().map(|items| {
        let mut services = Vec::new();
        let mut enums = Vec::new();
        let mut structs = Vec::new();
        let mut constants = Vec::new();
        let mut callbacks = Vec::new();
        let mut interfaces = Vec::new();

        // 将解析结果分类到对应的字段中
        for item in items {
            match item {
                RootItem::Service(service) => services.push(service),
                RootItem::Enum(enm) => enums.push(enm),
                RootItem::Struct(structure) => structs.push(structure),
                RootItem::Constant(constant) => constants.push(constant),
                RootItem::Callback(callback) => callbacks.push(callback),
                RootItem::Interface(interface) => interfaces.push(interface),
            }
        }

        Root {
            services,
            enums,
            structs,
            constants,
            callbacks,
            interfaces,
        }
    })
}
/// 定义顶级节点的枚举类型，用于分类解析结果
#[derive(Debug, PartialEq)]
enum RootItem {
    Service(Service),
    Enum(Enum),
    Struct(Struct),
    Constant(Constant),
    Callback(Callback),
    Interface(Interface),
}

#[derive(Debug, PartialEq)]
pub struct Root {
    pub services: Vec<Service>,
    pub enums: Vec<Enum>,
    pub structs: Vec<Struct>,
    pub constants: Vec<Constant>,
    pub callbacks: Vec<Callback>,
    pub interfaces: Vec<Interface>,
}

/// 方法的定义
#[derive(Debug, PartialEq)]
pub struct Method {
    pub doc: Option<String>,         // 方法的文档注释
    pub name: String,                // 方法名称
    pub params: Vec<(String, Type)>, // 方法参数列表 (参数名, 参数类型)
    pub return_type: Type,           // 返回值类型
}

/// 接口的定义
#[derive(Debug, PartialEq)]
pub struct Interface {
    pub doc: Option<String>,  // 接口的文档注释
    pub name: String,         // 接口名称
    pub service: String,      // 继承的服务名称
    pub methods: Vec<Method>, // 方法列表
}

/// 解析方法
fn method_parser() -> impl Parser<Token, Method, Error = Simple<Token>> {
    doc_comment()
        .then_ignore(just(Token::Fn)) // 跳过 `fn` 关键字
        .then(ident()) // 方法名称
        .then(
            just(Token::LeftParen)
                .ignore_then(
                    ident().then(ty()).separated_by(just(Token::Comma)), // 参数列表
                )
                .then_ignore(just(Token::RightParen)),
        ) // 方法参数
        .then_ignore(just(Token::Arrow)) // 跳过 `->`
        .then(ty()) // 返回值类型
        .then_ignore(just(Token::Semicolon).or_not()) // 可选分号
        .map(|(((doc, name), params), return_type)| Method {
            doc,
            name,
            params,
            return_type,
        })
}

/// 解析接口
pub fn interface_parser() -> impl Parser<Token, Interface, Error = Simple<Token>> {
    doc_comment()
        .then_ignore(just(Token::Interface)) // 跳过 `interface` 关键字
        .then(ident()) // 接口名称
        .then_ignore(just(Token::Colon))
        .then(ident()) // 继承的服务名称
        .then_ignore(just(Token::LeftBrace)) // 跳过左大括号
        .then(method_parser().repeated()) // 方法列表
        .then_ignore(just(Token::RightBrace)) // 跳过右大括号
        .map(|(((doc, name), service), methods)| Interface {
            doc,
            name,
            service,
            methods,
        })
}

/// 常量右值的可能类型
#[derive(Debug, PartialEq)]
pub enum ConstantValue {
    Number(i128),              // 整数
    Float(f64),                // 浮点数
    Bool(bool),                // 布尔值
    Struct(StructInit),        // 结构体初始化
    Enum(EnumInit),            // 枚举初始化
    Array(Vec<ConstantValue>), // 数组初始化
    String(String),            // 字符串
}

/// 服务
#[derive(Debug, PartialEq)]
pub struct Service {
    pub name: String,
    pub doc: Option<String>,
}
/// 结构体初始化
#[derive(Debug, PartialEq)]
pub struct StructInit {
    pub fields: Vec<(String, ConstantValue)>, // 字段列表
}

/// 枚举初始化
#[derive(Debug, PartialEq)]
pub struct EnumInit {
    pub name: String,             // 枚举名称
    pub variant: String,          // 枚举变体名称
    pub args: Vec<ConstantValue>, // 枚举参数
}

/// 常量定义
#[derive(Debug, PartialEq)]
pub struct Constant {
    pub name: String,         // 常量名称
    pub ty: Type,             // 常量类型
    pub value: ConstantValue, // 常量右值
    pub doc: Option<String>,  // 文档注释
}

/// 枚举变体
#[derive(Debug, PartialEq)]
pub struct EnumVariant {
    pub name: String,        // 枚举变体名称
    pub args: Vec<Type>,     // 可选参数类型列表
    pub doc: Option<String>, // 文档注释
}

/// 枚举声明
#[derive(Debug, PartialEq)]
pub struct Enum {
    pub name: String,               // 枚举名称
    pub variants: Vec<EnumVariant>, // 枚举变体列表
    pub doc: Option<String>,        // 文档注释
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Primitive(String),
    Custom(String),
    Array(Box<Type>, Option<usize>),
    Callback(String),
    Void,
}

#[derive(Debug, PartialEq)]
pub struct StructField {
    pub name: String,
    pub ty: Type,
    pub doc: Option<String>,
}

#[derive(Debug, PartialEq)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
    pub doc: Option<String>,
}

/// 回调声明
#[derive(Debug, PartialEq)]
pub struct Callback {
    pub name: String,        // 回调名称
    pub params: Vec<Type>,   // 参数类型列表
    pub ret: Option<Type>,   // 返回类型
    pub doc: Option<String>, // 文档注释
}

/// 回调解析器
pub fn callback_parser() -> impl Parser<Token, Callback, Error = Simple<Token>> {
    doc_comment()
        .then_ignore(just(Token::Callback)) // 跳过 `callback` 关键字
        .then(ident()) // 回调名称
        .then(
            just(Token::LeftParen) // 参数列表
                .ignore_then(ty().separated_by(just(Token::Comma)))
                .then_ignore(just(Token::RightParen)),
        )
        .then(
            just(Token::Arrow) // 返回类型（可选）
                .ignore_then(ty())
                .or_not(),
        )
        .then_ignore(just(Token::Semicolon).or_not()) // 跳过分号，允许省略
        .map(|(((doc, name), params), ret)| Callback {
            name,
            params,
            ret,
            doc,
        })
}

// 标识符解析器
fn ident() -> impl Parser<Token, String, Error = Simple<Token>> {
    select! { Token::Identifier(name) => name.to_string() }.labelled("ident")
}

/// 枚举变体解析器
fn enum_variant() -> impl Parser<Token, EnumVariant, Error = Simple<Token>> {
    doc_comment()
        .then(ident()) // 枚举变体名称
        .then(
            just(Token::LeftParen) // 如果有参数
                .ignore_then(ty().separated_by(just(Token::Comma))) // 参数类型列表
                .then_ignore(just(Token::RightParen))
                .or_not(),
        )
        .map(|((doc, name), args)| EnumVariant {
            name,
            args: args.unwrap_or_default(), // 如果没有参数，则为空列表
            doc,
        })
}

/// 枚举声明解析器
pub fn enum_parser() -> impl Parser<Token, Enum, Error = Simple<Token>> {
    doc_comment()
        .then_ignore(just(Token::Enum)) // 跳过 `enum` 关键字
        .then(ident()) // 枚举名称
        .then(
            enum_variant()
                .separated_by(just(Token::Comma)) // 枚举变体用逗号分隔
                .allow_trailing() // 允许尾逗号
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
        )
        .map(|((doc, name), variants)| Enum {
            name,
            variants,
            doc,
        })
}

// 基本类型解析器
fn primitive_type() -> impl Parser<Token, Type, Error = Simple<Token>> {
    select! {
        Token::U8 => Type::Primitive("u8".to_string()),
        Token::I8 => Type::Primitive("i8".to_string()),
        Token::U16 => Type::Primitive("u16".to_string()),
        Token::I16 => Type::Primitive("i16".to_string()),
        Token::U32 => Type::Primitive("u32".to_string()),
        Token::I32 => Type::Primitive("i32".to_string()),
        Token::U64 => Type::Primitive("u64".to_string()),
        Token::I64 => Type::Primitive("i64".to_string()),
        Token::F32 => Type::Primitive("f32".to_string()),
        Token::F64 => Type::Primitive("f64".to_string()),
        Token::Bool => Type::Primitive("bool".to_string()),
        Token::Str => Type::Primitive("str".to_string()),
        Token::Void => Type::Primitive("void".to_string())
    }
    .labelled("primitive_type")
}

// 类型解析器
fn ty() -> impl Parser<Token, Type, Error = Simple<Token>> {
    recursive(|ty| {
        let array_ty = just(Token::LeftBracket)
            .ignore_then(ty.clone())
            .then(
                just(Token::Semicolon)
                    .ignore_then(select! { Token::Integer(size) => size as usize })
                    .or_not(),
            )
            .then_ignore(just(Token::RightBracket))
            .map(|(ty, size)| Type::Array(Box::new(ty), size));

        array_ty
            .or(primitive_type())
            .or(ident().map(Type::Custom)) // 自定义类型
            .labelled("type")
    })
}

// 文档注释解析器
fn doc_comment() -> impl Parser<Token, Option<String>, Error = Simple<Token>> {
    select! {
        Token::DocLineComment(doc) => doc,
    }
    .repeated()
    .at_least(0)
    .map(|docs: Vec<String>| {
        if docs.is_empty() {
            None
        } else {
            Some(docs.join("\n"))
        }
    })
}

// 结构体字段解析器
fn struct_field() -> impl Parser<Token, StructField, Error = Simple<Token>> {
    doc_comment()
        .then(ident()) // 字段名称
        .then_ignore(just(Token::Colon)) // 跳过冒号
        .then(ty()) // 字段类型
        .map(|((doc, name), ty)| StructField { name, ty, doc })
}

// 结构体解析器
pub fn struct_parser() -> impl Parser<Token, Struct, Error = Simple<Token>> {
    let fields = struct_field()
        .separated_by(just(Token::Comma)) // 用逗号分隔字段
        .allow_trailing() // 允许末尾逗号
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)); // 用花括号包裹字段

    doc_comment()
        .then_ignore(just(Token::Struct)) // 跳过 `struct` 关键字
        .then(ident()) // 解析结构体名称
        .then(fields) // 解析字段列表
        .map(|((doc, name), fields)| Struct { name, fields, doc })
}

// 常量解析器
pub fn constant_parser() -> impl Parser<Token, Constant, Error = Simple<Token>> {
    doc_comment()
        .then_ignore(just(Token::Const)) // 跳过 `const` 关键字
        .then(ident()) // 常量名称
        .then_ignore(just(Token::Colon)) // 跳过冒号
        .then(ty()) // 常量类型
        .then_ignore(just(Token::Equals)) // 跳过等号
        .then(expr()) // 常量右值
        .then_ignore(just(Token::Semicolon).or_not()) // 跳过分号，允许省略
        .map(|(((doc, name), ty), value)| Constant {
            name,
            ty,
            value,
            doc,
        })
}

// 服务解析器
pub fn service_parser() -> impl Parser<Token, Service, Error = Simple<Token>> {
    doc_comment()
        .then_ignore(just(Token::Service))
        .then(ident())
        .then_ignore(just(Token::Semicolon).or_not()) // 跳过分号，允许省略
        .map(|(doc, name)| Service { name, doc })
}

// 右值解析器
fn expr() -> impl Parser<Token, ConstantValue, Error = Simple<Token>> {
    recursive(|expr| {
        let struct_init = just(Token::LeftBrace)
            .ignore_then(
                ident()
                    .then_ignore(just(Token::Colon)) // 字段名
                    .then(expr.clone()) // 字段值
                    .separated_by(just(Token::Comma)),
            )
            .then_ignore(just(Token::RightBrace))
            .map(|fields| ConstantValue::Struct(StructInit { fields }));

        let enum_init = ident() // 枚举初始化
            .then_ignore(just(Token::DoubleColon)) // 跳过 `::`
            .then(ident()) // 枚举变体名称
            .then(
                just(Token::LeftParen)
                    .ignore_then(expr.clone().separated_by(just(Token::Comma))) // 参数列表
                    .then_ignore(just(Token::RightParen))
                    .or_not(),
            )
            .map(|((name, variant), args)| {
                ConstantValue::Enum(EnumInit {
                    name,
                    variant,
                    args: args.unwrap_or_default(),
                })
            });

        let array_init = just(Token::LeftBracket) // 数组初始化
            .ignore_then(expr.clone().separated_by(just(Token::Comma)))
            .then_ignore(just(Token::RightBracket))
            .map(ConstantValue::Array);

        struct_init.or(enum_init).or(array_init).or(select! {
            Token::Integer(value) => ConstantValue::Number(value),
            Token::Float(value) => ConstantValue::Float(value.parse().unwrap()),
            Token::String(value) => ConstantValue::String(value),
            Token::True => ConstantValue::Bool(true),
            Token::False => ConstantValue::Bool(false),
        })
    })
}

#[cfg(test)]
mod test {
    #[allow(unused_imports)]
    use super::*;
    #[allow(unused_imports)]
    use chumsky::prelude::*;
    #[allow(unused_imports)]
    use chumsky::Stream;
    #[allow(unused_imports)]
    use logos::Logos;

    fn get_tokens(input: &str) -> Vec<Token> {
        Token::lexer(input)
            .spanned()
            .map(|(token, _)| token.unwrap())
            .filter(|x| {
                !matches!(
                    x,
                    Token::SingleLineComment(_) | Token::NewLine | Token::BlockComment(_)
                )
            })
            .collect::<Vec<_>>()
    }

    #[test]
    fn test_struct_field() {
        let input = r#"
            /// doc
            name: u8;
        "#;
        let tokens = get_tokens(input);
        let result = struct_field().parse(tokens);
        assert_eq!(
            result,
            Ok(StructField {
                name: "name".to_string(),
                ty: Type::Primitive("u8".to_string()),
                doc: Some(" doc\n".to_string())
            })
        );
    }

    #[test]
    fn test_struct_parser() {
        let input = r#"
            /// User struct
            struct User {
                name: str,
                age: u32,
            }
        "#;
        let tokens = get_tokens(input);
        let result = struct_parser().parse(tokens);
        assert_eq!(
            result,
            Ok(Struct {
                name: "User".to_string(),
                fields: vec![
                    StructField {
                        name: "name".to_string(),
                        ty: Type::Primitive("str".to_string()),
                        doc: None,
                    },
                    StructField {
                        name: "age".to_string(),
                        ty: Type::Primitive("u32".to_string()),
                        doc: None,
                    },
                ],
                doc: Some(" User struct\n".to_string()),
            })
        );
    }

    #[test]
    fn test_constant_parser() {
        let input = r#"
            /// Constant A
            const A: u32 = 32;
        "#;
        let tokens = get_tokens(input);
        let result = constant_parser().parse(tokens);
        assert_eq!(
            result,
            Ok(Constant {
                name: "A".to_string(),
                ty: Type::Primitive("u32".to_string()),
                value: ConstantValue::Number(32),
                doc: Some(" Constant A\n".to_string()),
            })
        );
    }

    #[test]
    fn test_enum_parser() {
        let input = r#"
            /// Color enum
            enum Color {
                Red(u8, u8),
                Green,
                Blue, // 尾逗号
            }
        "#;
        let tokens = get_tokens(input);
        let result = enum_parser().parse(tokens);
        assert_eq!(
            result,
            Ok(Enum {
                name: "Color".to_string(),
                variants: vec![
                    EnumVariant {
                        name: "Red".to_string(),
                        args: vec![
                            Type::Primitive("u8".to_string()),
                            Type::Primitive("u8".to_string())
                        ],
                        doc: None,
                    },
                    EnumVariant {
                        name: "Green".to_string(),
                        args: vec![],
                        doc: None,
                    },
                    EnumVariant {
                        name: "Blue".to_string(),
                        args: vec![],
                        doc: None,
                    },
                ],
                doc: Some(" Color enum\n".to_string()),
            })
        );
    }

    #[test]
    fn test_callback_parser() {
        let input = r#"
            /// Callback example
            callback Example(u32, bool) -> str;
        "#;
        let tokens = get_tokens(input);
        let result = callback_parser().parse(tokens);
        assert_eq!(
            result,
            Ok(Callback {
                name: "Example".to_string(),
                params: vec![
                    Type::Primitive("u32".to_string()),
                    Type::Primitive("bool".to_string())
                ],
                ret: Some(Type::Primitive("str".to_string())),
                doc: Some(" Callback example\n".to_string()),
            })
        );
    }

    #[test]
    fn test_expr_struct_init() {
        let input = r#"
            { x: 10, y: 20 }
        "#;
        let tokens = get_tokens(input);
        let result = expr().parse(tokens);
        assert_eq!(
            result,
            Ok(ConstantValue::Struct(StructInit {
                fields: vec![
                    ("x".to_string(), ConstantValue::Number(10)),
                    ("y".to_string(), ConstantValue::Number(20)),
                ],
            }))
        );
    }

    #[test]
    fn test_expr_enum_init() {
        let input = r#"
            Color::Red(255, 0, 0)
        "#;
        let tokens = get_tokens(input);
        let result = expr().parse(tokens);
        assert_eq!(
            result,
            Ok(ConstantValue::Enum(EnumInit {
                name: "Color".to_string(),
                variant: "Red".to_string(),
                args: vec![
                    ConstantValue::Number(255),
                    ConstantValue::Number(0),
                    ConstantValue::Number(0),
                ],
            }))
        );
    }

    #[test]
    fn test_constant_array_with_fixed_size() {
        let input = r#"
            const NUMS: [i32; 5] = [1, 2, 3, 4, 5];
        "#;
        let tokens = get_tokens(input);
        let result = constant_parser().parse(tokens);
        assert_eq!(
            result,
            Ok(Constant {
                name: "NUMS".to_string(),
                ty: Type::Array(Box::new(Type::Primitive("i32".to_string())), Some(5)),
                value: ConstantValue::Array(vec![
                    ConstantValue::Number(1),
                    ConstantValue::Number(2),
                    ConstantValue::Number(3),
                    ConstantValue::Number(4),
                    ConstantValue::Number(5),
                ]),
                doc: None,
            })
        );
    }

    #[test]
    fn test_constant_array_without_fixed_size() {
        let input = r#"
            const NUMS: [i32] = [1, 2, 3, 4, 5];
        "#;
        let tokens = get_tokens(input);
        let result = constant_parser().parse(tokens);
        assert_eq!(
            result,
            Ok(Constant {
                name: "NUMS".to_string(),
                ty: Type::Array(Box::new(Type::Primitive("i32".to_string())), None),
                value: ConstantValue::Array(vec![
                    ConstantValue::Number(1),
                    ConstantValue::Number(2),
                    ConstantValue::Number(3),
                    ConstantValue::Number(4),
                    ConstantValue::Number(5),
                ]),
                doc: None,
            })
        );
    }

    #[test]
    fn test_interface_parser() {
        let input = r#"
            /// rpc的文档注释，只能在头部
            interface UserInterface: UserService {
                /// get_user的文档注释
                fn get_user(userid i32) -> User
                fn get_users() -> [User] 
                fn add_user(user User) -> i32
                fn del_user(userid i32) -> i32; // 末尾的分号可选
                fn heartbeat(mycallback fncallback) -> void
            }
        "#;

        let tokens = get_tokens(input);
        let result = interface_parser().parse(tokens);

        assert_eq!(
            result,
            Ok(Interface {
                doc: Some(" rpc的文档注释，只能在头部\n".to_string()),
                name: "UserInterface".to_string(),
                service: "UserService".to_string(),
                methods: vec![
                    Method {
                        doc: Some(" get_user的文档注释\n".to_string()),
                        name: "get_user".to_string(),
                        params: vec![("userid".to_string(), Type::Primitive("i32".to_string()))],
                        return_type: Type::Custom("User".to_string()),
                    },
                    Method {
                        doc: None,
                        name: "get_users".to_string(),
                        params: vec![],
                        return_type: Type::Array(Box::new(Type::Custom("User".to_string())), None),
                    },
                    Method {
                        doc: None,
                        name: "add_user".to_string(),
                        params: vec![("user".to_string(), Type::Custom("User".to_string()))],
                        return_type: Type::Primitive("i32".to_string()),
                    },
                    Method {
                        doc: None,
                        name: "del_user".to_string(),
                        params: vec![("userid".to_string(), Type::Primitive("i32".to_string()))],
                        return_type: Type::Primitive("i32".to_string()),
                    },
                    Method {
                        doc: None,
                        name: "heartbeat".to_string(),
                        params: vec![(
                            "mycallback".to_string(),
                            Type::Custom("fncallback".to_string())
                        )],
                        return_type: Type::Primitive("void".to_string()),
                    },
                ],
            })
        );
    }

    #[test]
    fn test_root_parser() {
        let input = r#"
        /// Service documentation
        service UserService;

        /// Color enum
        enum Color {
            Red(u8, u8),
            Green,
            Blue,
        }

        /// User struct
        struct User {
            name: str,
            age: u32,
        }

        /// Constant A
        const A: u32 = 32;

        /// Callback example
        callback Example(u32, bool) -> str;

        /// rpc的文档注释，只能在头部
        interface UserInterface: UserService {
            /// get_user的文档注释
            fn get_user(userid i32) -> User
            fn get_users() -> [User] 
            fn add_user(user User) -> i32
            fn del_user(userid i32) -> i32; // 末尾的分号可选
            fn heartbeat(mycallback fncallback) -> void
        }
    "#;

        let tokens = get_tokens(input);
        let result = root_parser().parse(tokens);

        assert_eq!(
            result,
            Ok(Root {
                services: vec![Service {
                    name: "UserService".to_string(),
                    doc: Some(" Service documentation\n".to_string()),
                }],
                enums: vec![Enum {
                    name: "Color".to_string(),
                    variants: vec![
                        EnumVariant {
                            name: "Red".to_string(),
                            args: vec![
                                Type::Primitive("u8".to_string()),
                                Type::Primitive("u8".to_string())
                            ],
                            doc: None,
                        },
                        EnumVariant {
                            name: "Green".to_string(),
                            args: vec![],
                            doc: None,
                        },
                        EnumVariant {
                            name: "Blue".to_string(),
                            args: vec![],
                            doc: None,
                        },
                    ],
                    doc: Some(" Color enum\n".to_string()),
                }],
                structs: vec![Struct {
                    name: "User".to_string(),
                    fields: vec![
                        StructField {
                            name: "name".to_string(),
                            ty: Type::Primitive("str".to_string()),
                            doc: None,
                        },
                        StructField {
                            name: "age".to_string(),
                            ty: Type::Primitive("u32".to_string()),
                            doc: None,
                        },
                    ],
                    doc: Some(" User struct\n".to_string()),
                }],
                constants: vec![Constant {
                    name: "A".to_string(),
                    ty: Type::Primitive("u32".to_string()),
                    value: ConstantValue::Number(32),
                    doc: Some(" Constant A\n".to_string()),
                }],
                callbacks: vec![Callback {
                    name: "Example".to_string(),
                    params: vec![
                        Type::Primitive("u32".to_string()),
                        Type::Primitive("bool".to_string())
                    ],
                    ret: Some(Type::Primitive("str".to_string())),
                    doc: Some(" Callback example\n".to_string()),
                }],
                interfaces: vec![Interface {
                    doc: Some(" rpc的文档注释，只能在头部\n".to_string()),
                    name: "UserInterface".to_string(),
                    service: "UserService".to_string(),
                    methods: vec![
                        Method {
                            doc: Some(" get_user的文档注释\n".to_string()),
                            name: "get_user".to_string(),
                            params: vec![(
                                "userid".to_string(),
                                Type::Primitive("i32".to_string())
                            )],
                            return_type: Type::Custom("User".to_string()),
                        },
                        Method {
                            doc: None,
                            name: "get_users".to_string(),
                            params: vec![],
                            return_type: Type::Array(
                                Box::new(Type::Custom("User".to_string())),
                                None
                            ),
                        },
                        Method {
                            doc: None,
                            name: "add_user".to_string(),
                            params: vec![("user".to_string(), Type::Custom("User".to_string()))],
                            return_type: Type::Primitive("i32".to_string()),
                        },
                        Method {
                            doc: None,
                            name: "del_user".to_string(),
                            params: vec![(
                                "userid".to_string(),
                                Type::Primitive("i32".to_string())
                            )],
                            return_type: Type::Primitive("i32".to_string()),
                        },
                        Method {
                            doc: None,
                            name: "heartbeat".to_string(),
                            params: vec![(
                                "mycallback".to_string(),
                                Type::Custom("fncallback".to_string())
                            )],
                            return_type: Type::Primitive("void".to_string()),
                        },
                    ],
                }],
            })
        );
    }

    #[test]
    fn my_test() {
        let input = r#"
            const A:User = { name: "123", age: 123 };
        "#;
        let tokens = get_tokens(input);
        dbg!(&tokens);
        let result = constant_parser().parse(tokens);
        dbg!(result);
    }
}
