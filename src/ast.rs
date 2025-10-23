use crate::diagnostics::SourceSpan;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub name: String,
    pub annotation: Option<TypeExpr>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeExpr {
    pub name: String,
    pub generics: Vec<TypeExpr>,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: SourceSpan,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Literal(Literal),
    Variable(String),
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    ArrayLiteral(Vec<Expr>),
    MapLiteral(Vec<(Expr, Expr)>),
    TupleLiteral(Vec<Expr>),
    Group(Box<Expr>),
    Index {
        target: Box<Expr>,
        index: Box<Expr>,
    },
    Field {
        target: Box<Expr>,
        field: String,
    },
    Lambda {
        params: Vec<FunctionParam>,
        body: Vec<Stmt>,
    },
    Await(Box<Expr>),
    Try(Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct WhenArm {
    pub pattern: Pattern,
    pub body: Vec<Stmt>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Literal(Literal),
    Identifier(String),
    Tuple(Vec<Pattern>),
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    VarDecl {
        name: String,
        mutable: bool,
        annotation: Option<TypeExpr>,
        initializer: Option<Expr>,
    },
    ConstDecl {
        name: String,
        annotation: Option<TypeExpr>,
        value: Expr,
    },
    Function {
        name: String,
        params: Vec<FunctionParam>,
        return_type: Option<TypeExpr>,
        body: Vec<Stmt>,
        is_async: bool,
    },
    Module {
        name: Vec<String>,
        items: Vec<Stmt>,
    },
    Use {
        path: Vec<String>,
        alias: Option<String>,
    },
    Expr(Expr),
    Block(Vec<Stmt>),
    If {
        condition: Expr,
        then_branch: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
    },
    While {
        condition: Expr,
        body: Vec<Stmt>,
    },
    Loop {
        body: Vec<Stmt>,
    },
    For {
        binding: String,
        iterable: Expr,
        body: Vec<Stmt>,
    },
    When {
        subject: Expr,
        arms: Vec<WhenArm>,
    },
    Return(Option<Expr>),
    Break(Option<Expr>),
    Continue,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: SourceSpan,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: Option<Vec<String>>,
    pub items: Vec<Stmt>,
}
