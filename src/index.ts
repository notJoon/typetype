// Type variable (α, β, γ, ...)
export type TypeVar<Name extends string> = {
    kind: 'TypeVar';
    name: Name;
}

// Type constructor (Int, Bool, List, Function, etc.)
export type TypeCon<Name extends string> = {
    kind: 'TypeCon';
    name: Name;
}

// Type application (T1 T2)
export type TypeApp<Fun, Arg> = {
    kind: 'TypeApp';
    fun: Fun;
    arg: Arg;
}

// Function type (From -> To)
export type FunType<From, To> = TypeApp<TypeApp<TypeCon<'->'>, From>, To>;

// Basic types
export type IntType = TypeCon<'Int'>;
export type BoolType = TypeCon<'Bool'>;
export type ListType<T> = TypeApp<TypeCon<'List'>, T>;

// Type schemes (∀α.τ) - Polymorphic Types
type TypeScheme<Vars extends readonly string[], Type> = {
    kind: 'TypeSchema';
    vars: Vars;
    type: Type;
}

// Type environment (Γ)
type TypeEnv = Record<string, any>;

type Substitution = Record<string, any>;

// Identity substitution
export type EmptySubst = {}

type ApplySubst<S extends Substitution, T> =
    T extends TypeVar<infer Name>
        ? Name extends keyof S
            ? S[Name]
            : T
        : T extends TypeApp<infer Fun, infer Arg>
            ? TypeApp<ApplySubst<S, Fun>, ApplySubst<S, Arg>>
            : T extends TypeCon<any>
                ? T
                : T

type ComposeSubst<S1 extends Substitution, S2 extends Substitution> = {
    [K in keyof S1 | keyof S2]: K extends keyof S1
        ? ApplySubst<S2, S1[K]>
        : K extends keyof S2
            ? S2[K]
            : never;
}

type FreeTypeVars<T> =
    T extends TypeVar<infer Name>
        ? [Name]
        : T extends TypeApp<infer Fun, infer Arg>
            ? [...FreeTypeVars<Fun>, ...FreeTypeVars<Arg>]
            : T extends TypeCon<any>
                ? []
                : []

// Generalization (∀-introduction)
type Generalize<Env extends TypeEnv, T> = TypeScheme<
    Exclude<FreeTypeVars<T>[number], FreeTypeVars<Env>[number]>[],
    T
>;

type Instantiate<T> =
    T extends TypeScheme<infer Vars, infer Type>
        // create fresh type variables for each bound variable
        ? ApplySubst<CreateFreshSubst<Vars>, Type>
        : T

type CreateFreshSubst<Vars extends readonly string[]> =
    Vars extends readonly [infer Head, ...infer Tail]
        ? Head extends string
            ? Tail extends readonly string[]
                ? { [K in Head]: TypeVar<`fresh_${K}`> } & CreateFreshSubst<Tail>
                : { [K in Head]: TypeVar<`fresh_${K}`> }
            : {}
        : {}

export type Unify<T1, T2> = UnifyHelper<T1, T2, EmptySubst>

type UnifyHelper<T1, T2, S extends Substitution> =
  T1 extends TypeVar<infer Name1>
    ? T2 extends TypeVar<infer Name2>
      ? Name1 extends Name2
        ? S  // Same variable
        : ComposeSubst<S, { [K in Name1]: T2 }>  // Bind T1 to T2
      : Name1 extends FreeTypeVars<T2>[number]
        ? never  // Occurs check failed
        : ComposeSubst<S, { [K in Name1]: T2 }>
    : T2 extends TypeVar<infer Name2>
      ? Name2 extends FreeTypeVars<T1>[number]
        ? never  // Occurs check failed
        : ComposeSubst<S, { [K in Name2]: T1 }>
    : T1 extends TypeCon<infer Name1>
      ? T2 extends TypeCon<infer Name2>
        ? Name1 extends Name2
          ? S  // Same constructor
          : never  // Different constructors
        : never
      : T1 extends TypeApp<infer Fun1, infer Arg1>
        ? T2 extends TypeApp<infer Fun2, infer Arg2>
          ? UnifyHelper<
              Arg1, 
              Arg2, 
              UnifyHelper<Fun1, Fun2, S>
            >
          : never
        : never

// algoruthm W
export type InferType<Expr, Env extends TypeEnv = {}> = 
  Expr extends { kind: 'Var', name: infer Name }
    ? Name extends keyof Env
      ? [Instantiate<Env[Name]>, EmptySubst]
      : never  // Unbound variable
    : Expr extends { kind: 'Abs', param: infer Param, body: infer Body }
      ? Param extends string
        ? InferAbs<Param, Body, Env>
        : never
    : Expr extends { kind: 'App', fun: infer Fun, arg: infer Arg }
      ? InferApp<Fun, Arg, Env>
    : Expr extends { kind: 'Let', name: infer Name, value: infer Value, body: infer Body }
      ? Name extends string
        ? InferLet<Name, Value, Body, Env>
        : never
    : never

type InferAbs<Param extends string, Body, Env extends TypeEnv> = 
InferType<Body, Env & { [K in Param]: TypeVar<`α${Param}`> }> extends [infer BodyType, infer Subst]
    ? [FunType<TypeVar<`α${Param}`>, BodyType>, Subst]
    : never

// Helper for application inference
type InferApp<Fun, Arg, Env extends TypeEnv> = 
  InferType<Fun, Env> extends [infer FunType, infer S1 extends Substitution]
    ? InferType<Arg, ApplySubstToEnv<S1, Env>> extends [infer ArgType, infer S2 extends Substitution]
      ? Unify<ApplySubst<S2, FunType>, TypeApp<TypeApp<TypeCon<'->'>, ArgType>, TypeVar<'β'>>> extends infer S3 extends Substitution
        ? [ApplySubst<S3, TypeVar<'β'>>, ComposeSubst<ComposeSubst<S1, S2>, S3>]
        : never
      : never
    : never

// Helper for let expression inference
type InferLet<Name extends string, Value, Body, Env extends TypeEnv> = 
    InferType<Value, Env> extends [infer ValueType, infer S1 extends Substitution]
        ? InferType<Body, ApplySubstToEnv<S1, Env> & { [K in Name]: Generalize<Env, ValueType> }> extends [infer BodyType, infer S2 extends Substitution]
            ? [BodyType, ComposeSubst<S1, S2>]
            : never
        : never

// Apply substitution to environment
type ApplySubstToEnv<S extends Substitution, Env extends TypeEnv> = {
    [K in keyof Env]: ApplySubst<S, Env[K]>
}

export type Expr = 
  | { kind: 'Var', name: string }
  | { kind: 'Abs', param: string, body: Expr }
  | { kind: 'App', fun: Expr, arg: Expr }
  | { kind: 'Let', name: string, value: Expr, body: Expr }
  | { kind: 'Lit', value: number | boolean }
