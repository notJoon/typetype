/**
 * Represents a type variable (e.g., α, β) in the type AST.
 * @template Name - The name of the type variable.
 * @remarks
 * Type variables are placeholders for unknown types during inference.
 */
export type TypeVar<Name extends string> = {
  kind: 'TypeVar';
  name: Name;
};

/**
 * Represents a type constructor (e.g., Int, Bool, List) in the type AST.
 * @template Name - The name of the type constructor.
 * @remarks
 * Type constructors define concrete or parameterized types in the language.
 */
export type TypeCon<Name extends string> = {
  kind: 'TypeCon';
  name: Name;
};

/**
 * Represents application of one type to another (type-level function application).
 * @typeparam Fun - The function/type being applied.
 * @typeparam Arg - The argument type.
 * @remarks
 * Used to construct function types and parameterized types like List<T>.
 */
export type TypeApp<Fun, Arg> = {
  kind: 'TypeApp';
  fun: Fun;
  arg: Arg;
};

/**
 * Union of all type AST node kinds: variables, constructors, and applications.
 * @template Name - Type variable and constructor names.
 */
export type TypeAST<Name extends string = string> =
  | TypeVar<Name>
  | TypeCon<Name>
  | TypeApp<any, any>;

/**
 * Helper for building function types (From -> To) as nested TypeApp nodes.
 * @typeparam From - The argument type.
 * @typeparam To - The return type.
 */
export type FunType<From, To> =
  TypeApp<TypeApp<TypeCon<'->'>, From>, To>;

export type IntType = TypeCon<'Int'>;
export type BoolType = TypeCon<'Bool'>;

/**
 * Constructs a list type applied to element type T (List<T>).
 * @typeparam T - Element type of the list.
 */
export type ListType<T> = TypeApp<TypeCon<'List'>, T>;

/**
 * Represents a polymorphic type scheme (∀α. τ).
 * @template Vars - Array of bound type variable names.
 * @template T - The underlying monomorphic type AST.
 * @remarks
 * Captures the idea of universal quantification over type variables.
 */
export type TypeScheme<
  Vars extends readonly string[],
  T
> = {
  kind: 'TypeScheme';
  vars: Vars;
  type: T;
};

/**
 * Type environment mapping variable names to their polymorphic TypeSchemes.
 * 
 * @todo Remove any
 */
export type TypeEnv =
  Record<string, TypeScheme<string[], any>>;

/**
 * Substitution mapping from type variable names to concrete types.
 * @template Name - Names of type variables being substituted.
 * @remarks
 * Used during unification to record type equalities.
 * 
 * @todo Remove any
 */
export type Substitution<Name extends string = string> =
  Record<Name, any>;

export const EmptySubst: Substitution<string> = {} as Substitution<string>;

/**
 * Applies a substitution to a type AST, replacing variables as needed.
 * @template S - The substitution to apply.
 * @template T - The type AST to transform.
 */
export type ApplySubst<
  S extends Substitution<Name>,
  T,
  Name extends string = string
> =
  T extends TypeVar<infer V>
    ? V extends keyof S
      ? S[V]
      : TypeVar<V>
  : T extends TypeApp<infer F, infer A>
    ? TypeApp<ApplySubst<S, F>, ApplySubst<S, A>>
  : T extends TypeCon<infer C>
    ? TypeCon<C>
  : never;

/**
 * Composes two substitutions S1 and S2, applying S2 to results of S1.
 * @template S1 - First substitution.
 * @template S2 - Second substitution.
 */
export type ComposeSubst<
  S1 extends Substitution<Name>,
  S2 extends Substitution<Name>,
  Name extends string = string
> = {
  [K in keyof S1 | keyof S2]:
    K extends keyof S1
      ? ApplySubst<S2, S1[K]>
      : K extends keyof S2
        ? S2[K]
        : never;
};

/**
 * Computes free type variables present in a type AST.
 * @template T - The type AST to inspect.
 * @returns Union of variable names.
 */
export type FreeTypeVars<T> =
  T extends TypeVar<infer V>
    ? V
    : T extends TypeApp<infer F, infer A>
      ? FreeTypeVars<F> | FreeTypeVars<A>
      : never;

/**
 * Generalizes a monomorphic type T over the environment Env,
 * producing a polymorphic TypeScheme that quantifies over free vars.
 * @template Env - The type environment.
 * @template T - The type to generalize.
 */
export type Generalize<
  Env extends TypeEnv,
  T
> = TypeScheme<
  Exclude<
    FreeTypeVars<T>,
    FreeTypeVars<Env[keyof Env]['type']>
  >[],
  T
>;

/**
 * Creates a fresh substitution mapping each variable in Vars to a new TypeVar
 * prefixed with "fresh_".
 */
export type CreateFreshSubst<
  Vars extends readonly unknown[]
> = Vars extends [infer H, ...infer R]
  ? H extends string
    ? { [K in H]: TypeVar<`fresh_${H}`> } & CreateFreshSubst<R>
    : CreateFreshSubst<R>
  : Record<string, never>;

/**
 * Instantiates a polymorphic TypeScheme by replacing bound vars with fresh ones.
 */
export type Instantiate<S> = 
  S extends TypeScheme<infer Vars, infer T>
    ? ApplySubst<CreateFreshSubst<Vars>, T>
    : never;

/**
 * Entry point for type unification, yielding a substitution or never on failure.
 */
export type Unify<
  T1,
  T2,
  Name extends string = string
> = UnifyHelper<T1, T2, typeof EmptySubst, Name>;

export type UnifyHelper<
  T1,
  T2,
  S extends Substitution<Name>,
  Name extends string = string
> =
  T1 extends TypeVar<infer V1>
    ? T2 extends TypeVar<infer V2>
      ? [V1] extends [V2]
        ? S
        : ComposeSubst<S, Record<V1, T2>>
    : V1 extends FreeTypeVars<T2>
      ? never
      : ComposeSubst<S, Record<V1, T2>>
  : T2 extends TypeVar<infer V2>
    ? V2 extends FreeTypeVars<T1>
      ? never
      : ComposeSubst<S, Record<V2, T1>>
  : T1 extends TypeCon<infer C1>
    ? T2 extends TypeCon<infer C2>
      ? [C1] extends [C2]
        ? S
        : never
      : never
  : T1 extends TypeApp<infer F1, infer A1>
    ? T2 extends TypeApp<infer F2, infer A2>
      ? UnifyHelper<A1, A2, UnifyHelper<F1, F2, S>>
      : never
  : never;

/**
 * AST for expressions in the mini-language.
 */
export type Expr =
  | { kind: 'Var'; name: string }
  | { kind: 'Abs'; param: string; body: Expr }
  | { kind: 'App'; fun: Expr; arg: Expr }
  | { kind: 'Let'; name: string; value: Expr; body: Expr }
  | { kind: 'Lit'; value: number }
  | { kind: 'Lit'; value: boolean };

/**
 * Maps expression kinds to their corresponding inference results.
 */
type InferTypeMap<E extends Expr, Env extends TypeEnv> = {
  'Var': E extends { kind: 'Var'; name: infer N extends string }
    ? N extends keyof Env
      ? [Instantiate<Env[N]>, typeof EmptySubst]
      : never
    : never;
  'Abs': E extends { kind: 'Abs'; param: infer P extends string; body: infer B extends Expr }
    ? InferAbs<P, B, Env>
    : never;
  'App': E extends { kind: 'App'; fun: infer F extends Expr; arg: infer A extends Expr }
    ? InferApp<F, A, Env>
    : never;
  'Let': E extends { kind: 'Let'; name: infer N extends string; value: infer V extends Expr; body: infer B extends Expr }
    ? InferLet<N, V, B, Env>
    : never;
  'Lit': E extends { kind: 'Lit'; value: infer V }
    ? V extends number
      ? [IntType, typeof EmptySubst]
      : V extends boolean
        ? [BoolType, typeof EmptySubst]
        : never
    : never;
};

/**
 * Implements Algorithm W for type inference, returning a tuple:
 * [inferred TypeAST, accumulated Substitution]
 */
export type InferType<
  E extends Expr,
  Env extends TypeEnv = Record<string, never>
> = E extends { kind: infer K extends keyof InferTypeMap<E, Env> }
  ? InferTypeMap<E, Env>[K]
  : never;

/**
 * Applies a substitution to every TypeScheme in the environment.
 */
export type ApplySubstToEnv<
  S extends Substitution<Name>,
  Env extends TypeEnv,
  Name extends string = string
> = {
  [K in keyof Env]: ApplySubst<S, Env[K]['type']> extends infer T
    ? TypeScheme<Env[K]['vars'], T>  
    : never;
};

/**
 * Infers the type of a lambda abstraction, extending the environment with
 * a fresh type variable for the parameter.
 */
export type InferAbs<
  P extends string,
  B extends Expr,
  Env extends TypeEnv
> = InferType<
  B,
  Env & { [K in P]: TypeScheme<[], TypeVar<`α${P}`>> }
> extends [infer BodyType, infer S extends Substitution]
    ? [FunType<TypeVar<`α${P}`>, BodyType>, S]
    : never;

/**
 * Infers the type of a function application by inferring both sides,
 * unifying the function type with an arrow type, and composing substitutions.
 */
export type InferApp<
  F extends Expr,
  A extends Expr,
  Env extends TypeEnv
> = InferType<F, Env> extends [infer FT, infer S1 extends Substitution]
    ? InferType<A, ApplySubstToEnv<S1, Env>> extends [infer AT, infer S2 extends Substitution]
      ? Unify<
          ApplySubst<S2, FT>,
          TypeApp<TypeApp<TypeCon<'->'>, AT>, TypeVar<'β'>>
        > extends infer S3 extends Substitution
        ? [ApplySubst<S3, TypeVar<'β'>>, ComposeSubst<ComposeSubst<S1, S2>, S3>]
        : never
      : never
    : never;

/**
 * Infers let-expressions by inferring the bound value,
 * generalizing its type, and inferring the body in extended env.
 */
export type InferLet<
  N extends string,
  V extends Expr,
  B extends Expr,
  Env extends TypeEnv
> = InferType<V, Env> extends [infer VT, infer S1 extends Substitution]
    ? InferType<
        B,
        ApplySubstToEnv<S1, Env> & { [K in N]: Generalize<Env, VT> }
      > extends [infer BT, infer S2 extends Substitution]
      ? [BT, ComposeSubst<S1, S2>]
      : never
    : never;
