export type TypeVar<Name extends string> = {
  kind: 'TypeVar';
  name: Name;
};

export type TypeCon<Name extends string> = {
  kind: 'TypeCon';
  name: Name;
};

export type TypeApp<Fun, Arg> = {
  kind: 'TypeApp';
  fun: Fun;
  arg: Arg;
};

export type TypeAST<Name extends string = string> =
  | TypeVar<Name>
  | TypeCon<Name>
  | TypeApp<any, any>;

export type FunType<From, To> =
  TypeApp<TypeApp<TypeCon<'->'>, From>, To>;

export type IntType = TypeCon<'Int'>;
export type BoolType = TypeCon<'Bool'>;
export type ListType<T> = TypeApp<TypeCon<'List'>, T>;

// Polymorphic Type Scheme (∀α.τ)
export type TypeScheme<
  Vars extends readonly string[],
  T
> = {
  kind: 'TypeScheme';
  vars: Vars;
  type: T;
};

// Environment and Substitutions
export type TypeEnv =
  Record<string, TypeScheme<string[], any>>;

export type Substitution<Name extends string = string> =
  Record<Name, any>;

export const EmptySubst: Substitution<string> = {} as Substitution<string>;

// Apply and Compose Substitutions
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

// Free Type Variables
export type FreeTypeVars<T> =
  T extends TypeVar<infer V>
    ? V
    : T extends TypeApp<infer F, infer A>
      ? FreeTypeVars<F> | FreeTypeVars<A>
      : never;

// Generalization and Instantiation
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

export type CreateFreshSubst<
  Vars extends readonly unknown[]
> = Vars extends [infer H, ...infer R]
  ? H extends string
    ? { [K in H]: TypeVar<`fresh_${H}`> } & CreateFreshSubst<R>
    : CreateFreshSubst<R>
  : Record<string, never>;

export type Instantiate<S> = 
  S extends TypeScheme<infer Vars, infer T>
    ? ApplySubst<CreateFreshSubst<Vars>, T>
    : never;

// Unification
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

// Expressions and Type Inference
export type Expr =
  | { kind: 'Var'; name: string }
  | { kind: 'Abs'; param: string; body: Expr }
  | { kind: 'App'; fun: Expr; arg: Expr }
  | { kind: 'Let'; name: string; value: Expr; body: Expr }
  | { kind: 'Lit'; value: number }
  | { kind: 'Lit'; value: boolean };

export type InferType<
  E extends Expr,
  Env extends TypeEnv = Record<string, never>
> =
  E extends { kind: 'Var'; name: infer N }
    ? N extends keyof Env
      ? [Instantiate<Env[N]>, typeof EmptySubst]
      : never
  : E extends { kind: 'Abs'; param: infer P; body: infer B }
    ? P extends string
      ? B extends Expr
        ? InferAbs<P, B, Env>
        : never
      : never
  : E extends { kind: 'App'; fun: infer F; arg: infer A }
    ? F extends Expr
      ? A extends Expr
        ? InferApp<F, A, Env>
        : never
      : never
  : E extends { kind: 'Let'; name: infer N; value: infer V; body: infer B }
    ? N extends string
      ? V extends Expr
        ? B extends Expr
          ? InferLet<N, V, B, Env>
          : never
        : never
      : never
  : E extends { kind: 'Lit'; value: infer V }
    ? V extends number
      ? [IntType, typeof EmptySubst]
      : V extends boolean
        ? [BoolType, typeof EmptySubst]
        : never
  : never;

// 10. Helpers for Inference
export type ApplySubstToEnv<
  S extends Substitution<Name>,
  Env extends TypeEnv,
  Name extends string = string
> = {
  [K in keyof Env]: ApplySubst<S, Env[K]['type']> extends infer T
    ? TypeScheme<Env[K]['vars'], T>  
    : never;
};

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
