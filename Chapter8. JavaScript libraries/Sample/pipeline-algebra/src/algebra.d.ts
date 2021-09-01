export type Middleware<TContext> = (
  ctx: TContext,
  next: () => Promise<void>
) => Promise<void>;

export function run<TContext>(
  ctx: TContext, 
  middleware: Middleware<TContext>): Promise<void>;

export let empty: Middleware<any>;

export function append<TContext>(
  left: Middleware<TContext>,
  right: Middleware<TContext>
): Middleware<TContext>;

export function use<TContext>(
  right: Middleware<TContext>,
  left: Middleware<TContext>
): Middleware<TContext>;

export function concat<TContext>(
  xs: Array<Middleware<TContext>>
): Middleware<TContext>;

export function parallel<TContext>(
  first: Middleware<TContext>,
  second: Middleware<TContext>
): Middleware<TContext>;
