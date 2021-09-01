import { curry, flip, reduce } from 'ramda'

export const run = curry((ctx, middleware) => middleware(ctx, () => Promise.resolve()))

export const empty = curry((_ctx, next) => next())

export const append = curry((left, right, ctx, next) => left(ctx, () => right(ctx, next)))

export const use = flip(append)

export const concat = reduce(append, empty)

export const parallel = curry((first, second, ctx, next) => Promise.all([first |> run(ctx), second |> run(ctx)]).then(_ => next()))