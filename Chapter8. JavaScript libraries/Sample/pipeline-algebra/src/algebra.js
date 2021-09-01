import { curry, identity, reduce, flip } from 'ramda';

// function empty(_ctx, next) {
//     return next();
// }

export const run = curry((ctx, middleware) => middleware(ctx, Promise.resolve))

export const empty = () => identity

export const append = curry((left, right, ctx, next) => left(ctx, () => right(ctx, next)))

export const use = flip(append)

export const concat = reduce(append, empty)

export const parallel = (first, second) => async (ctx, next) => {
    await (first |> run(ctx))
    await (second |> run(ctx))
    await next
}



