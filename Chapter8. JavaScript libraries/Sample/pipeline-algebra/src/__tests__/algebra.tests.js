import { run, empty, append, use, concat, parallel } from '../algebra'
import 'jest-extended'

function timeout(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

describe("algebra tests:", () => {
    it("run: ", async () => {
        const ctx = {}
        const middleware = jest.fn()
        await (middleware |> run(ctx))
        expect(middleware).toHaveBeenCalled()
        expect(middleware).toHaveBeenLastCalledWith(ctx, expect.any(Function))
    })

    it("empty: ", async () => {
        const ctx = {}
        await (empty |> run(ctx))
        expect(ctx).toStrictEqual({})
    })

    it("append: ", async () => {
        const ctx = { value: 5 }
        const left = async (ctx, next) => {
            ctx.value += 1
            await next()
        }

        const right = async (ctx, next) => {
            ctx.value *= 2
            await next()
        }

        await (append(left, right) |> run(ctx))
        expect(ctx.value).toBe(12)
    })

    it("use: ", async () => {
        const ctx = { value: 5 }
        const left = async (ctx, next) => {
            ctx.value += 1
            await next()
        }

        const right = async (ctx, next) => {
            ctx.value *= 2
            await next()
        }

        await (empty |> use(left) |> use(right) |> run(ctx))
        expect(ctx.value).toBe(12)
    })

    it("concat: ", async () => {
        const ctx = { value: 5 }
        const left = async (ctx, next) => {
            ctx.value += 1
            await next()
        }

        const right = async (ctx, next) => {
            ctx.value *= 2
            await next()
        }

        const pipeline = [empty, left, right, empty, empty] |> concat
        await (pipeline |> run(ctx))
        expect(ctx.value).toBe(12)
    })

    it("parallel: ", async () => {
        const ctx = { value: 5 }
        const first = async (ctx, next) => {
            await timeout(300)
            ctx.value += 1
            await next()
        }

        const second = async (ctx, next) => {
            await timeout(200)
            ctx.value *= 2
            await next()
        }
        const pipeline = parallel(first, second)
        await (pipeline |> run(ctx))
        expect(ctx.value).toBe(11)
    })
})