import { run } from '../algebra'

describe("algebra tests:", () => {
    it("run: ", async () => {
        const ctx = {}
        const middleware = jest.fn()
        await middleware |> run(ctx)
        expect(middleware).toHaveBeenCalled();
    })
})