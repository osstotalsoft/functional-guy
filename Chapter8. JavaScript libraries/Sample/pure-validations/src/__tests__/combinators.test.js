import { required, email } from '../primitiveValidators'
import { stopOnFirstFailure, concatFailure, field, shape, items } from '../combinators'
import { ValidationResult } from '../algebra'
import { $do } from '@totalsoft/zion'

const { Success, Failure } = ValidationResult

describe("combinators tests:", () => {
    it("stopOnFirstFailure: ", () => {
        const validator = stopOnFirstFailure([required, email])
        expect(validator(null)).toStrictEqual(Failure(["required"]))
        expect(validator("notAnEmail")).toStrictEqual(Failure(["not an email"]))
    })

    it("concatFailure: ", () => {
        const validator = concatFailure([required, email])
        expect(validator(null)).toStrictEqual(Failure(["required", "not an email"]))
        expect(validator("notAnEmail")).toStrictEqual(Failure(["not an email"]))
    })

    it("do notation: ", () => {
        const validator = x => $do(function* () {
            const x1 = yield required(x)
            return yield email(x1 + "@totalsoft.ro")
        })
        expect(validator(null)).toStrictEqual(Failure(["required"]))
        expect(validator("notAnEmail")).toStrictEqual(Success("notAnEmail@totalsoft.ro"))
    })

    it("field: ", () => {
        const validator = required |> field("name")
        expect(validator({ name: null })).toStrictEqual(Failure(["required"]))
        expect(validator({ name: "radu" })).toStrictEqual(Success({ name: "radu" }))
    })

    it("shape: ", () => {
        const validator = shape({ name: required, email: email })
        expect(validator({ name: null, email: "notAnEmail" })).toStrictEqual(Failure(["required", "not an email"]))
        //expect(validator({name:"radu"})).toStrictEqual(Success({name:"radu"}))
    })

    it("items: ", () => {
        const validator = items(required)
        expect(validator([null, null])).toStrictEqual(Failure(["required", "required"]))
        //expect(validator({name:"radu"})).toStrictEqual(Success({name:"radu"}))
    })

    // it("applicative lift: ", () => {
    //     const validator = concatFailure([required, email])
    //     expect(validator(null)).toStrictEqual(Failure(["required", "not an email"]))
    //     expect(validator("notAnEmail")).toStrictEqual(Failure(["not an email"]))
    // })
})