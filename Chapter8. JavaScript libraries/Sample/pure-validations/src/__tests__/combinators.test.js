import { required, email } from '../primitiveValidators'
import { stopOnFirstFailure, concatFailure, field, shape, items, error } from '../combinators'
import { ValidationResult } from '../algebra'
import { $do } from '@totalsoft/zion'
import { map, concat } from 'ramda'

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

    it("error: ", () => {
        const validator = required |> error(map(concat("*")));
        expect(validator(null)).toStrictEqual(Failure(["*required"]))
        expect(validator("something")).toStrictEqual(Success("something"))
    })

    it("field: ", () => {
        const validator = required |> field("name")
        expect(validator({ name: null })).toStrictEqual(Failure(["name: required"]))
        expect(validator({ name: "radu" })).toStrictEqual(Success({ name: "radu" }))
    })

    it("shape: ", () => {
        const validator = shape({ name: required, email: email })
        expect(validator({ name: null, email: "notAnEmail" })).toStrictEqual(Failure(["name: required", "email: not an email"]))
        //expect(validator({name:"radu"})).toStrictEqual(Success({name:"radu"}))
    })

    it("items: ", () => {
        const validator = items(required)
        expect(validator([null, null])).toStrictEqual(Failure(["0: required", "1: required"]))
        //expect(validator({name:"radu"})).toStrictEqual(Success({name:"radu"}))
    })

    it("composed validator: ", () => {
        const validator = shape({
            name: required |> error(map(concat("*"))),
            email: [required, email] |> stopOnFirstFailure,
            addresses: required |> items
        })
        expect(validator({
            name:"",
            email:"wrongEmail",
            addresses:["some addr",""]
        })).toStrictEqual(Failure(["name: *required", "email: not an email", "addresses: 1: required"]))
        //expect(validator({name:"radu"})).toStrictEqual(Success({name:"radu"}))
    })

    // it("applicative lift: ", () => {
    //     const validator = concatFailure([required, email])
    //     expect(validator(null)).toStrictEqual(Failure(["required", "not an email"]))
    //     expect(validator("notAnEmail")).toStrictEqual(Failure(["not an email"]))
    // })
})