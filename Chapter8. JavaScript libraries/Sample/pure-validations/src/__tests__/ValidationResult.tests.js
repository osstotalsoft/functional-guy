import { ValidationResult } from '../algebra'
import { required, email } from '../primitiveValidators'
import { map, inc, toUpper, chain } from 'ramda'

const { Success, Failure } = ValidationResult;

describe("ValidationResult tests:", () => {
    it("Functor map: ", () => {
        expect(Success(7) |> map(inc)).toStrictEqual(Success(8))
        expect(Failure(["some", "msgs"]) |> map(toUpper)).toStrictEqual(Failure(["some", "msgs"]))
    })

    it("Monad bind(chain): ", () => {
        const composedValidator = x => x |> required |> chain(email)
        expect("" |> composedValidator).toStrictEqual(Failure(["required"]))
        expect("notAnEmail" |> composedValidator).toStrictEqual(Failure(["not an email"]))
    })
})