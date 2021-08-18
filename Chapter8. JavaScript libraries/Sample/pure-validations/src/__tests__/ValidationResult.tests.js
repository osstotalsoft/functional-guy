import { ValidationResult } from '../algebra'
import { required, email } from '../primitiveValidators'
import { map, inc, identity, composeK } from 'ramda'

const { Success, Failure } = ValidationResult;

describe("ValidationResult tests:", () => {
    it("Functor map: ", () => {
        expect(Success(7) |> map(inc)).toStrictEqual(Success(8))
        expect(Failure(["some", "msgs"]) |> map(identity)).toStrictEqual(Failure(["some", "msgs"]))
    })

    it("Monad bind: ", () => {
        expect("wrongEmail" |> composeK(required, email)).toStrictEqual(Failure("not an email"))
    })
})