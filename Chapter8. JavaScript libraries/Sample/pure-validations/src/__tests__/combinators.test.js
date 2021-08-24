import {required, email} from '../primitiveValidators'
import {stopOnFirstFailure} from '../combinators'
import {ValidationResult} from '../algebra'

const {/*Success,*/ Failure} = ValidationResult

describe("combinators tests:", () => {
    it("stopOnFirstFailure: ", () => {
        const validator = stopOnFirstFailure([required, email])
        expect(validator(null)).toStrictEqual(Failure(["required"]))
        expect(validator("notAnEmail")).toStrictEqual(Failure(["not an email"]))
    })
})