import { composeK as _composeK, flip, reduce, liftN, always, identity, apply, map, applyTo, curry, toPairs, length, range, concat } from 'ramda'
import { ValidationResult } from './algebra'
import { $do } from '@totalsoft/zion'

const { Success, Failure } = ValidationResult
const composeK = flip(_composeK)

export const stopOnFirstFailure = reduce(composeK, Success)

const concatFailure2 = (v1, v2) => x =>
    [v1, v2]
    |> map(applyTo(x))
    |> apply(liftN(2, always(identity)))

export const concatFailure = reduce(concatFailure2, Success)

export const error = curry((errorsMapFn, v) => x =>
    v(x).cata({
        Success: Success,
        Failure: errors => Failure(errors |> errorsMapFn)
    })
)

export const field = curry((key, fieldValidator) => x => $do(function* () {
    const fieldValue = x[key]
    const v = fieldValidator|> error(map(concat(`${key}: `)))
    yield v(fieldValue)
    return x
}))

export const shape = shapeValidator =>
    shapeValidator
    |> toPairs
    |> map(apply(field))
    |> concatFailure

export const items = itemValidator => x =>
    x
    |> length
    |> range(0)
    |> map(i => field(i, itemValidator))
    |> concatFailure
    |> applyTo(x)
