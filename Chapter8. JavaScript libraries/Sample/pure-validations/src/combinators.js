import { composeK as _composeK, flip, reduce, liftN, always, identity, apply, map, applyTo, curry, toPairs, length, range, subtract } from 'ramda'
import { ValidationResult } from './algebra'
import { $do } from '@totalsoft/zion'

const { Success } = ValidationResult
const composeK = flip(_composeK)

export const stopOnFirstFailure = reduce(composeK, Success)

const concatFailure2 = (v1, v2) => x =>
    [v1, v2]
    |> map(applyTo(x))
    |> apply(liftN(2, always(identity)))

export const concatFailure = reduce(concatFailure2, Success)

export const field = curry((key, fieldValidator) => x => $do(function* () {
    const fieldValue = x[key]
    yield fieldValidator(fieldValue)
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
