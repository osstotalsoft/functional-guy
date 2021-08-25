import { pipeK, reduce, map, applyTo, liftN, always, identity, apply, curry, concat, toPairs, range, length, keys } from 'ramda'
import { ValidationResult } from './algebra'
import { $do } from '@totalsoft/zion'

const { Success, Failure } = ValidationResult
const composeK = pipeK

export const stopOnFirstFailure = reduce(composeK, Success)

const concatFailure2 = (v1, v2) => x =>
    [v1, v2]
    |> map(applyTo(x))
    |> apply(liftN(2, always(identity)))

// const concatFailure2 = (v1, v2) => x =>{
//     const vr1 = v1(x)
//     const vr2 = v2(x)
//     const takeLast = curry((_,y) => y)
//     const liftA2 = liftN(2)
//     const takeLastVr = liftA2(takeLast)
//     return takeLastVr(vr1, vr2)
// }

export const concatFailure = reduce(concatFailure2, Success)

export const error = curry((errorsMapFn, v) => x =>
    v(x).cata({
        Success: Success,
        Failure: errors => Failure(errors |> errorsMapFn)
    })
)

export const field = curry((key, fieldValidator) => x => $do(function* () {
    const fieldValue = x[key]
    const v = fieldValidator |> error(map(concat(`${key}: `)))
    yield v(fieldValue)
    return x
}))

export const shape = shapeValidator =>
    shapeValidator
    |> toPairs // [[key, validator]]
    |> map(apply(field)) // [ValidationResult a]
    |> concatFailure //ValidationResult a

export const items = itemValidator => x =>
    x
    |> length
    |> range(0) // [0..length x]
    |> map(i => field(i, itemValidator)) // [Validator a]
    |> concatFailure
    |> applyTo(x)

export const allProps = propValidator => x =>
    x
    |> keys
    |> map(k => field(k, propValidator)) // [Validator a]
    |> concatFailure
    |> applyTo(x)