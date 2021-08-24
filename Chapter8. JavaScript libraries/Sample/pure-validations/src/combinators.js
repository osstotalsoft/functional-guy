import { composeK as _composeK, flip, reduce } from 'ramda'
import { ValidationResult } from './algebra'

const { Success } = ValidationResult
const composeK = flip(_composeK)

export const stopOnFirstFailure = reduce(composeK, Success)