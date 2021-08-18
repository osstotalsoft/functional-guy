import { composeK, flip, reduce } from 'ramda'
import { ValidationResult } from './algebra'

const { Success } = ValidationResult
const myComposeK = flip(composeK)

export const stopOnFirstFailure = reduce(myComposeK, Success)