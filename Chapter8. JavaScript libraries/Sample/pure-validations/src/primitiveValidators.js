import { ValidationResult } from './algebra'

const { Success, Failure } = ValidationResult;

//primitive validators
export const required = x =>
    x !== null && x !== undefined && (typeof x === "string" ? x !== "" : true)
        ? Success(x)
        : Failure(["required"])

export const email = x => {
    const regex = /^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/
    return regex.test(String(x).toLowerCase())
        ? Success(x)
        : Failure(["not an email"])
}
