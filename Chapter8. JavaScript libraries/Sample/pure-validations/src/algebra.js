import { taggedSum } from "daggy"
import * as fl from "fantasy-land"
import { map, compose, concat } from "ramda"

//data ValidationResult a  =  Success a | Failure [String]
export const ValidationResult = taggedSum("ValidationResult", {
    Success: ["value"],
    Failure: ["errors"]
});
const { Success, Failure } = ValidationResult;

/* Functor ValidationResult */ {
    ValidationResult.prototype[fl.map] = function (f) {
        return this.cata({
            Success: compose(Success, f),
            Failure: Failure
        })
    }

}

/* Apply ValidationResult */ {
    ValidationResult.prototype[fl.ap] = function (fn) {
        return this.cata({
            Success: a => fn |> map(f => f(a)),
            Failure: errors => fn.cata({
                Success: _ => Failure(errors),
                Failure: errors2 => Failure(concat(errors2, errors))
            })
        })
    }
}

/* Applicative ValidationResult */ {
    ValidationResult[fl.of] = Success;
}

/* Chain ValidationResult */ {
    ValidationResult.prototype[fl.chain] = function (f) {
        return this.cata({
            Success: f,
            Failure: errors => Failure(errors)
        })
    }
}



//type Validator a = a -> ValidationResult a





