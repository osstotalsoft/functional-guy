import { taggedSum } from "daggy"
import * as fl from "fantasy-land"
import { map, compose } from "ramda"

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
            Success: x => fn |> map(f => f(x)),
            Failure: Failure
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
            Failure: Failure
        })
    }
}



//type Validator a = a -> ValidationResult a





