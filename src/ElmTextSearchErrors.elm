module ElmTextSearchErrors exposing
    ( AddError(..)
    , RemoveError(..)
    , SearchError(..)
    )

{-| Error types used in ElmTextSearch results.


## Types

@docs AddError
@docs RemoveError
@docs SearchError

-}


{-| Used in error Result case of ElmTextSearh.addT
-}
type AddError
    = AddErrorUniqueRefIsEmpty
    | NoTermsToIndexAfterTokenisation
    | DocAlreadyExists


{-| Used in error Result case of ElmTextSearh.removeT
-}
type RemoveError
    = RemoveErrorUniqueRefIsEmpty
    | DocIsNotInIndex


{-| Used in error Result case of ElmTextSearh.searchT
-}
type SearchError
    = IndexIsEmpty
    | QueryIsEmpty
    | NoTermsToSearchAfterTokenisation
