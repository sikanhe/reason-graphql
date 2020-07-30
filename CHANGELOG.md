## DEV
### *reason-graphql*
* (Breaking) Removed "Variations". Instead, we provide guidance on how to integrate this with different IO/Async libraries like `reason-promise` or `reason-future`. This prevents version conflicts due to users wanting to upgrade those libraries independently from `reason-graphql`. 
* (Breaking) Changed runtime representation of `List` graphql type to be Array to align better with reality of JS environment and upcoming Bucklescript changes (to prioritize array over list). (https://github.com/sikanhe/reason-graphql/pull/41)

## 0.6.1 
### *reason-graphql*
* Fixed introspection not working due to subscription type missing ([#33](https://github.com/sikanhe/reason-graphql/pull/33))

## 0.6.0
### *reason-graphql*
* Upgraded bs-platform to v7.0.1 ([#29](https://github.com/sikanhe/reason-graphql/pull/29))
* Fixed erroring on undefined variables by coercing missing variables to `null` ([#29](https://github.com/sikanhe/reason-graphql/pull/29))

## 0.4.2
### *reason-graphql*
* Added __typename support ([#24](https://github.com/sikanhe/reason-graphql/pull/24))

## 0.4.0 

### *reason-graphql*
* Added `GraphqlPromise` which is a pre-configured variation  that uses `Js.Promise` as the IO type. 
