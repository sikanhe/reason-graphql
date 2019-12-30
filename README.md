[![CircleCI](https://circleci.com/gh/sikanhe/reason-graphql/tree/master.svg?style=svg)](https://circleci.com/gh/sikanhe/reason-graphql/tree/master)

Type safe GraphQL server in pure reason. Compiles to nodejs. A direct port from https://github.com/andreas/ocaml-graphql-server to make it work with Javascript backend.



### Motivation

Bucklescript is an amazing alternative to other compie-to-js languages. But it's a tragedy that we have such few libraries and bindings written for the server-side. 

In the browser, we can get away with just one great binding for React. However, for the server side, people want major building blocks - a solid database abstractions, a fast http server and a Graphql layer are usually must haves for non-trivial projects. 

For these type of frameworks, I believe we should do more than just writing simple bindings to existing npm libraries, because then we are not taking advantage of the expressive type system and rich features of OCaml/Reason. We also lose the chance to show what a language like Reason can do for developer happiness and why its worth it to use it over existing alternatives. 

### What we get over the traditional JS/TS/Flow + graphql-js combination

#### Type safety without manual work
Like the ReasonReact implementation, GraphQL schemas defined by this library can express more things and place more constraint on the graphql schema than the vanilla javascript or typescript counterparts. 

With Typescript (or Flow), you are required to manually write out the types for all your fields, or use a type generation cli tool like https://graphql-code-generator.com - on top of a lot of manual typecasting, because its typesystem cannot express advanced concepts like heterogenous lists (essentially what arguments and fields are, in GraphQL). 

To give you a taste of what this means - if we define a field argument called `id` with type `string` with graphql-js, even with typescript or flow, it cannot infer the type of `args` inside the resolver to be `{id: string}`. We have to manually type it, which makes it very error-prone. Forget about tracking the nullability of the fields - I have seen many production errors where the manually casted types are out of sync with the schema definition, and vice versa, when the schema gets out of sync with the underlying database models.

```typescript
  type PersonByIdArgs = {
    id: string
  }
  
  personById: {
    type: Person,
    args: { id: GraphqlNonNull(GraphqQLString) },
    resolve: (ctx, parent, args: PersonByIdArgs) => {
                           ^^^^ this is inferred as `Any`, so we need to manually cast it to `PersonByIdArgs`
    }
  }
```

In Reason, we can use a more advanced feature of the type system called GADT (Generalized Algebriac Data Types) to express our schema types. 

What GADT allows us to do is to have type-safe definitions without needing to manually write types for field arguments and resolver. It can "unfold" the types for resolver as you write out field args! (https://drup.github.io/2016/08/02/difflists/)

```reason 
 field("PersonById", 
  ~typ=person, 
  ~args=Args.[arg("id", nonnull(string))] 
  ~resolve=(_ctx, _parent, id) => {
//                         ^^ Unfolds args into resolver arguments and correctly types it as a string!
  }
```

This works for as many arguments as you like, it infers nullability for you as well and give you an `option('a)` if its nullable! 

```reason 
 field("PersonById", 
  ~typ=person, 
  ~args=Args.[arg("id", nonnull(string)), arg("age", int)] 
  ~resolve=(_ctx, _parent, id,     age) => {
                           ^^      ^^^ 
                           string  option(int) because age is not non-null
  }
```
 
### Features todolist:
  - [x] Query 
  - [x] Mutation 
  - [x] Async Fields
  - [x] Directives
  - [ ] Subscription
  - [ ] Non-type Validations (unused fragment, unused variables, and etc)
