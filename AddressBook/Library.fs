namespace AddressBook


module Person =
    let private (>>=) a f = Result.bind f a

    // Look at making this type private so it can't be constructed
    // https://stackoverflow.com/questions/13925361/is-it-possible-to-enforce-that-a-record-respects-some-invariants/13925632#13925632
    //
    // Also want to explore having stronger types (e.g. an Age type that can't be negative)
    type Person = {
        FirstName: string
        LastName: string
        Age: int
    }
    
    type Contact =
        | PersonalContact of Person
        
    let printContact c =
        [
         sprintf "Contact Name: %s %s" c.FirstName c.LastName ;
         sprintf "Contact Age: %i" c.Age
        ] |> String.concat "\n"
        
    let isNotBlank (name:string) =
        match name with
        | "" -> Error "Can't have empty name"
        | _ -> Ok name
        
    let hasTitleCase (name:string) =
        // TODO: Handle case when name is null
        if (string name.[0]).ToUpperInvariant() <> (string name.[0])
        then Error (sprintf "Name [%s] does not have Title Case" name)
        else Ok name
    
    let meetsSomeArbitraryLengthCriteria (name:string) =
        match name.Length with
        | 1 | 2 -> Error (sprintf "Name [%s] is too short" name)
        | x when x = 6 -> Error (sprintf "We dont accept people with 6 letter names [%s]" name) // This is an example of a Match Expression
        | _ -> Ok name
        
    // Future Xerx is going to look at this and wonder wat?
    let validateFirstName name =
        name
        |> isNotBlank       // This line uses pipelineing to pass Name into isNotBlank
        >>= hasTitleCase    // This line uses Result.bind in the definition of a private bind operator (See top of file)
    
    let validateLastName name =
        name
        |> isNotBlank
        >>= hasTitleCase
        >>= meetsSomeArbitraryLengthCriteria
        
    let validateAge age =
        match age with
        | x when x <= 0 -> Error (sprintf "Age must be greater than 0")
        | _ -> Ok age
    
    let validateInput firstName lastName age =
        let firstName = validateFirstName firstName
        let lastName = validateLastName lastName
        let age = validateAge
        
        // Something smarter can be done here than this...
        // Need to learn about Kleisli (fish) operator?
        // ROP also should have a better answer
        // https://fsharpforfunandprofit.com/posts/recipe-part2/
        //
        // DT also suggested using Computation Expressions
        // https://fsharpforfunandprofit.com/posts/computation-expressions-wrapper-types/#another-example
        // e.g.
        // let! n = validateName name
        // let! a = validateAge age
        // Person n a
        let errors = [firstName; lastName] //;age THIS WON'T WORK FOR AGE...
                     |> List.map (function
                         | Ok _ -> ""
                         | Error e -> e
                         )
                     |> List.filter (fun x -> x <> "")
        if errors.Length = 0
        then Ok ()
        else Error (String.concat "\n" errors)
    
    // Validation here (and in helper methods above) is an example of Error Handling
    // when doing Railway Oriented Programming
    // https://medium.com/@kai.ito/test-post-3df1cf093edd
    let create firstName lastName age =
        
        // The use of map here will map over the results and convert the output into the Person record
        // This should change - maybe be collapsed into the function above once
        // we add more fields for validation.
        validateInput firstName lastName age
        |> Result.map (fun _ -> PersonalContact {
                                    FirstName = firstName
                                    LastName = lastName
                                    Age = age
                                })

    // DT: using bind
    let create' firstName lastName age =
        validateFirstName firstName |> Result.bind (fun fname ->
            validateLastName lastName |> Result.bind (fun lname ->
                validateAge age |> Result.bind (fun a ->
                    Ok (PersonalContact {
                            FirstName = fname 
                            LastName  = lname
                            Age = age
                    })
                )
            )
        )

    // DT: using applicative and hieroglyphics
    let private (<*>) (f : Result<'a -> 'b, 'e>) (a: Result<'a, 'e>) : Result<'b, 'e> =
        Result.bind (fun f' ->
            Result.map f' a
        ) f

    let private (<!>) = Result.map // <$> in Haskell

    let create'' firstName lastName age =
        let mkPerson f l a = PersonalContact { FirstName = f; LastName = l; Age = a }
        mkPerson <!> validateFirstName firstName <*> validateLastName lastName <*> validateAge age

    // DT: using computation expression
    type ResultBuilder() = // based on https://fsharpforfunandprofit.com/posts/computation-expressions-wrapper-types/#another-example
        member this.Bind(m, f) = 
            match m with
            | Error e -> Error e
            | Ok a -> f a
        member this.Return(x) = Ok x

    let result = ResultBuilder()

    let create''' firstName lastName age  : Result<Contact, string> =
        result {
            let! fname = validateFirstName firstName
            let! lname = validateLastName lastName
            let! a = validateAge age
            return PersonalContact { FirstName = fname; LastName = lname; Age = age }
        }

    // DT: using Validation
    type Validation<'a, 'e> =
        | Valid of 'a
        | Invalid of 'e list

    // see https://hackage.haskell.org/package/validation
    module Validation =

        let map (f : 'a -> 'b) (a : Validation<'a, 'e>) =
            match a with
            | Valid a' -> Valid (f a')
            | Invalid e -> Invalid e

        let apply (f : Validation<'a -> 'b, 'e>) (a : Validation<'a, 'e>) : Validation<'b, 'e> =
            match f with
            | Valid f' -> map f' a
            | Invalid e ->
                match a with
                | Valid _ -> Invalid e
                | Invalid e' -> Invalid (e @ e')    // @ = List.append

    // This will work nicely with computation expressions as of F# 5
    // https://devblogs.microsoft.com/dotnet/announcing-f-5-preview-1/ (search for "Applicative")
    // I need to look into this more, but for now will just use hieroglyphics:

    let create'''' firstName lastName age =
        let (<*>) f a = Validation.apply
        let (<!>) = Validation.map
        let toValidation (r: 'a -> Result<'a, 'e>) : ('a -> Validation<'a, 'e>) = fun a ->
            match r a with
            | Ok x -> Valid x
            | Error e -> Invalid [e]

        let mkPerson f l a = PersonalContact { FirstName = f; LastName = l; Age = a }
        mkPerson
            <!> toValidation validateFirstName firstName
            <*> toValidation validateLastName lastName
            <*> toValidation validateAge age

module AddressBook =
    open Person
    
    type AddressBook = Contact list

    let addToAddressBook book person =
        person :: book
    

module SortAddressBook =
    open AddressBook
    
    type SortOrder =
        | Ascending
        | Descending
    
    let sort (addressBook:AddressBook) (order: SortOrder) =
        match order with
            | Ascending -> List.sort addressBook
            | Descending -> List.sortDescending addressBook
        
    

module Test =
    open Person
    let test1 =
        let peach = Person.create "Peach" "The Princess" 24
        let luigi = Person.create "Luigi" "The Brother" 25
        let mario = Person.create "Mario" "The Plumber" 26
        
        let addressBook = [peach; mario; luigi]
        
        let printFunc = (fun (PersonalContact contact) ->
            printfn "Contact Name: %s %s" contact.FirstName contact.LastName
            Ok contact
        )
        let raisedFunc = Result.bind printFunc
        
        addressBook
            |> List.iter (fun c -> raisedFunc c |> ignore)
        
