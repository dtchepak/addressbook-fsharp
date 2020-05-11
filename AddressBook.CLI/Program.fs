// Learn more about F# at http://fsharp.org

open System
open AddressBook
open Person
open SortAddressBook

module Menu =
    type MenuSelection =
        | CreateNew
        | ListAll
        | SortAddressBook
        | ExitApp
    type MenuChoice = MenuSelection option
       
    let private validateChoice (key:ConsoleKeyInfo) =
        match (string key.KeyChar).ToUpperInvariant() with
        | "C" -> Some CreateNew
        | "L" -> Some ListAll
        | "S" -> Some SortAddressBook
        | "X" -> Some ExitApp
        | _ -> None
        
    let private printMenu () =
        printf "Menu:
1. (C)reate a new person in the address book
2. (L)ist all people in the address book
3. (S)ort the address book
4. E(x)it the program

Enter your selection: "
    
    let getMenuOption () =
        printMenu ()
        let key = Console.ReadKey ()
        printfn "\n"
        validateChoice key

module NewAddressBookApp =
    open AddressBook
    type AppResult = AddressBook option

    type AppCommand = AddressBook -> AppResult

    let sortEntries : AppCommand = fun addressBook ->
        let validateChoice (choice:ConsoleKeyInfo) =
            match (string choice.KeyChar).ToUpperInvariant () with
                | "A" -> Some Ascending
                | "D" -> Some Descending
                | _ -> None
        
        printf "What order? (A)scending or (D)escending? "
        Console.ReadKey ()
        |> validateChoice
        |> Option.map (sort addressBook)
        |> Option.orElse (Some addressBook)

    let listEntries : AppCommand = fun addressBook ->
        let allEntries addressBook =
            addressBook
            |> List.map (function
                | PersonalContact c -> printContact c)
            |> String.concat "\n"
        
        match List.length addressBook with
        | 0 -> printfn "There are no entries in the address book\n"
        | _ -> printfn "All entries in the address book:\n%s\n" <| allEntries addressBook
        Some addressBook

    let createEntry : AppCommand = fun addressBook ->
        printf "Enter First Name: " 
        let firstNameString = Console.ReadLine ()
        printf "Enter Last Name: "
        let lastNameString = Console.ReadLine ()

        match create firstNameString lastNameString with
        | Ok c -> addToAddressBook addressBook c |> Some
        | Error m ->
            printfn "%s" m
            Some addressBook

    let private constant a _ = a

    let rec runMenu() : AppCommand = 
        match Menu.getMenuOption() with
        | None -> runMenu()
        | Some Menu.ListAll -> listEntries
        | Some Menu.SortAddressBook -> sortEntries
        | Some Menu.CreateNew -> createEntry
        | Some Menu.ExitApp -> constant None

let rec iterate (step: 'a -> 'a option) (a: 'a) =
    step a |> Option.bind (iterate step)

[<EntryPoint>]
let main _ =
    iterate (fun entries -> NewAddressBookApp.runMenu() entries) [] |> ignore
    0 // return an integer exit code
