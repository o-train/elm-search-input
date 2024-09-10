# Elm Search Input

Simple search input for use in Elm with debouncer capabilities.

## Description

This is not intended for external use yet and the docs will be filled out in more detail when ready.
Let people know what your project can do specifically. Provide context and add a link to any reference visitors might be unfamiliar with. A list of Features or a Background subsection can also be added here. If there are alternatives to your project, this is a good place to list differentiating factors.

## Badges
On some READMEs, you may see small images that convey metadata, such as whether or not all the tests are passing for the project. You can use Shields to add some to your README. Many services also have instructions for adding a badge.

## Visuals
Depending on what you are making, it can be a good idea to include screenshots or even a video (you'll frequently see GIFs rather than actual videos). Tools like ttygif can help, but check out Asciinema for a more sophisticated method.

## Installation
To install run:

```bash
elm install o-train/elm-search-input
```

## Usage

```elm
import Select

type alias YourItem =
    { id : Int 
    , name : String }

type alias YourModel =
    { items = List YourItem
    , selectInput = Select.Model YourUser
    }

initModel =
    { items = [] 
    , selectInput = Select.basicInit etc.
    }

type Msg =
    ...
    | SelectInput (Select.Msg YourItem)

update : ...
    SelectInput subMsg ->
        SelectInput subMsg ->
            let
                ( selectInputModel, selectMsg ) =
                    Select.update subMsg model.selectInput
            in
            ( { model | selectInput = selectInputModel }, selectMsg |> Cmd.map SelectInput )
        
```

## Roadmap
- [ ] Refactor from initial quick one file write
- [ ] Refactor inits using Builder pattern
- [ ] Improve docs
- [ ] Make it Beta (and better...)
- [ ] Better example

## Contributing
To contribute please open an MR/PR with a title and description of what it does. Point it to `main` branch

## Authors and acknowledgment
Olly Putland (Maintainer)

## License
MIT License

## Project status
Alpha
