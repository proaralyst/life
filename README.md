# life
Playing with simulations on a grid. The aim is to start off small & build
complexity.

## Todo
* Write the automaton interface. This should provide the functor that `interact`
  will eventually transform to. Will have to convert incoming keys into the
  `Interact` values, and provide state update side effects. Should probably also
  convert from some `view` DSL & package everything nicely up into a record.
  Inspiration from PureScript's Halogen.

* Fettle the Life component

* Add a cursor, a pause shortcut & the ability to flip a cell's state
