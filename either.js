// Uses Ramda & Ramda FantasyLand
// 1. Use chain to flatten the Right to a value. 
//    Refer the implementation: https://github.com/ramda/ramda-fantasy/blob/master/src/Either.js.
//    Notice the difference between chain implementation for Right and Left
// 2. When parseTodos return Left, then the execution stops midway. Again, check Left.map implementation.

const Right = Either.Right
const Left = Either.Left
const logError = (error) => { console.log('Error: ' + error.message); };
const writeToLocalStorage = (x) => {
  console.error("Writing to todos: " + x)
  window.localStorage.setItem("todos", x)
}
const addTodo = (val) => {
  const eitherLogOrWrite = Either.either(logError, writeToLocalStorage);
  const parseTodos = str => {
      console.log(str)
      try { let y = JSON.parse(str); return Either.Right(y) }
      catch(err) { console.log("error in parsing");Either.Left( new Error("error while trying to parse")) }   
  }
  let todos = Either.Right(Maybe(window.localStorage.getItem("todos")).getOrElse("[]"))
    .chain(x => parseTodos(x))
    .map(x => R.prepend(val, x))
    .map(x => compose(R.uniq, R.filter(y => y.length > 0))(x))
    .map(y => {console.log(y); return JSON.stringify(y)});
  
  eitherLogOrWrite(todos)
}
  
addTodo("say hello1");
