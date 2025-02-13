package compiler

// https://en.wikipedia.org/wiki/Symbol_table
/*
Symbol tables are used to associate identifiers (also called a symbol) with information.
Common bound information are things like location, scope, declaration status, symbol type, or whatever else would be useful for you...

We'll associate our symbols with scope and a unique number as described in code/code.go

The idiomatic operations on a symbol are:
	- define: Defines an identifier in a given scope and associates information with it

	- resolve: You resolve the symbol to the information you previously defined it with
*/

// SymbolScope is used to track a given scope of symbols. The value of a SymbolScope itself is not important,
// the important thing is that it is unique, because we need to differentiate between scopes (like global, local, etc..)
type SymbolScope string

const (
	// GlobalScope represents symbols defined globally, as the name suggest.
	GlobalScope SymbolScope = "GLOBAL"
)

// Symbol holds all the necessary information we need for a single symbol. It's name, scope and index.
type Symbol struct {
	Name  string      // Name is the name of this symbol
	Scope SymbolScope // Scope is the scope on which this symbol was defined.
	Index int         // Index is the index of the symbol in the store
}

// SymbolTable associates strings with Symbol.
type SymbolTable struct {
	store          map[string]Symbol // store associates the identifier we come across in Monkey source cods with a Symbol
	numDefinitions int               // numDefinitions just tracks how many Symbols we know about/have bound with identifiers
}

// NewSymbolTable returns a pointer to a new initialized SymbolTable.
func NewSymbolTable() *SymbolTable {
	return &SymbolTable{
		store:          make(map[string]Symbol),
		numDefinitions: 0,
	}
}

// Define creates a new Symbol with the given name/identifier. SymbolTable numDefinitions is used as Symbol.Index.
// It then stores this created symbol in the SymbolTable under the given name/identifier, increments the numDefinitions
// and returns the created Symbol to the caller.
func (s *SymbolTable) Define(name string) Symbol {
	symbol := Symbol{Name: name, Index: s.numDefinitions, Scope: GlobalScope}
	s.store[name] = symbol
	s.numDefinitions++
	return symbol
}

// Resolve looks up the given name in the SymbolTable.
// It returns the Symbol and a boolean indicting if this symbol exists in the SymbolTable at all.
func (s *SymbolTable) Resolve(name string) (Symbol, bool) {
	sym, ok := s.store[name]
	return sym, ok
}
