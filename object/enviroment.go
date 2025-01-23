package object

// Environment is a classic name in interpreters, especially "lisp-y" ones.
// In reality you can think of it as a wrapper around a map[string]object.Object.
// It associates strings with evaluated objects.
type Environment struct {
	store map[string]Object // store is a map[string]object.Object of bound object to a given name
}

// NewEnvironment initializes a new Environment structure and returns a pointer to it.
// It, of course, also takes care of Environment.store map initialization
func NewEnvironment() *Environment {
	s := make(map[string]Object)
	return &Environment{store: s}
}

// Get gets a a given name from Environment store map.
// It returns Object and a bool, which represent weather a Object exists under the given name in the Environment or if
// default value was returned (there is no set value under that name)
func (e *Environment) Get(name string) (Object, bool) {
	obj, ok := e.store[name]
	return obj, ok
}

// Set sets a val Object to a given name in the Environment store map.
// It returns back out the val Object.
func (e *Environment) Set(name string, val Object) Object {
	e.store[name] = val
	return val
}
