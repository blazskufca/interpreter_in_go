package vm

import (
	"github.com/blazskufca/interpreter_in_go/code"
	"github.com/blazskufca/interpreter_in_go/object"
)

/*
FUNCTIONS IN THE VM:

The Monkey VM constant pool contains object.CompiledFunction. When we come across a function OpCall we need to execute
instructions inside object.CompiledFunction sitting on top of the stack! And we need to continue executing these function
instructions until we either hit a OpReturnValue(Preserves the return value on top of the stack)/OpReturn!

Then we remove the just executed object.CompiledFunction from the stack and replace ti with return value.

But how do we "execute the function"?

Firstly a refresher, currently we iterate over the program instructions and their operands via an instructionPointer.
On branching we change the instruction pointer manually, and therefore what instruction we fetch from the program slice.

We don't want to change this, because it works!

What we want to change is the used data and the instruction pointer. After we are done we need to change them back
to what they were before we started executing the function...

Consider this:

													let one = fn() { 5 };
													let two = fn() { one() };
													let three = fn() { two() };
													three();

When we call three, the data and the instruction pointer need to change.
When we call two, the data and the instruction pointer need to change.
When we call one, the data and the instruction pointer need to change.

And then in reverse:
When function one returns we need to change it to whatever we left it at in function two.
Same for two.
Same for three, but this time we need to set back the values for the main program.

So the function calls are nested and the execution relevant data - the instructions and the instruction pointer are accessed
in LIFO (last in, first out) manner!

Frame is short for call frame, or stack frame, and is the name for a data structure that holds execution-relevant information.

In compiler or interpreter literature this is also sometimes called an activation record (https://www.sciencedirect.com/topics/computer-science/activation-record)

On real machines – computers – a frame is a not something separate from but a designated part of the stack.
It’s where the return address, the arguments to the current function and its local variables are stored.

In virtual-machine land we don’t have to use the stack.
We’re not constrained by standardized calling conventions and other much too real things, like real memory addresses and locations.
We can store frames and other relevant execution-relevant information where we'd like to (within reason of the working environment)

What we'll do is make a new stack for Frames instead of keeping them on our main stack.
*/

// Frame is a call/stack frame or rather an activation record (https://www.sciencedirect.com/topics/computer-science/activation-record)
type Frame struct {
	fn *object.CompiledFunction // fn points to the *object.CompiledFunction for which this stack frame is for
	ip int                      // ip is the instruction pointer in this frame, for this function.
}

// NewFrame returns a pointer to a new activation record which references the given function. The Frame.ip is initialized
// to -1.
func NewFrame(fn *object.CompiledFunction) *Frame {
	return &Frame{fn: fn, ip: -1}
}

// Instructions returns code.Instructions for a function referenced by this activation record.
func (f *Frame) Instructions() code.Instructions {
	return f.fn.Instructions
}
