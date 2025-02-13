package vm

import (
	"errors"
	"fmt"
	"github.com/blazskufca/interpreter_in_go/code"
	"github.com/blazskufca/interpreter_in_go/compiler"
	"github.com/blazskufca/interpreter_in_go/object"
)

const StackSize = 2048

// GlobalsSize is 65536, because code.OpGlobalGet and code.OpGlobalSet have a single 2 byte operand...Meaning the maximum range
// is what the value GlobalSize is set to.
const GlobalsSize = 65536

// True is a global *object.Boolean with a value of true which the Monkey VM reuses anytime it needs a true boolean.
var True = &object.Boolean{Value: true}

// False is a global *object.Boolean with a value of false which the Monkey VM reuses anytime it needs a false boolean.
var False = &object.Boolean{Value: false}

// Null represents a value or lack there of a value. It's used when expressions in Monkey evaluate to "nothing"
var Null = &object.Null{}

// VM represent a Monkey Virtual Machine - The heart of the bytecode interpreter, which executes/evaluates bytecode
// produced by compiler.Compiler.
// Monkey virtual machine is a stack based virtual machine.
type VM struct {
	constants    []object.Object   // constants are the evaluated literals and other constants from the compiler.Compiler constants pool
	instructions code.Instructions // instructions are the bytecode instructions produced by compiler.Compiler
	stack        []object.Object   // stack is the Monkey virtual machine stack
	// sp is the Stack Pointer (https://www.techtarget.com/whatis/definition/stack-pointer).
	// It always points to the next value.
	// Top of the stack is stack[ sp -1 ].
	sp      int
	globals []object.Object // globals is the evaluated global store
}

// New returns a pointer to a new, initialized, Monkey virtual machine.
// Note that VM.stack is initialized to a slice with size of StackSize.
// VM.sp is initialized to 0, as you might expect.
func New(bytecode *compiler.Bytecode) *VM {
	return &VM{
		constants:    bytecode.Constants,
		instructions: bytecode.Instructions,
		stack:        make([]object.Object, StackSize),
		sp:           0,
		globals:      make([]object.Object, GlobalsSize),
	}
}

// NewWithGlobalsStore returns a pointer to a new VM with preset Globals.
func NewWithGlobalsStore(bytecode *compiler.Bytecode, s []object.Object) *VM {
	vm := New(bytecode)
	vm.globals = s
	return vm
}

// StackTop returns an object.Object on top of the stack, object at position VM.stack[VM.sp - 1], to the caller.
// If the VM stack is empty, nil is returned to the caller instead!
// This is a DEPRECATED method. You probably want to call LastPoppedStackElem instead, since VM auto-cleans the stack
// with expression statements and similar, meaning this method will give you incorrect results.
func (vm *VM) StackTop() object.Object {
	if vm.sp == 0 {
		return nil
	}
	return vm.stack[vm.sp-1]
}

// LastPoppedStackElem will return the last object.Object which the VM has popped of its stack.
func (vm *VM) LastPoppedStackElem() object.Object {
	// This works because our vm.sp always points at the NEXT spot on the stack (i.e. FREE SPOT where the next element will be pushed).
	// But we pop by just decrementing the stack pointer...We don't remove them from the underlying stack implementation (slice), they are not set to nil...
	// that spot is just marked as free, meaning we can simpy check what was there
	return vm.stack[vm.sp]
}

// Run is the heartbeat itself of the Monkey virtual machine, if you take analogy of the VM structure being a heart.
// It mimics and performs the fetch–decode–execute cycle (https://en.wikipedia.org/wiki/Instruction_cycle) on the
// bytecode generated by compiler.Compiler.
func (vm *VM) Run() error {
	for instructionPointer := 0; instructionPointer < len(vm.instructions); instructionPointer++ {
		// We take the first byte, which is always a OPCODE and we cast it said type.
		// Note that it's important that we do not use  code.Lookup here to get from a byte to an Opcode.
		// That would be far TOO SLOW. It costs time to move the byte around, lookup the opcode’s definition, return
		// it and take it apart.
		OPCODE := code.Opcode(vm.instructions[instructionPointer])
		switch OPCODE {
		case code.OpConstant:
			// code.OpConstant are uint16 indexes into the constants pool.
			// Therefore, what we need to do first is decode this number, the index.
			constIndex := code.ReadUint16(vm.instructions[instructionPointer+1:])
			instructionPointer += 2 // Again, since code.OpConstant is uint16 this means we need to advance 2 bytes afterward
			// This is the execute part of the fetch-decode-execute cycle for constants/literals
			// We push the constant onto the VM's stack!
			err := vm.push(vm.constants[constIndex])
			if err != nil {
				return err
			}
		case code.OpAdd, code.OpSub, code.OpMul, code.OpDiv:
			err := vm.executeBinaryOperation(OPCODE)
			if err != nil {
				return err
			}
		case code.OpPop:
			_ = vm.pop()
		case code.OpTrue:
			err := vm.push(True)
			if err != nil {
				return err
			}
		case code.OpFalse:
			err := vm.push(False)
			if err != nil {
				return err
			}
		case code.OpEqual, code.OpNotEqual, code.OpGreaterThan:
			err := vm.executeComparison(OPCODE)
			if err != nil {
				return err
			}
		case code.OpBang:
			err := vm.executeBangOperator()
			if err != nil {
				return err
			}
		case code.OpMinus:
			err := vm.executeMinusOperator()
			if err != nil {
				return err
			}
		case code.OpJump:
			pos := int(code.ReadUint16(vm.instructions[instructionPointer+1:]))
			// Since we are looping through the array and incrementing Instruction Pointer each time, we have to set it
			// just before the instruction we want here
			instructionPointer = pos - 1
		case code.OpJumpNotTruthy:
			pos := int(code.ReadUint16(vm.instructions[instructionPointer+1:]))
			instructionPointer += 2 // Skip over the address encoded in OpJumpNotTruthy
			condition := vm.pop()
			if !isTruthy(condition) {
				instructionPointer = pos - 1
			}
		case code.OpNull:
			err := vm.push(Null)
			if err != nil {
				return err
			}
		case code.OpSetGlobal:
			globalIndex := code.ReadUint16(vm.instructions[instructionPointer+1:])
			instructionPointer += 2
			vm.globals[globalIndex] = vm.pop()
		case code.OpGetGlobal:
			globalIndex := code.ReadUint16(vm.instructions[instructionPointer+1:])
			instructionPointer += 2
			err := vm.push(vm.globals[int(globalIndex)])
			if err != nil {
				return err
			}
		}

	}
	return nil
}

// isTruthy evaluates truthy monkey expressions and returns a Go native bool
func isTruthy(obj object.Object) bool {
	switch obj := obj.(type) {
	case *object.Boolean:
		return obj.Value
	case *object.Null:
		return false
	default:
		return true
	}
}

// executeMinusOperator handle the - prefix operator on integer operands.
// An error might be returned if the operand is not of type *object.Integer or if the VM stack overflows.
func (vm *VM) executeMinusOperator() error {
	operand := vm.pop()
	operandInteger, ok := operand.(*object.Integer)
	if !ok {
		return errors.New("operand is not an integer: " + string(operand.Type()))
	}
	if operandInteger.Type() != object.INTEGER_OBJ {
		return errors.New("operand is not an integer: " + string(operand.Type()))
	}
	integerValue := operandInteger.Value
	return vm.push(&object.Integer{Value: -integerValue})
}

// executeBangOperator handles the ! prefix operator on boolean values according to predefined logic.
// An error might be returned indicating a stack overflow in Monkey VM.
func (vm *VM) executeBangOperator() error {
	operand := vm.pop()

	switch operand {
	case True:
		return vm.push(False)
	case False:
		return vm.push(True)
	case Null:
		return vm.push(True)
	default:
		return vm.push(False)
	}
}

// executeComparison tries to evaluate boolean expressions (true != false, etc...) on the last two stack objects.
// If it fails an error is returned to the caller otherwise the boolean expression is evaluated, the operands are popped
// from the stack and the result is placed on it.
func (vm *VM) executeComparison(OPCODE code.Opcode) error {
	right, left := vm.pop(), vm.pop()
	if left.Type() == object.INTEGER_OBJ && right.Type() == object.INTEGER_OBJ {
		return vm.executeIntegerComparison(OPCODE, left, right)

	}
	switch OPCODE {
	case code.OpEqual:
		return vm.push(nativeBoolToBooleanObject(right == left))
	case code.OpNotEqual:
		return vm.push(nativeBoolToBooleanObject(right != left))
	default:
		return fmt.Errorf("unknown operator: %d (%s %s)", OPCODE, left.Type(), right.Type())
	}
}

// executeIntegerComparison is similar to executeComparison, but it's specific to *object.Integer objects.
// Both the left and the right have to be of this type...
func (vm *VM) executeIntegerComparison(op code.Opcode, left, right object.Object) error {
	leftIntegerObject, ok := left.(*object.Integer)
	if !ok {
		return errors.New("left object is not an integer: " + string(left.Type()))
	}
	rightIntegerObject, ok := right.(*object.Integer)
	if !ok {
		return errors.New("right object is not an integer: " + string(right.Type()))
	}
	leftValue, rightValue := leftIntegerObject.Value, rightIntegerObject.Value
	switch op {
	case code.OpEqual:
		return vm.push(nativeBoolToBooleanObject(rightValue == leftValue))
	case code.OpNotEqual:
		return vm.push(nativeBoolToBooleanObject(rightValue != leftValue))
	case code.OpGreaterThan:
		return vm.push(nativeBoolToBooleanObject(leftValue > rightValue))
	default:
		return fmt.Errorf("unknown operator: %d", op)
	}
}

// nativeBoolToBooleanObject transforms go booleans (true, false) into Monkey booleans (True, False).
// It is just a simple helper function.
func nativeBoolToBooleanObject(input bool) *object.Boolean {
	if input {
		return True
	}
	return False
}

// executeBinaryOperation executes all the infix operations by delegating them to the appropriate subroutines.
// If an error is produced (either by the called subroutine or by the fact that executeBinaryOperation does not know
// how to delegate work correctly) it's returned to the caller!
func (vm *VM) executeBinaryOperation(op code.Opcode) error {
	right, left := vm.pop(), vm.pop()
	leftType, rightType := left.Type(), right.Type()

	if leftType == object.INTEGER_OBJ && rightType == object.INTEGER_OBJ {
		return vm.executeBinaryIntegerOperation(op, left, right)
	}
	return errors.New("unsupported types for binary operation: left=" + string(leftType) + " right=" + string(rightType))
}

// executeBinaryIntegerOperation knows how to perform arithmetic between integers.
// If an error is encountered (like one of the operands, left or right not being a *object.Integer or the VM encountering
// a stack overflow) this error is returned to the caller.
// Otherwise, the resulting value is pushed onto the VM stack.
func (vm *VM) executeBinaryIntegerOperation(op code.Opcode, left, right object.Object) error {
	leftType, ok := left.(*object.Integer)
	if !ok {
		return errors.New(string("invalid operand type on the left side: " + leftType.Type()))
	}
	rightType, ok := right.(*object.Integer)
	if !ok {
		return errors.New(string("invalid operand type on the right side: " + rightType.Type()))
	}
	leftValue, rightValue := leftType.Value, rightType.Value
	var result int64
	switch op {
	case code.OpAdd:
		result = leftValue + rightValue
	case code.OpSub:
		result = leftValue - rightValue
	case code.OpMul:
		result = leftValue * rightValue
	case code.OpDiv:
		result = leftValue / rightValue
	default:
		return fmt.Errorf("unknown integer operator: %d", op)
	}
	return vm.push(&object.Integer{Value: result})
}

// push taken an object.Object and tries to push it onto the VM stack.
// To try and do this, it first checks that VM stack size is not equal or larger to StackSize. If it is, an error of
// stack overflow is returned to the caller.
// Otherwise, it adds the object to the VM stack at the current stack pointer (sp) and then increments this sp.
// In case of success nil is returned to the caller.
func (vm *VM) push(o object.Object) error {
	if vm.sp >= StackSize {
		return errors.New("stack overflow")
	}
	vm.stack[vm.sp] = o
	vm.sp++
	return nil
}

// pop pops an object from VM stack and decrements the stack pointer, sp.
// Popped object is returned to the caller.
// If VM's stack is empty, a nil is returned to the caller instead!
func (vm *VM) pop() object.Object {
	if vm.sp == 0 {
		return nil
	}
	topObject := vm.stack[vm.sp-1]
	vm.sp--
	return topObject
}
