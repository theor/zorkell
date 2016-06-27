module OpCodes where

data OpCode =
    O_0NNN --	Calls RCA 1802 program at address NNN. Not necessary for most ROMs.
  | O_00E0 --	Clears the screen.
  | O_00EE --	Returns from a subroutine.
  | O_1NNN --	Jumps to address NNN.
  | O_2NNN --	Calls subroutine at NNN.
  | O_3XNN --	Skips the next instruction if VX equals NN.
  | O_4XNN --	Skips the next instruction if VX doesn't equal NN.
  | O_5XY0 --	Skips the next instruction if VX equals VY.
  | O_6XNN --	Sets VX to NN.
  | O_7XNN --	Adds NN to VX.
  | O_8XY0 --	Sets VX to the value of VY.
  | O_8XY1 --	Sets VX to VX or VY.
  | O_8XY2 --	Sets VX to VX and VY.
  | O_8XY3 --	Sets VX to VX xor VY.
  | O_8XY4 --	Adds VY to VX. VF is set to 1 when there's a carry, and to 0 when there isn't.
  | O_8XY5 --	VY is subtracted from VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
  | O_8XY6 --	Shifts VX right by one. VF is set to the value of the least significant bit of VX before the shift.[2]
  | O_8XY7 --	Sets VX to VY minus VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
  | O_8XYE --	Shifts VX left by one. VF is set to the value of the most significant bit of VX before the shift.[2]
  | O_9XY0 --	Skips the next instruction if VX doesn't equal VY.
  | O_ANNN --	Sets I to the address NNN.
  | O_BNNN --	Jumps to the address NNN plus V0.
  | O_CXNN --	Sets VX to the result of a bitwise and operation on a random number and NN.
  | O_DXYN --	Sprites stored in memory at location in index register (I), 8bits wide. Wraps around the screen. If when drawn, clears a pixel, register VF is set to 1 otherwise it is zero. All drawing is XOR drawing (i.e. it toggles the screen pixels). Sprites are drawn starting at position VX, VY. N is the number of 8bit rows that need to be drawn. If N is greater than 1, second line continues at position VX, VY+1, and so on.
  | O_EX9E --	Skips the next instruction if the key stored in VX is pressed.
  | O_EXA1 --	Skips the next instruction if the key stored in VX isn't pressed.
  | O_FX07 --	Sets VX to the value of the delay timer.
  | O_FX0A --	A key press is awaited, and then stored in VX.
  | O_FX15 --	Sets the delay timer to VX.
  | O_FX18 --	Sets the sound timer to VX.
  | O_FX1E --	Adds VX to I.[3]
  | O_FX29 --	Sets I to the location of the sprite for the character in VX. Characters 0-F (in hexadecimal) are represented by a 4x5 font.
  | O_FX33 --	Stores the binary-coded decimal representation of VX, with the most significant of three digits at the address in I, the middle digit at I plus 1, and the least significant digit at I plus 2. (In other words, take the decimal representation of VX, place the hundreds digit in memory at location in I, the tens digit at location I+1, and the ones digit at location I+2.)
  | O_FX55 --	Stores V0 to VX (including VX) in memory starting at address I.[4]
  | O_FX65 --	Fills V0 to VX (including VX) with values from memory starting at address I.[4]
