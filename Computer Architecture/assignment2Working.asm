#David R Parker II
#CS 465
#Assignment 2
#02/28/2017
#
# This program converts a 32 bit unsigned integer into binary coded decimal
# Most significant digit is the left most digit in $v1
# Least significant digit is the right most digit in $v0

.text
	addi $s0, $s0, 0xffffffff	#declare number to convert
	add $s7,$s7, $zero		#declare initial counter
	addi $s6, $s6, 10		#register with 10 in it	
	
	#display number value for testing purposes
	la $a0, ($s0)
	li $v0, 36
	syscall
	sub $v0, $v0, $v0
	
	first_reg:
		divu 	$s0, $s6		#get lowest digit
		mfhi 	$s1			#move remainder to s1
		mflo 	$s0			#move quotient digit to s0
		mul 	$s2, $s7, 4		#get amount to shift by
		sllv 	$s1, $s1, $s2		#shift digit into position
		or 	$v0, $v0, $s1		#store in v0
		addi 	$s7, $s7, 1		#increment counter
		beqz 	$s0, finished		#stop program if all digits extracted
		bne 	$s7,5,first_reg		#loop back for at least 5 digits
	sub 	$s7,$s7,$s7			#reset counter to zero
	next_reg:
		divu 	$s0, $s6		#get lowest digit
		mfhi 	$s1			#move remainder to s1
		mflo 	$s0			#move quotient digit to s0
		mul 	$s2, $s7, 4		#get amount to shift by
		sllv 	$s1, $s1, $s2		#shift digit into position
		or 	$v1, $v1, $s1		#store in v1
		addi 	$s7, $s7, 1		#increment counter
		beqz 	$s0, finished		#stop the program if all digits extracted
		bne 	$s7,5,next_reg		#loop back for at least 5 digits
	finished:
	
