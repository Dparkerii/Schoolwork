
.data
limit: .word 3999
newline: .asciiz "\n"
tab: .asciiz "\t"
array: .byte 'I','V','X','L','C','D','M'
d2rnerror: .asciiz "Number was reduced below 0"
tr: .asciiz "True"
fl: .asciiz "False"
lt: .word 0xFF000000
lm: .word 0x00FF0000
rm: .word 0x0000FF00
rt: .word 0x000000FF

#s0 = limit
#s1 = counter/initial input
#s2 = 4 left most chars of rn
#s3 = 4 left middle
#s4 = 4 right middle
#s5 = 4 right most
#s6 = result from rn2d
#s7 = return address

.text
#generate 1-3999
	lw $s0,limit		#initialize limit  s0 = 3999
generater:
	addi $s1,$s1,1		#initialize counter/increment	s1 = counter/initial input
	la $a0,($s1)		#print counter
	li $v0,1
	syscall
	la $a0,tab		#tab for formatting
	li $v0,4
	syscall
	jal d2rn		#do subroutine to convert to roman numeral
	la $a0,tab		#tab for formatting
	li $v0,4
	syscall
	jal rn2d		#convert roman numeral back to decimal
	la $a0,tab		#tab for formatting
	li $v0,4
	syscall
	la $a0,($s6)
	li $v0,1
	syscall
	la $a0,tab		#tab for formatting
	li $v0,4
	syscall
	jal compare		#compare both numbers
	la $a0,newline		#newline for formatting
	li $v0,4
	syscall
	sub $s2,$s2,$s2		#clear registers for next iteration
	sub $s3,$s3,$s3
	sub $s4,$s4,$s4
	sub $s5,$s5,$s5
	sub $s6,$s6,$s6
	sub $s7,$s7,$s7
	blt $s1,$s0,generater	#loop back until limit is reached
	j exit
d2rn:#convert each to roman numeral
	add $t0, $t0, $s1	#get decimal value to modify 	t0 = value to modify
	sub $t9, $t9, $t9	#counter for storage
	la $s7, ($ra)
	d2rnret:
	bge $t0,1000,thousand	#thousand
	bge $t0,900,ninehund	#ninehund
	bge $t0,500,fivehund	#fivehund
	bge $t0,400,fourhund	#fourhund
	bge $t0,100,onehund	#onehund
	bge $t0,90,ninety	#ninety
	bge $t0,50,fifty	#fifty
	bge $t0,40,fourty	#fourty
	bge $t0,10,ten		#ten
	bge $t0,9,nine		#nine
	bge $t0,5,five		#five
	bge $t0,4,four		#four
	bge $t0,1,one		#one
	blt $t0,$zero,error1	#something went wrong
	la $ra,($s7)
	jr $ra			#exit loop
#convert back to decimal
rn2d:
	#can tell which bit to start with based on t9 value
	#edge cases of 1, 2, 3 after that just go until anded value = 0
	#t9 is how many letters
	sub $t7,$t7,$t7		#clear counter that counts up to $t9 value
	la $s7,($ra)		#save return address
	rn2dret:	
	blt $t7,4,wone		#load $s2
	blt $t7,8,wtwo		#load $s3
	blt $t7,12,wthree	#load $s4
	b wfour			#load $s5
	wone:
		la $a3,($s2)
		jal getchar
		b getval
	wtwo:
		la $a3,($s3)
		jal getchar
		b getval
	wthree:
		la $a3,($s4)
		jal getchar
		b getval
	wfour:
		la $a3,($s5)
		jal getchar
		b getval
	
	getval:
		#need to shift t6 into correct position using modulation trick
		la $t1,array
		mfhi $t5		#get shift amount
		mul $t5,$t5,-8
		addi $t5,$t5,24
		
		lb $t6,6($t1)		#check if M
		sllv $t6,$t6,$t5
		beq $v0,$t6,M
		
		lb $t6,4($t1)		#check if C
		sllv $t6,$t6,$t5
		beq $v0,$t6,C
		
		lb $t6,5($t1)		#check if D
		sllv $t6,$t6,$t5
		beq $v0,$t6,D
		
		lb $t6,3($t1)		#check if L
		sllv $t6,$t6,$t5
		beq $v0,$t6,L

		lb $t6,2($t1)		#check if X
		sllv $t6,$t6,$t5
		beq $v0,$t6,X
		
		lb $t6,1($t1)		#check if V
		sllv $t6,$t6,$t5
		beq $v0,$t6,V
		
		lb $t6,0($t1)		#check if I
		sllv $t6,$t6,$t5
		beq $v0,$t6,I

		b exit
		M:
			addi $s6,$s6,1000
			addi $t7,$t7,1
			bne $t9,$t7,rn2dret	#more elements left to sum
			b done
		#C
		
		C:
			addi $t7,$t7,1
			jal getchar
			lb $t5,6($t1)
			beq $v0,$t5,CM
			lb $t5,5($t1)
			beq $v0,$t5,CD
			addi $s6,$s6,100
			bne $t9,$t7,rn2dret
			b done
			CM:
				addi $s6,$s6,900
				addi $t7,$t7,1
				bne $t9,$t7,rn2dret
				b done
			CD:
				addi $s6,$s6,400
				addi $t7,$t7,1
				bne $t9,$t7,rn2dret
				b done
		#D
		
		D:
			addi $s6,$s6,500
			addi $t7,$t7,1
			bne $t9,$t7,rn2dret	#more elements left to sum
			b done
		#L
		L:
			addi $s6,$s6,50
			addi $t7,$t7,1
			bne $t9,$t7,rn2dret	#more elements left to sum
			b done
		#X
		X:
			addi $t7,$t7,1
			jal getchar
			lb $t5,4($t1)
			beq $v0,$t5,XC
			lb $t5,3($t1)
			beq $v0,$t5,XL
			#addi $t7,$t7,-1
			addi $s6,$s6,10
			bne $t9,$t7,rn2dret
			b done
			XC:
				addi $s6,$s6,90
				addi $t7,$t7,1
				bne $t9,$t7,rn2dret
				b done
			XL:
				addi $s6,$s6,40
				addi $t7,$t7,1
				bne $t9,$t7,rn2dret
				b done

		#V
		V:
			addi $t7,$t7,1
			addi $s6,$s6,5
			bne $t9,$t7,rn2dret	#more elements left to sum
			b done
		#I
		I:
			addi $t7,$t7,1
			beq $t9,$t7,JI
			jal getchar
			mfhi $t5
			mul $t5,$t5,-8
			addi $t5,$t5,24
			lb $t4,2($t1)
			sllv $t4,$t4,$t5
			beq $v0,$t4,IX
			lb $t4,1($t1)
			sllv $t4,$t4,$t5
			beq $v0,$t4,IV
			
			JI:
				addi $s6,$s6,1
				bne $t9,$t7,rn2dret
				b done
			IX:
				addi $s6,$s6,9
				addi $t7,$t7,1
				bne $t9,$t7,rn2dret
				b done
			IV:
				addi $s6,$s6,4
				addi $t7,$t7,1
				bne $t9,$t7,rn2dret
				b done

	done:
		la $ra,($s7)
		jr $ra
		
	
getchar:
	sub $t8,$t8,$t8
	addi $t8,$t8,4
	div $t7, $t8
	sub $t8,$t8,$t8
	mfhi $t8		#get remainder
	beqz $t8,char0		#left most byte
	beq $t8,1,char1		#left middle byte
	beq $t8,2,char2		#right middle byte
	beq $t8,3,char3		#right most byte
	char0:
		lw $t8,lt
		and $v0,$a3,$t8
		jr $ra
	char1:
		lw $t8,lm
		and $v0,$a3,$t8
		jr $ra
	char2:
		lw $t8,rm
		and $v0,$a3,$t8
		jr $ra
	char3:
		lw $t8,rt
		and $v0,$a3,$t8
		jr $ra

#compare original to converted
compare:
	bne $s1,$s6,false
	la $a0,tr	#true
	li $v0,4
	syscall
	jr $ra

false:
	la $a0,fl	#false
	li $v0,4
	syscall
	jr $ra

#if illegal roman numeral given, return 0 or -#
error1:
	la $a0,d2rnerror
	li $v0,4
	syscall
	j exit

exit:
	li $v0,10
	syscall

thousand:#M
	subi $t0,$t0,1000
	la $t1,array
			#print M
	lb $a0,6($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
	b d2rnret
ninehund:#CM
	subi $t0,$t0,900
	la $t1,array
			#print C
	lb $a0,4($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
			#print M
	lb $a0,6($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
	b d2rnret
fivehund:#D
	subi $t0,$t0,500
	la $t1,array
			#print D
	lb $a0,5($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
	b d2rnret
fourhund:#CD
	subi $t0,$t0,400
	la $t1,array
			#print C
	lb $a0,4($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
			#print D
	lb $a0,5($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
	b d2rnret
onehund:#C
	subi $t0,$t0,100
	la $t1,array
			#print C
	lb $a0,4($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
	b d2rnret
ninety:#XC
	subi $t0,$t0,90
	la $t1,array
			#print X
	lb $a0,2($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
			#print C
	lb $a0,4($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
	b d2rnret
fifty:#L
	subi $t0,$t0,50
	la $t1,array
			#print L
	lb $a0,3($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
	b d2rnret
fourty:#XL
	subi $t0,$t0,40
	la $t1,array
			#print X
	lb $a0,2($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
			#print L
	lb $a0,3($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
	b d2rnret
ten:#X
	subi $t0,$t0,10
	la $t1,array
			#print X
	lb $a0,2($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
	b d2rnret
nine:#IX
	subi $t0,$t0,9
	la $t1,array
			#print I
	lb $a0,0($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
			#print X
	lb $a0,2($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage

	b d2rnret
five:#V
	subi $t0,$t0,5
	la $t1,array
			#print V
	lb $a0,1($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
	b d2rnret
four:#IV
	subi $t0,$t0,4
	la $t1,array
			#print I
	lb $a0,0($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
			#print V
	lb $a0,1($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
	b d2rnret
one:#I
	subi $t0,$t0,1
	la $t1,array
			#print I
	lb $a0,0($t1)
	li $v0,11
	syscall
	jal storage	#store roman numeral
	addi, $t9,$t9,1	#count byte for storage
	b d2rnret
	
storage:
	bgt $t9,11,ls5	#store in right most word
	bgt $t9,7,ls4	#store in middle right word
	bgt $t9,3,ls3	#store in middle left word
	
	#sll $s2,$s2,8	#make room for byte
	addi $t2,$t2,4
	div $t9,$t2
	mfhi $t2
	mul $t2,$t2,-8
	add $t2,$t2,24
	sllv $a0,$a0,$t2
	or $s2,$s2,$a0	#insert byte
	jr $ra
ls3:
	addi $t2,$t2,4
	div $t9,$t2
	mfhi $t2
	mul $t2,$t2,-8
	add $t2,$t2,24
	sllv $a0,$a0,$t2
	or $s3,$s3,$a0	#insert byte
	jr $ra
ls4:
	addi $t2,$t2,4
	div $t9,$t2
	mfhi $t2
	mul $t2,$t2,-8
	add $t2,$t2,24
	sllv $a0,$a0,$t2
	or $s4,$s4,$a0	#insert byte
	jr $ra
ls5:
	addi $t2,$t2,4
	div $t9,$t2
	mfhi $t2
	mul $t2,$t2,-8
	add $t2,$t2,24
	sllv $a0,$a0,$t2
	or $s5,$s5,$a0	#insert byte
	jr $ra
