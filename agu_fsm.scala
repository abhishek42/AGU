package agu

import Chisel._

class aguData extends Bundle{
		val baseAddress    = UInt(INPUT, width = 32) 			
		val stride         = UInt(INPUT, width = 32)			
		val span 		   = UInt(INPUT, wisth = 32)	
		val skip           = UInt(INPUT, width = 32)	
		val rows		   = UInt(INPUT, width = 32)
}

class agu extends Module{
	val io = new Bundle{
		val in = Decoupled(new aguData).flip
		val finalAddress   = Decoupled(UInt(width = 32))			
	}

	io.finalAddress.bits 	 := UInt(4, width = 32)
 	io.finalAddress.valid    := Bool(false)					
 	io.in.ready				 := Bool(false)		
	val rBaseAddress 	     = Reg(init = UInt(0, width = 32))
	val tempBaseAddress 	 = Reg(init = UInt(0, width = 32))   	//init = initialization value on reset
	val finalAddressTemp     = Reg(init = UInt(0, width = 32))			
	val rStride 			 = Reg(init = UInt(0, width = 32))
	val rSkip 			 	 = Reg(init = UInt(0, width = 32))
	val rSpan				 = Reg(init = UInt(0, width = 32))
	val numRow				 = Reg(init = UInt(0, width = 32))

	when(io.in.valid){
		rBaseAddress			 := io.in.bits.baseAddress
		tempBaseAddress 		 := io.in.bits.baseAddress
	//	baseAddressConst		 := io.in.bits.baseAddress
		rSpan					 := io.in.bits.span
		rStride 				 := io.in.bits.stride
		rSkip					 := io.in.bits.skip
		numRow					 := io.in.bits.numRow
		io.in.ready				 := Bool(true)
	}
	io.finalAddress.bits  := finalAddressTemp


	when(io.finalAddress.ready){
		io.finalAddress.valid := Bool(false)
		//Have a counter here to check on the value of number of rows

	}
}