package agu

import Chisel._

class aguData extends Bundle{
		val baseAddress    = UInt(INPUT, width = 32) 			//											
		val stride         = UInt(INPUT, width = 32)			//can be kept constant(=1) as the data is not sparse
		val filterRows 	   = UInt(INPUT, width = 32)			 // terminology 
		val filterCols     = UInt(INPUT, width = 32)			//equal to the span
		val skip           = UInt(INPUT, width = 32)			// see if it has to be calculated or received from metadata
}

class agu extends Module{
	val io = new Bundle{
		val extIP          = Valid(new aguData).flip		//see if decoupled
		val aguAddr        = Decoupled(UInt(width = 32))	//Valid(UInt(width = 32))		//see if decoupled 
	}

	val fifo         = Module(new mkSizedFIFOF(UInt(width=32),4)

 	val countReg 	 = RegInit(UInt(0,width = 32))
	val rbaseAddress = RegInit(UInt(0,width = 32))
	val clNum		 = RegInit(UInt(0,wdith = 32))
	val winWidthReg	 = RegInit(UInt(0,width = 32))
	val fEnqValidReg = RegInit(Bool(false))
	val startReg     = RegInit(Bool(true))



	winWidthReg		 := io.extIP.bits.filterRows		//not sure about this

 	aguAddr.bits     := fifo.deq.bits
 	aguAddr.valid    := fifo.deq.valid

 	fifo.enq.bits    := countReg
 	fifo.enq.valid   := fEnqValidReg
 	fifo.deq.ready   := aguAddr.ready						// not sure about this


 	when(io.extIP.valid && startReg && fifo.enq.ready){
 		countReg           := io.extIp.bits.baseAddress 
 		rbaseAddress       := io.extIp.bits.baseAddress + io.extIp.bits.skip
 		clNum		       := (io.extIp.bits.baseAddress(0:2)+io.extIp.bits.filterRows)>> UInt(3)						// yet to implement the ciel function
 		startReg	       := Bool(false)
 		fEnqValidReg       := Bool(true)
 		winWidthReg	       := winWidthReg - UInt(1)
 	}.elsewhen(clNum && fifo.enq.ready){
 		countReg           := countReg + UInt(1)
 		clNum              := clNum - UInt(1)
 		fEnqValidReg       := Bool(true)
 	}.elsewhen(winWidthReg && fifo.enq.ready){
 		countReg           := rbaseAddress
 		rbaseAddress       := rbaseAddress + io.extIp.bits.skip
 		winWidthReg        := winWidthReg - UInt(1)
 		fEnqValidReg       := Bool(true)
 		clNum		       := (io.extIp.bits.baseAddress(0:2) +  io.extIp.bits.filterRows)>> UInt(3)				// same ciel function as first when statement
 	}.elsewhen(!winWidthReg){
 		startReg	       := Bool(true)
 		fEnqValidReg       := Bool(false)
 	}.otherwise{
 		fEnqValidReg       := Bool(false)
 	}
}