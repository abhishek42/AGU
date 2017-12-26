package agu

import Chisel._
//import Chisel.utils._

class aguData extends Bundle{
		val baseAddress    = UInt(INPUT, width = 32) 			//											
		val stride         = UInt(INPUT, width = 32)			//can be kept constant(=1) as the data is not sparse
		val span		   = UInt(INPUT, width = 32)
		val skip           = UInt(INPUT, width = 32)			// see if it has to be calculated or received from metadata
		//val filterRows 	   = UInt(INPUT, width = 32)			 // terminology 
		//val filterCols     = UInt(INPUT, width = 32)			//equal to the span
}

class agu extends Module{
	val io = new Bundle{
		val extIP          = Decoupled(new aguData).flip  //Valid(new aguData).flip		//see if decoupled
		val aguAddr        = Decoupled(UInt(width = 32))	//Valid(UInt(width = 32))		//see if decoupled 
	}

	//val fifo         = Module(new mkSizedFIFOF(UInt(width=32),4)
	//val fifo 		       = Queue(io.extIP,4)
	val fifo 			   = Module(new Queue(UInt(width=32),4))


 	val countReg 	       = RegInit(UInt(0,width = 32))
	val rbaseAddress       = RegInit(UInt(0,width = 32))
	val clNum	           = RegInit(UInt(0,width = 32))
	val constClNum	           = RegInit(UInt(0,width = 32))
	val winWidthReg	       = RegInit(UInt(0,width = 32))
	//var winWidthReg	       = UInt()
	val fEnqValidReg       = RegInit(Bool(false))
	val startReg           = RegInit(Bool(true))
	val aguValidReg		   = RegInit(Bool(false))
	//io.extIP.ready		   := Bool(false)

	//winWidthReg	     	   := io.extIP.bits.span		//not sure about this
	io.extIP.ready         := startReg

 	io.aguAddr.bits        := fifo.io.deq.bits	
 	io.aguAddr.valid       := fifo.io.deq.valid

 	fifo.io.enq.bits       := countReg
 	fifo.io.enq.valid      := fEnqValidReg
 	fifo.io.deq.ready      := io.aguAddr.ready						// not sure about this


 	when(io.extIP.valid && startReg && fifo.io.enq.ready){
 		countReg           := io.extIP.bits.baseAddress 
 		clNum		       := ((io.extIP.bits.baseAddress(2,0) + io.extIP.bits.span)>> UInt(3)) + UInt(1)
 		constClNum		   := ((io.extIP.bits.baseAddress(2,0) + io.extIP.bits.span)>> UInt(3)) + UInt(1)				// required to take care of the delays associated with invlidating the last calcuated value
 		winWidthReg	       := io.extIP.bits.span		//not sure about this
 		//clNum			   := UInt(3)
 		//rbaseAddress       := io.extIP.bits.baseAddress + (io.extIP.bits.skip + clNum)*UInt(8)	
 		startReg           := Bool(false)
 		fEnqValidReg       := Bool(true)
 		when(winWidthReg != UInt(0)){
 			winWidthReg	       := winWidthReg - UInt(1)
 		}
 	} .elsewhen(fifo.io.enq.ready && (clNum != UInt(0))){
 		when(clNum === constClNum){
 			aguValidReg := Bool(false)
 			io.aguAddr.valid := aguValidReg
 		}
 		countReg           := countReg + UInt(8)
 		rbaseAddress	   := countReg + io.extIP.bits.skip*UInt(8)			//redundant but rbaseAddress will be calculated using the last address calculated
 		clNum              := clNum - UInt(1)
 		fEnqValidReg       := Bool(true)
 		//when(clNum === UInt(1)){aguValidReg := Bool(false)}
	} .elsewhen(fifo.io.enq.ready && (winWidthReg != UInt(0))){
 		//io.aguAddr.valid   := Bool(false)
 		countReg           := rbaseAddress
 		winWidthReg        := winWidthReg - UInt(1)
 		fEnqValidReg       := Bool(true)
 		clNum		       := ((io.extIP.bits.baseAddress(2,0) +  io.extIP.bits.span)>> UInt(3)) + UInt(1)
 		constClNum		   := ((io.extIP.bits.baseAddress(2,0) + io.extIP.bits.span)>> UInt(3)) + UInt(1)
 		//clNum			   := UInt(3)
 		//rbaseAddress       := rbaseAddress + (io.extIP.bits.skip + clNum)*UInt(8)
 	} .elsewhen(!winWidthReg){
 		startReg	       := Bool(true)
 		fEnqValidReg       := Bool(false)
 	} .otherwise{
 		fEnqValidReg       := Bool(false)
 	}
}


class aguTests(c: agu) extends Tester(c){
	//supplying the AGU inputs with initial values
	poke(c.io.extIP.bits.baseAddress,7)
	poke(c.io.extIP.bits.stride,1)
	poke(c.io.extIP.bits.span,17)
	//poke(c.io.extIP.bits.filterCols,4)
	poke(c.io.extIP.bits.skip,10)
	poke(c.io.extIP.valid,1)
	poke(c.io.aguAddr.ready,1)
	peek(c.io.aguAddr.valid)
	peek(c.io.aguAddr.bits)
	peek(c.clNum)
	peek(c.countReg)
	peek(c.startReg)
	peek(c.rbaseAddress)
	//peek(c.io.extIP.ready)
	//peek(c.fifo.io.enq.ready)
	peek(c.winWidthReg)
	println("addr:"+peek(c.io.aguAddr.bits))
	println("valid:"+peek(c.io.aguAddr.valid))
	
	for(i <- 0 until 16){
		peek(c.io.aguAddr.valid)
		peek(c.io.aguAddr.bits)
		peek(c.clNum)
		peek(c.countReg)
		//peek(c.io.extIP.valid)
		peek(c.startReg)
		//peek(c.fifo.io.enq.ready)
		peek(c.winWidthReg)
		peek(c.fifo.io.deq)
		peek(c.rbaseAddress)
		println("addr:"+peek(c.io.aguAddr.bits))
		println("valid:"+peek(c.io.aguAddr.valid))
		step(1)
		//val ready = Mux((i%4 == 0),0,1)
		//poke(c.io.aguAddr.ready, ready)
	}
}

object agu {
  def main(args: Array[String]): Unit = {
    val tutArgs = args.slice(1, args.length)
    chiselMainTest(tutArgs, () => Module(new agu())) {
      c => new aguTests(c) }
  }
}
