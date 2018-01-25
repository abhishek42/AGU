package agu

import Chisel._
//import Config._
//import Chisel.utils._

object AGUDefines{
	val ADDRWIDTH		   = 32
	val STRIDEWIDTH		   = 32
	val SPANWIDTH		   = 32
	val SKIPWIDTH		   = 32
	val NUMBANKS           = 4							//or should i keep it as cache line width
	val NUMBANKSWIDTH      = log2Up(NUMBANKS)
	val HIGHBIT			   = NUMBANKSWIDTH - 1
	val FIFODEPTH		   = 4
	val NUMCLWIDTH		   = 32
}

class aguData extends Bundle{
	val baseAddress    	   = UInt(INPUT, width = AGUDefines.ADDRWIDTH) 												
	val stride             = UInt(INPUT, width = AGUDefines.STRIDEWIDTH)			
	val span		       = UInt(INPUT, width = AGUDefines.SPANWIDTH)
	val skip               = UInt(INPUT, width = AGUDefines.SKIPWIDTH)		
}

class agu extends Module{
	val io = new Bundle{
		val extIP          = Decoupled(new aguData).flip  								//Valid(new aguData).flip		//see if decoupled
		val aguAddr        = Decoupled(UInt(width = AGUDefines.ADDRWIDTH))				//Valid(UInt(width = 32))		//see if decoupled 
	}

	val fifo 			   = Module(new Queue(UInt(width = AGUDefines.ADDRWIDTH),AGUDefines.FIFODEPTH))

 	val countReg 	       = RegInit(UInt(0, width = AGUDefines.ADDRWIDTH))
	val rbaseAddress       = RegInit(UInt(0, width = AGUDefines.ADDRWIDTH))
	val clNum	           = RegInit(UInt(0, width = AGUDefines.NUMCLWIDTH))
	val constClNum	       = RegInit(UInt(0, width = AGUDefines.NUMCLWIDTH))
	val winWidthReg	       = RegInit(UInt(0, width = AGUDefines.SPANWIDTH))
	//var winWidthReg	   = UInt()
	val fEnqValidReg       = RegInit(Bool(false))
	val startReg           = RegInit(Bool(true))
	val aguValidReg		   = RegInit(Bool(false))

	io.extIP.ready         := startReg

 	io.aguAddr.bits        := fifo.io.deq.bits	
 	io.aguAddr.valid       := fifo.io.deq.valid

 	fifo.io.enq.bits       := countReg
 	fifo.io.enq.valid      := fEnqValidReg
 	fifo.io.deq.ready      := io.aguAddr.ready						


 	when(io.extIP.valid && startReg && fifo.io.enq.ready){
 		countReg           := io.extIP.bits.baseAddress 
 		clNum		       := ((io.extIP.bits.baseAddress(AGUDefines.HIGHBIT,0) + io.extIP.bits.span)>> UInt(AGUDefines.NUMBANKSWIDTH)) + UInt(1)				// ciel function = [bank_number + span]/ number_of_banks + 1 will give us the number of cache lines to access
 		constClNum		   := ((io.extIP.bits.baseAddress(AGUDefines.HIGHBIT,0) + io.extIP.bits.span)>> UInt(AGUDefines.NUMBANKSWIDTH)) + UInt(1)				// required to take care of the delays associated with invlidating the last calcuated value
 		winWidthReg	       := io.extIP.bits.span			
 		startReg           := Bool(false)
 		fEnqValidReg       := Bool(true)
 		when(winWidthReg   != UInt(0)){																					// kind off hard coded [don't want it to get updated in the first cycle, update only when initialized with correct value]
 			winWidthReg	   := winWidthReg - UInt(1)
 		}
 	} .elsewhen(fifo.io.enq.ready && (clNum != UInt(0))){
 		when(clNum === constClNum){
 			aguValidReg    := Bool(false)
 			io.aguAddr.valid := aguValidReg
 		}
 		countReg           := countReg + UInt(AGUDefines.NUMBANKS)
 		rbaseAddress	   := countReg + io.extIP.bits.skip*UInt(AGUDefines.NUMBANKS)			//redundant but rbaseAddress will be calculated using the last address calculated
 		clNum              := clNum - UInt(1)
 		fEnqValidReg       := Bool(true)
	} .elsewhen(fifo.io.enq.ready && (winWidthReg != UInt(0))){
 		//io.aguAddr.valid   := Bool(false)
 		countReg           := rbaseAddress
 		winWidthReg        := winWidthReg - UInt(1)
 		fEnqValidReg       := Bool(true)
 		clNum		       := ((io.extIP.bits.baseAddress(AGUDefines.HIGHBIT,0) + io.extIP.bits.span)>> UInt(AGUDefines.NUMBANKSWIDTH)) + UInt(1)
 		constClNum		   := ((io.extIP.bits.baseAddress(AGUDefines.HIGHBIT,0) + io.extIP.bits.span)>> UInt(AGUDefines.NUMBANKSWIDTH)) + UInt(1)
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
	poke(c.io.extIP.bits.baseAddress,7)
	poke(c.io.extIP.bits.stride,1)
	poke(c.io.extIP.bits.span,17)
	poke(c.io.extIP.bits.skip,10)
	poke(c.io.extIP.valid,1)
	poke(c.io.aguAddr.ready,1)
	peek(c.io.aguAddr.valid)
	peek(c.io.aguAddr.bits)
	peek(c.clNum)
	peek(c.countReg)
	peek(c.startReg)
	peek(c.rbaseAddress)
	peek(c.winWidthReg)
	println("addr:"+peek(c.io.aguAddr.bits))
	println("valid:"+peek(c.io.aguAddr.valid))
	
	for(i <- 0 until 8){
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

	for(i <- 0 until 4){
		poke(c.io.aguAddr.ready,0)
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
