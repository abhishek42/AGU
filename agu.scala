// Implementation of address generation unit
package agu

import Chisel._

class aguData extends Bundle{
		val baseAddress    = UInt(INPUT, width = 32) 			//											
		val stride         = UInt(INPUT, width = 32)			//can be kept constant(=1) as the data is not sparse
		val filterRows 	   = UInt(INPUT, width = 32)			 // terminology 
		val filterCols     = UInt(INPUT, width = 32)			//equal to the span
		val skip           = UInt(INPUT, width = 32)			// see if it has to be calculated or received from metadata

		//val done 		   = Bool(OUTPUT)
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
	val numCols 			 = Reg(init = UInt(0, width = 32))
	val numRows				 = Reg(init = UInt(0, width = 32))
	val finalAddressTemp     = Reg(init = UInt(0, width = 32))			
	val pRow  				 = Reg(init = UInt(0, width = 32))
	val pCols 				 = Reg(init = UInt(0, width = 32))
	val rStride 			 = Reg(init = UInt(0, width = 32))
	val rSkip 			 	 = Reg(init = UInt(0, width = 32))
	val baseAddressConst	 = Reg(init = UInt(0, width = 32))

    when(io.in.valid){
		//io.finalAddress.bits 	 := io.in.bits.baseAddress
		rBaseAddress			 := io.in.bits.baseAddress
		tempBaseAddress 		 := io.in.bits.baseAddress
		baseAddressConst		 := io.in.bits.baseAddress
		numCols 				 := io.in.bits.filterCols
		numRows					 := io.in.bits.filterRows
		rStride 				 := io.in.bits.stride
		rSkip					 := io.in.bits.skip
		io.in.ready				 := Bool(true)
	}
	io.finalAddress.bits  := finalAddressTemp
		

		//<WHEN IT CAN NOT GO INSIDE A BLOCK AND TAKE THE ASSIGNMENT IT USES THE DEFAULT OR INITIALIZED VALUE>
		//<MAKE SURE THE OUTPUT IS GOING OUT ONLY WHEN IT IS CORRECT I>E TAKE CARE OF THE DELAYS ASSOCIATED>



	when(io.finalAddress.ready){
		io.finalAddress.valid := Bool(false)
		when(pRow < numRows){
			when(pCols < numCols){
				finalAddressTemp := tempBaseAddress + rStride*pCols

				when(tempBaseAddress === UInt(0) && rBaseAddress === UInt(0)){						//UInt(0) => Value with which we have initialized tempBaseAddress
					io.finalAddress.valid := Bool(false)
				}.elsewhen(tempBaseAddress === baseAddressConst && rBaseAddress === baseAddressConst){
					io.finalAddress.valid := Bool(true)
				}

				when(io.finalAddress.valid){
					io.finalAddress.bits := finalAddressTemp
					pCols := pCols + UInt(1)
				}
			}


			when(pCols === numCols && !io.finalAddress.valid){
				//<the address output at this time should be the previous address only and valid should go low as it is not the correct address output>
				pRow := pRow + UInt(1)
				pCols := UInt(0)
			}
		}
		when(pRow === numRows){
			io.finalAddress.valid := Bool(false)
		}
		tempBaseAddress  := rBaseAddress + pRow*((numCols+rSkip)*UInt(1))		 			//calculating the  temporary base address of each rows first element 
	}

		

	/*when(io.finalAddress.ready){
		io.finalAddress.valid             := Bool(false)
		when(pRow <= numRows){					
			when(pCols <= numCols){
				tempBaseAddress			  := rBaseAddress + pRow*((numCols+rSkip)*UInt(1))			//Block_size		
				finalAddressTemp       := tempBaseAddress + rStride*pCols

				//<Condition for valid signal assertion>
			
				unless(rBaseAddress === UInt(0) && tempBaseAddress === UInt(0)){
					io.finalAddress.valid := Bool(true)
				}

				when(pCols <= numCols && io.finalAddress.valid){
					io.finalAddress.bits  := finalAddressTemp
					pCols                 := pCols + UInt(1)										
				}
			}

			when(pCols === numCols && io.finalAddress.valid){
	 			io.finalAddress.bits      := finalAddressTemp + rSkip
				tempBaseAddress			  := rBaseAddress + pRow*((numCols+rSkip)*UInt(1))			//Block_size		
				pRow 		         	  := pRow + UInt(1)
				pCols 					  := UInt(0)
			}
		}
		when(pRow === numRows){
			io.finalAddress.valid         := Bool(false)
		}
	}*/
}


class aguTests(c: agu) extends Tester(c){

	poke(c.io.finalAddress.ready,0)
	poke(c.io.in.valid,1)
	poke(c.io.in.bits.baseAddress,5)
	poke(c.io.in.bits.stride,1)
	poke(c.io.in.bits.skip,10)
	poke(c.io.in.bits.filterCols,3)
	poke(c.io.in.bits.filterRows,3)


	println("pRow:"+peek(c.pRow)+
			" tempAddr:"+peek(c.tempBaseAddress)+
			" ready:"+peek(c.io.in.ready)+
			" BA:"+peek(c.baseAddressConst)+
			" rBase:"+peek(c.rBaseAddress)+
			" rStride:"+peek(c.rStride)+
			" rSkip:"+peek(c.rSkip)+
			" |pCols:"+peek(c.pCols)+
			" finalAddress.valid:"+peek(c.io.finalAddress.valid)+
			" finalAddr:"+peek(c.io.finalAddress.bits))

	for(i <- 0 until 9){
		//peek(c.io.finalAddress.bits)
		//peek(c.io.finalAddress.valid)

		step(1)
		poke(c.io.finalAddress.ready,1)
			println("pRow:"+peek(c.pRow)+
			" tempAddr:"+peek(c.tempBaseAddress)+
			" ready:"+peek(c.io.in.ready)+
			" BA:"+peek(c.baseAddressConst)+
			" rBase:"+peek(c.rBaseAddress)+
			" rStride:"+peek(c.rStride)+
			" rSkip:"+peek(c.rSkip)+
			" |pCols:"+peek(c.pCols)+
			" finalAddress.valid:"+peek(c.io.finalAddress.valid)+
			" finalAddr:"+peek(c.io.finalAddress.bits))
	}
	
}

object agu {
  def main(args: Array[String]): Unit = {
    val tutArgs = args.slice(1, args.length)
    chiselMainTest(tutArgs, () => Module(new agu())) {
      c => new aguTests(c) }
  }
}