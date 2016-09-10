Number divide := Number getSlot("/")
Number / := method(arg, if(arg == 0, 0, self divide(arg)))
