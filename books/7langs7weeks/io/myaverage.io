List myaverage := method(
    total := 0
    self foreach(num,
        num hasProto(Number) ifFalse(
            Exception raise("List should only contain numbers")
        )
        total = total + num
    )
    return total / self size
)
