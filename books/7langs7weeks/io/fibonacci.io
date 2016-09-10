fibloops := method(num,
    prev := 0
    curr := 1
    for(i, 1, num - 1,
        next := curr + prev
        prev = curr
        curr = next
    )
    return curr
)

fibrec := method(num,
    if(num == 1 or num == 2,
        1,
        fibrec(num - 1) + fibrec(num - 2)
    )
)
