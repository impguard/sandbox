thenum := (Random value * 99 + 1) floor
prevguess := nil
in := File standardInput

writeln("Guess a number between 1 and 100.")
for (i, 1, 10,
    write("What's your guess? ")
    guess := in readLine asNumber
    if (guess == thenum,
        writeln("You got it!")
        System exit
    )
    if (prevguess,
        prevclose := (prevguess - thenum) abs
        currclose := (guess - thenum) abs
        if (prevclose < currclose,
            writeln("Colder"),
            writeln("Warmer!")
        )
    ) else (
        writeln("Nope!")
    )
    prevguess := guess
)

writeln("Darn, better luck next time...")
writeln("My number was " .. thenum)
