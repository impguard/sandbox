Builder := Object clone do(
    tab := ""

    mapstr := method(map,
        result := "" asMutable
        map foreach(key, value,
            result appendSeq(" \"#{key}\"=\"#{value}\"" interpolate)
        )
        result
    )

    forward := method(
        args := call message arguments
        attrs := ""

        if (args size > 0 and args at(0) name == "curlyBrackets",
            attrs = mapstr(doMessage(args removeFirst))
        )

        writeln(tab, "<", call message name, attrs, ">")
        tab = tab .. "    "

        args foreach(arg,
            content := self doMessage(arg)
            if(content type == "Sequence", writeln(tab, content))
        )

        tab = tab exclusiveSlice(4)
        writeln(tab, "</", call message name, ">")
    )
)

Builder ul(
            li("Io"),
            li("Lua"),
            li("Javascript")
            )

Builder book({"author": "Tate"},
            page1("HI"),
            page2({"annotatation": "bleh"}, "BYE!"),
            page3("BEH")
            )
