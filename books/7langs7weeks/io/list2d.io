List2d := List clone
List2d dimx := 0
List2d dimy := 0

List2d dim := method(x, y,
    self dimx := x
    self dimy := y
    setSize(x)
    mapInPlace(_, child := list(); child setSize(y))
)

List2d set := method(x, y, value,
    at(x) atPut(y, value)
    self
)

List2d get := method(x, y,
    at(x) at(y)
)

List2d transpose := method(
    newlist := List2d clone dim(dimx, dimy)

    foreach(x, array,
        array foreach(y, value,
            newlist set(y, x, value)
        )
    )

    return newlist
)

List2d tofile := method(filename,
    file := File clone openForUpdating(filename)
    foreach(array,
        file write(array join(",") .. "\n")
    )
    file close
    self
)

# Currently not handling anything but fresh start case.
List2d fromfile := method(filename,
    file := File clone openForReading(filename)
    while(line := file readLine,
        append (line split(","))
    )
    self dimx := size
    self dimy := at(0) size
)
