book('Moby Dick', 'Herman Melville').
book('Ready Player One', 'Ernest Cline').
book('Enders Game', 'Orson Scott Card').
book('Speaker for the Dead', 'Orson Scott Card').
book('Enders Shadow', 'Orson Scott Card').
book('Children of the Mind', 'Orson Scott Card').
book('Ender in Exile', 'Orson Scott Card').
book('The Great Gatsby', 'F. Scott Fitzgerald').
book('The Curious Case of Benjamin Button', 'F. Scott Fitzgerald').
book('The Grapes of Wrath', 'John Steinbeck').
book('1984', 'George Orwell').
book('Animal Farm: A Fairy Story', 'George Orwell').
book('Catch-22', 'Joseph Heller').
book('To Kill a Mockingbird', 'Harper Lee').
book('Lord of the Flies', 'William Golding').
book('The Chronicles of Narnia', 'C.S. Lewis').

only_book(Name, Author) :-
    bagof(Allname, book(Allname, Author), Result),
    length(Result, 1),
    book(Name, Author).

books_by(Name, Author) :-
    book(Name, Author).
