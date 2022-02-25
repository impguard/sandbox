// Honestly this example is a shitshow. I don't think this is the best way to use/think about
// modules but it functionally makes sense.

// Essentially namespaces in C++
// grouping things together under a namespace.
// Can one add to a namespace?

mod front_of_house {
    pub mod hosting {
        pub fn add_to_waitlist() {}

        fn seat_at_table() {}
    }

    pub mod serving {
        fn take_order() {}

        pub fn serve_order() {}

        fn take_payment() {}
    }
}

mod back_of_house {
    fn fix_incorrect_order() {
        cook_order();
        super::front_of_house::serving::serve_order();
    }

    fn cook_order() {}
}

/*
// Nope! this is not allowed, can't redefine the module
// So perhaps namespacing is different otherwise how does
// the std module work?
mod front_of_house {
    mod serving {
        fn yell_at_customer() {}
    }
}
*/

pub fn eat_at_restaurant() {
    // Absolute path
    crate::front_of_house::hosting::add_to_waitlist();

    // Relative path
    front_of_house::hosting::add_to_waitlist();
}
