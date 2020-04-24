/// Code for performing gradual type checking.

use scoping::Context;
use typing::{Type, Refinement};

pub fn check_compatibility(desired_t: &Vec<Type>, given_t: &Type) -> Vec<Type> {
    let mut remaining = vec!();

    

    return remaining;
}

fn check_single_relationship(desired_t: &Type, given_t: &Type) -> bool {
    return match desired_t {
        Type::i32
    };
}
