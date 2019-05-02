use std::collections::HashSet;
use std::collections::BTreeSet;
use std::hash::Hash;

pub fn c_int<T>(a: &HashSet<T>, b: &HashSet<T>) -> HashSet<T>
where T: Eq, T: Hash, T: Clone {
    let int = a.intersection(b);
    let cloned = int.into_iter().map(|x| x.clone()).collect();
    return cloned;
} 

/// Take the union of two hashsets, cloning elements from both and consuming neither set.
pub fn c_union<T>(a: &HashSet<T>, b: &HashSet<T>) -> HashSet<T>
where T: Eq, T: Hash, T: Clone {
    let union = a.union(b);
    let cloned = union.into_iter().map(|x| x.clone()).collect();
    return cloned;  
}

/// Take the union of two hashsets, moving elements out of the second. Both original sets are consumed.
pub fn m_union<T>(mut a: HashSet<T>,  b: HashSet<T>) -> HashSet<T>
where T: Eq, T: Hash {
    for element in b.into_iter() {
        a.insert(element);
    }
    return a;
}

pub fn vec_c_int<T>(a: &Vec<T>, b: &Vec<T>) -> Vec<T> 
where T: Eq, T: Clone {
    let mut new_vec: Vec<T> = vec!();
    for element in a.iter() {
        if b.contains(element) {
            new_vec.push((*element).clone());
        }
    }
    return new_vec;
}