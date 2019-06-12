use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::BTreeSet;
use std::hash::Hash;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::fmt::Debug;

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

pub fn vec_c_union<T>(a: &Vec<T>, b: &Vec<T>) -> Vec<T> 
where T: Eq, T: Clone {
    let mut new_vec: Vec<T> = a.clone();
    for element in b.iter() {
        if !new_vec.contains(element) {
            new_vec.push((*element).clone());
        }
    }
    return new_vec;
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

pub fn vec_subset<T>(a: &Vec<T>, b: &Vec<T>) -> bool 
where T: Eq, T: Clone {
    for element in a.iter() {
        if !b.contains(element) {
            return false;
        }
    }
    return true;
}

pub fn extend_map<K, V>(mut a: HashMap<K, V>, b: HashMap<K, V>) -> HashMap<K, V>
where V: Eq, K: Hash, K: Eq, K: Debug, V: Debug {
    println!("Merging maps: {:?}, {:?}",a, b );
    for (k, v) in b.into_iter() {
        if (a.contains_key(&k)) {
            panic!("Duplicate key found:\n {:?}\n {:?}", v, a.get(&k));
        } else {
            a.insert(k, v);
        }
    }
    return a;
}

static NODE_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);
static SCOPE_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub fn get_next_id() -> usize {
    let next_id = NODE_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
    return next_id as usize;
}

/// Return a unique scope ID.
pub fn get_next_scope_id() -> usize {
    let next_id = SCOPE_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
    return next_id as usize;
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_vec_c_int() {
        let vec1 = vec![1, 2, 3];
        let vec2 = vec![3, 4, 5];
        let merged = vec_c_int(&vec1, &vec2);
        println!("{:?}", merged);
    }
}