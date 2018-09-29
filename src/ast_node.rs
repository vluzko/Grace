use std::fmt::Display;


pub trait ASTNode: Display{
    /// Generate WAST bytecode from an AST
    /// CONVENTIONS:
    ///     Generated bytecode should not start or end with a newline, *except* for modules / other
    ///     top level functions. Starting and ending newlines are the responsibility of the parent
    ///     node. We don't have to do it this way, but it's important to have a single convention.
    ///     Nodes *do* need to handle newlines at the start and end of their *children*, obviously.
    fn generate_bytecode(&self) -> String;
}
