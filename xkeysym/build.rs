

#[cfg(not(feature="generate-keysyms"))]
fn main() {

}
#[cfg(feature="generate-keysyms")]
fn main() {

    use keysym_generator::generate;
    generate("src/automatically_generated.rs").expect("Generation failed");

}
