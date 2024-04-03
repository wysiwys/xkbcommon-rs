
use unicase::UniCase;
use convert_case::{Case,Casing};
use std::path::Path;
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Write};

pub fn make_keywords_file(
    in_path: &Path,
    out_path: &Path
) {

    
    let in_file = BufReader::new(File::open(in_path).unwrap());

    let mut out_file = BufWriter::new(File::create(out_path).unwrap());
    let mut keywords = vec![];
    let mut strings = vec![];

    writeln!(
        &mut out_file,
        "use crate::lexer::Token;\n
        use unicase::*;").unwrap();
    let mut map_builder = phf_codegen::OrderedMap::new();
    let re = regex::Regex::new(r"([a-z_]+),[ \t]*([A-Z_]+)").unwrap();
    for line in in_file.lines() {
        let l = line.unwrap().clone();
        if let Some((_,[key,value])) = re.captures(l.as_str())
                .map(|caps| caps.extract()) {

                    let value = value.to_case(Case::UpperCamel);

                    if !keywords.contains(&value) {
                        keywords.push(value.clone());
                        strings.push(key.to_owned());
                    }
                    let value = format!("Token::{}",value);
                    map_builder.entry(UniCase::ascii(key.to_owned()), &value);


        } else { panic!("Token could not be read"); }


    }


//to_string
    writeln!(
        &mut out_file,

        "
        pub(crate) static KEYWORDS: phf::OrderedMap<UniCase<&'static str>, Token> = {}\n;\n",
        map_builder.build()
    ).unwrap();

}
