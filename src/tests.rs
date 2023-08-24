#[test]
fn digits_to_words() {
    let digits = ["1", "2", "4", "8", "16", "32", "64", "128", "256", "512"];
    let words = [
        "One",
        "Two",
        "Four",
        "Eight",
        "Sixteen",
        "ThirtyTwo",
        "SixtyFour",
        "OneHundredTwentyEight",
        "TwoHundredFiftySix",
        "FiveHundredTwelve",
    ];

    let mut buffer = Vec::new();
    for (digit, word) in digits.iter().zip(words.iter()) {
        super::conv::d2w(&mut buffer, digit.as_bytes());
        assert_eq!(word.as_bytes(), buffer.as_slice());
        buffer.clear();
    }
}

#[test]
fn caps_to_pascal_case() {
    let mut buffer = Vec::new();
    let text = "CAPS";

    super::conv::c2pc(&mut buffer, text.bytes());
    assert_eq!(b"Caps", buffer.as_slice());
}

#[test]
fn pascal_case_to_screaming_snake_case() {
    let mut buffer = Vec::new();
    let text = "ThisIsPascalCase";
    super::conv::pc2ssc(&mut buffer, text.bytes());

    assert_eq!(b"THIS_IS_PASCAL_CASE", buffer.as_slice());
}

#[test]
fn camel_case_to_snake_case() {
    let mut buffer = Vec::new();
    let text = "thisIsCamelCase";
    super::conv::cc2sc(&mut buffer, text.bytes());

    assert_eq!(b"this_is_camel_case", buffer.as_slice());
}

#[test]
fn screaming_snake_case_to_pascal_case() {
    let mut buffer = Vec::new();
    let text = "HELLO_THERE_HOW_ARE_YOU";
    super::conv::ssc2pc(&mut buffer, text.bytes());

    assert_eq!(b"HelloThereHowAreYou", buffer.as_slice());
}
