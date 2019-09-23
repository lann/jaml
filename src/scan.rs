#[derive(Debug, PartialEq)]
enum TokenType {
    SpacesStart,
    SpacesEnd,

    Newline,

    CommentStart,
    CommentEnd,

    BareWordOrStringStart,
    BareWordNullEnd,
    BareWordTrueEnd,
    BareWordFalseEnd,
    BareStringContinue,
    BareStringStart,
    BareStringEnd,

    NumberStart,
    NumberEnd,

    QuotedStringStart,
    QuotedStringEnd,

    Dash,
    Colon,
    Question,

    OpenCurly,
    CloseCurly,

    OpenSquare,
    CloseSquare,
}

enum State {
    Next,

    Spaces,
    NewlineLF,

    Comment,

    DashOrMinus,

    NumberIntDigit,
    NumberDot,
    NumberFracDigit,
    NumberExp,
    NumberExpSign,
    NumberExpDigit,

    BareStringOrReserved(&'static [u8], u8),
    BareString,

    QuotedString,
    QuotedStringEscape,
    QuotedStringEscapeHex(u8),
}

#[derive(Debug)]
enum Error {
    BadNumberFormat,
    BadStringEscape,
    BadStringNewline,
    UnexpectedByte,
    UnexpectedEnd,
}

const LF: u8 = b'\r';
const NL: u8 = b'\n';

const END: u8 = 0xff; // End of input sentinel (0xff is never a valid UTF-8 byte)

const NULL: &'static [u8] = b"null";
const TRUE: &'static [u8] = b"true";
const FALSE: &'static [u8] = b"false";

type InternalResult = Result<(Option<TokenType>, bool, State), Error>;

impl State {
    pub fn feed(self, byte: u8) -> Result<(Option<TokenType>, Option<TokenType>, Self), Error> {
        if byte == END {
            return Err(Error::UnexpectedByte);
        }

        match self._feed(byte)? {
            (token, true, next_state) => Ok((None, token, next_state)),
            (token1, false, state) => match state._feed(byte)? {
                (token2, true, next_state) => Ok((token1, token2, next_state)),
                _ => panic!("scanner failed to advance twice in a row"),
            },
        }
    }

    pub fn end(self) -> Result<Option<TokenType>, Error> {
        match self._feed(END)? {
            (token, false, Self::Next) => Ok(token),
            _ => Err(Error::UnexpectedEnd),
        }
    }

    fn _feed(self, byte: u8) -> InternalResult {
        use self::State::*;

        match self {
            // Next token rule; currently between tokens.
            Next => match byte {
                b' ' => token_start(TokenType::SpacesStart, Spaces),
                LF => advance(NewlineLF),
                NL => token_end(TokenType::Newline),

                b'#' => token_start(TokenType::CommentStart, Comment),

                b'-' => advance(DashOrMinus),

                b'0' => Err(Error::BadNumberFormat),
                b'0'..=b'9' => token_start(TokenType::NumberStart, NumberIntDigit),

                b'n' => token_start(
                    TokenType::BareWordOrStringStart,
                    BareStringOrReserved(NULL, 1),
                ),
                b't' => token_start(
                    TokenType::BareWordOrStringStart,
                    BareStringOrReserved(TRUE, 1),
                ),
                b'f' => token_start(
                    TokenType::BareWordOrStringStart,
                    BareStringOrReserved(FALSE, 1),
                ),

                _ if is_bare_word_first(byte) => {
                    token_start(TokenType::BareStringStart, BareString)
                }

                b'"' => token_start(TokenType::QuotedStringStart, QuotedString),

                b':' => token_end(TokenType::Colon),
                b'?' => token_end(TokenType::Question),
                b'{' => token_end(TokenType::OpenCurly),
                b'}' => token_end(TokenType::CloseCurly),
                b'[' => token_end(TokenType::OpenSquare),
                b']' => token_end(TokenType::CloseSquare),

                END => Ok((None, false, Next)),

                _ => Err(Error::UnexpectedByte),
            },

            // ' '
            Spaces => match byte {
                b' ' => advance(Spaces),
                _ => token_ended(TokenType::SpacesEnd),
            },
            // '\r'
            NewlineLF => match byte {
                NL => token_end(TokenType::Newline),
                _ => token_ended(TokenType::Newline),
            },

            // '#'
            Comment => match byte {
                LF | NL | END => token_ended(TokenType::CommentEnd),
                _ => advance(Comment),
            },

            // '-'
            DashOrMinus => match byte {
                b'0' => Err(Error::BadNumberFormat),
                b'0'..=b'9' => token_started(TokenType::NumberStart, NumberIntDigit),
                _ => token_ended(TokenType::Dash),
            },

            // '-' | [1-9]
            NumberIntDigit => match byte {
                b'0'..=b'9' => advance(NumberIntDigit),
                b'.' => advance(NumberDot),
                b'e' | b'E' => advance(NumberExp),
                _ => token_ended(TokenType::NumberEnd),
            },
            // '.'
            NumberDot => match byte {
                b'0'..=b'9' => advance(NumberFracDigit),
                _ => Err(Error::BadNumberFormat),
            },
            // '.' | [0-9]
            NumberFracDigit => match byte {
                b'0'..=b'9' => advance(NumberFracDigit),
                b'e' | b'E' => advance(NumberExp),
                _ => token_ended(TokenType::NumberEnd),
            },
            // 'e' | 'E'
            NumberExp => match byte {
                b'0'..=b'9' => advance(NumberExpDigit),
                b'+' | b'-' => advance(NumberExpSign),
                _ => Err(Error::BadNumberFormat),
            },
            // '+' | '-'
            NumberExpSign => match byte {
                b'0'..=b'9' => advance(NumberExpDigit),
                _ => Err(Error::BadNumberFormat),
            },
            // '+' | '-' | [0-9]
            NumberExpDigit => match byte {
                b'0'..=b'9' => advance(NumberExpDigit),
                _ => token_ended(TokenType::NumberEnd),
            },

            // Partially matched keyword ("null" | "true" | "false")
            BareStringOrReserved(word, len) => match byte {
                _ if word.get(len as usize) == Some(&byte) => {
                    advance(BareStringOrReserved(word, len + 1))
                }
                _ if is_bare_word_rest(byte) => {
                    token_start(TokenType::BareStringContinue, BareString)
                }
                _ => token_ended(match word {
                    NULL => TokenType::BareWordNullEnd,
                    TRUE => TokenType::BareWordTrueEnd,
                    FALSE => TokenType::BareWordFalseEnd,
                    _ => TokenType::BareStringEnd,
                }),
            },
            // [a-zA-Z0-9_$./-]
            BareString => match byte {
                _ if is_bare_word_rest(byte) => advance(BareString),
                _ => token_ended(TokenType::BareStringEnd),
            },

            // Inside double-quoted string but not in escape sequence.
            QuotedString => match byte {
                b'"' => token_end(TokenType::QuotedStringEnd),
                b'\\' => advance(QuotedStringEscape),
                LF | NL => Err(Error::BadStringNewline),
                _ => advance(QuotedString),
            },
            // '\'
            QuotedStringEscape => match byte {
                b'"' | b'\\' | b'/' | b'b' | b'f' | b'n' | b'r' | b't' => advance(QuotedString),
                b'u' => advance(QuotedStringEscapeHex(0)),
                _ => Err(Error::BadStringEscape),
            },
            // "\u" [0-9a-fA-F]{digits}
            QuotedStringEscapeHex(digits) => if byte.is_ascii_hexdigit() {
                if digits == 3 {
                    advance(QuotedString)
                } else {
                    advance(QuotedStringEscapeHex(digits + 1))
                }
            } else {
                Err(Error::BadStringEscape)
            },
        }
    }
}

impl Default for State {
    fn default() -> Self {
        State::Next
    }
}

fn advance(next_state: State) -> InternalResult {
    Ok((None, true, next_state))
}

fn token_start(typ: TokenType, next_state: State) -> InternalResult {
    Ok((Some(typ), true, next_state))
}

fn token_started(typ: TokenType, next_state: State) -> InternalResult {
    Ok((Some(typ), false, next_state))
}

fn token_end(typ: TokenType) -> InternalResult {
    Ok((Some(typ), true, State::Next))
}

fn token_ended(typ: TokenType) -> InternalResult {
    Ok((Some(typ), false, State::Next))
}

fn is_bare_word_first(byte: u8) -> bool {
    byte.is_ascii_alphabetic()
        || match byte {
            b'_' | b'$' | b'.' | b'/' => true,
            _ => false,
        }
}

fn is_bare_word_rest(byte: u8) -> bool {
    is_bare_word_first(byte) || byte.is_ascii_digit() || byte == b'-'
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokenType::*;

    #[test]
    fn test_foo() {
        assert_eq!(
            tokens(b"  # abc\n"),
            [SpacesStart, SpacesEnd, CommentStart, CommentEnd, Newline,]
        );
    }

    fn tokens(bytes: &[u8]) -> Vec<TokenType> {
        let mut state = State::default();
        let mut tokens = vec![];
        for &byte in bytes {
            let (token1, token2, next_state) = state.feed(byte).unwrap();
            if let Some(token_type) = token1 {
                tokens.push(token_type);
            }
            if let Some(token_type) = token2 {
                tokens.push(token_type);
            }
            state = next_state;
        }
        if let Some(token_type) = state.end().unwrap() {
            tokens.push(token_type);
        }
        tokens
    }
}
