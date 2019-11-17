#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq, Ord, PartialOrd)]
pub struct Loc(pub i32);

impl Loc {
    pub fn next(&mut self) {
        self.0 += 1;
    }

    pub fn proceed(&mut self, c: char) {
        match c {
            '\x0a' | '\x0d' => self.next(),
            _ => {}
        }
    }
}
