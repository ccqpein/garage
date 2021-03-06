use cssparser::ParseError;
use scraper::{ElementRef, Html, Selector};
use selectors::parser::SelectorParseErrorKind;

/// find tags deeper and deeper
#[derive(Debug)]
pub struct ParserTree {
    selectors: Vec<Selector>,
}

impl ParserTree {
    pub fn new<'a>(
        argvs: &'a [&'a str],
    ) -> Result<Self, ParseError<'a, SelectorParseErrorKind<'a>>> {
        let mut cache = Vec::with_capacity(argvs.len());
        for s in argvs {
            match Selector::parse(s) {
                Ok(h) => cache.push(h),
                Err(e) => return Err(e),
            }
        }

        Ok(Self { selectors: cache })
    }

    pub fn select_html<'a>(&self, html: &'a Html) -> Vec<ElementRef<'a>> {
        let root = html.root_element();
        self.select_ele(&root)
    }

    pub fn select_ele<'a>(&self, ele: &ElementRef<'a>) -> Vec<ElementRef<'a>> {
        let mut a: Vec<ElementRef<'a>> = vec![ele.clone()];
        for i in 0..self.selectors.len() {
            a = a
                .iter()
                .map(|ele_ref| {
                    ele_ref
                        .select(&self.selectors[i])
                        .collect::<Vec<ElementRef<'a>>>()
                })
                .flatten()
                .collect();
        }
        a
    }
}

/// find all tags/class/etc. in one big tag.
#[derive(Debug)]
pub struct ParserSet {
    selectors: Vec<Selector>,
}

impl ParserSet {
    pub fn new<'a>(
        argvs: &'a [&'a str],
    ) -> Result<Self, ParseError<'a, SelectorParseErrorKind<'a>>> {
        let mut cache = Vec::with_capacity(argvs.len());
        for s in argvs {
            match Selector::parse(s) {
                Ok(h) => cache.push(h),
                Err(e) => return Err(e),
            }
        }

        Ok(Self { selectors: cache })
    }

    pub fn select_html<'a>(&self, html: &'a Html) -> Vec<ElementRef<'a>> {
        let root = html.root_element();
        self.select_ele(&root)
    }

    pub fn select_ele<'a>(&self, ele: &ElementRef<'a>) -> Vec<ElementRef<'a>> {
        let mut result = vec![];
        for e in ele.children() {
            if let Some(ee) = ElementRef::wrap(e) {
                if self.selectors.iter().any(|selector| selector.matches(&ee)) {
                    result.push(ee);
                }
            }
        }
        result
    }
}
