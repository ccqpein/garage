use ego_tree::iter::Children;
use ego_tree::NodeRef;
use scraper::node::Node;
use scraper::ElementRef;
use std::io::{Error, ErrorKind, Result};

/// iterate and skip children nods and stop **at** the predicate true
pub fn skip_children_after<'a, P>(
    cs: &mut ElementRef<'a>,
    predicate: P,
) -> Result<Children<'a, Node>>
where
    P: Fn(&NodeRef<Node>) -> bool,
{
    let mut children = cs.children();
    match children.find(|a| predicate(a)) {
        Some(_) => Ok(children),
        None => Err(Error::new(ErrorKind::NotFound, "Cannot find")),
    }
}
