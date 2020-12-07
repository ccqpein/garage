mod datatypes;

use cssparser::ParseError;
use curl::easy::Easy;
use scraper::node::Node;
use scraper::{ElementRef, Html, Selector};
use selectors::parser::SelectorParseErrorKind;

pub fn get_telegram_html() -> Html {
    let mut dst = Vec::new();
    let mut easy = Easy::new();
    easy.url("https://core.telegram.org/bots/api").unwrap();

    let mut transfer = easy.transfer();

    transfer
        .write_function(|data| {
            dst.extend_from_slice(data);
            Ok(data.len())
        })
        .unwrap();

    transfer.perform().unwrap();
    drop(transfer); // Drop!

    // make page in a huge string
    let page = String::from_utf8(dst).unwrap();

    Html::parse_fragment(&page)
}

pub fn find_doc_real_body<'a>(
    tags: &'a [&str],
    html: &'a Html,
) -> Result<ElementRef<'a>, ParseError<'a, SelectorParseErrorKind<'a>>> {
    let mut element_ref = vec![html.root_element()];
    // select tag deeper and deeper
    for tag in tags {
        let selector = match Selector::parse(tag) {
            Ok(h) => h,
            Err(e) => return Err(e),
        };

        element_ref = element_ref
            .iter()
            .map(|ele| ele.select(&selector).collect::<Vec<ElementRef<'a>>>())
            .flatten()
            .collect();
    }

    Ok(element_ref[0])
}

pub fn pick_all_informations_with_filter<'a, P>(
    tags: &'a [&str],
    ele: &ElementRef<'a>,
    predicate: &mut P,
) -> Vec<ElementRef<'a>>
where
    P: FnMut(&ElementRef<'a>) -> bool,
{
    let selectors: Vec<Selector> = tags.iter().map(|s| Selector::parse(s).unwrap()).collect();
    let mut result = vec![];
    for e in ele.children() {
        if let Some(a) = ElementRef::wrap(e) {
            if selectors.iter().any(|sele| sele.matches(&a)) && predicate(&a) {
                result.push(a)
            }
        } else {
            //println!("not element: {:?}", e.value());
        }
    }
    result
}

///
///
#[derive(Debug)]
pub enum Status {
    Nil,

    DataTypeName,
    DataTypeDoc,
    DataTypeTable,

    MethodName,
    MethodDoc,
    MethodTable,
}
impl Status {
    pub fn is_table(&self) -> bool {
        match self {
            Self::DataTypeTable | Self::MethodTable => true,
            _ => false,
        }
    }
}

pub fn is_datatype(s: &str) -> bool {
    if let Some(c) = s.chars().next() {
        c.is_uppercase()
    } else {
        false
    }
}

pub fn is_method(s: &str) -> bool {
    if let Some(c) = s.chars().next() {
        c.is_lowercase()
    } else {
        false
    }
}

pub fn h4_check(e: &ElementRef, status: &mut Status) {
    // there is a <a> tag at beginning, so, get the last
    if let Some(node) = e.last_child() {
        match node.value() {
            Node::Text(te) => {
                if is_datatype(&te) {
                    *status = Status::DataTypeName;
                    //println!("h4: {:?}", &te);
                } else if is_method(&te) {
                    *status = Status::MethodName;
                }
            }
            _ => {}
        };
    }
}

pub fn p_check(e: &ElementRef, status: &mut Status) {
    match status {
        Status::DataTypeName => *status = Status::DataTypeDoc,
        Status::MethodName => *status = Status::MethodDoc,
        _ => (),
    }
}

pub fn table_check(e: &ElementRef, status: &mut Status) {
    match status {
        Status::MethodName | Status::MethodDoc => *status = Status::MethodTable,
        Status::DataTypeName | Status::DataTypeDoc => *status = Status::DataTypeTable,
        _ => (),
    }
}

pub fn generate_structs<'a>(ele_l: Vec<ElementRef<'a>>) {
    let mut status = Status::Nil;
    for ele in ele_l {
        match ele.value().name() {
            "h4" => h4_check(&ele, &mut status),
            "p" => p_check(&ele, &mut status),
            "table" => table_check(&ele, &mut status),
            _ => {}
        }

        //:= TODO: generate struct
        //:= TODO: table parser

        if status.is_table() {
            status = Status::Nil;
        }
    }
}

pub fn clean_tag<'a>(s: &'a str, tag: &'a str) -> &'a str {
    let taglen = tag.len();
    let len = s.len();
    &s[taglen..len - taglen - 1]
}

pub fn table_parser<'a>(e: &'a str) -> Vec<(&'a str, &'a str, &'a str)> {
    //:= Just get tbody, first td in tr is parameter
    let mut a = e.split('\n').filter(|s| *s != "");
    let mut status = 0;
    let mut result = vec![];
    loop {
        match a.next() {
            Some("<tbody>") => status += 1,
            Some("<tr>") => status += 1,
            Some("</tbody>") => status -= 1,
            Some("</tr>") => status -= 1,
            Some(v) => {
                if status == 2 && v.starts_with("<td>") {
                    result.push((v, a.next().unwrap(), a.next().unwrap()));
                }
            }
            None => break,
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use scraper::ElementRef;

    #[test]
    fn table_parser_test() {
        let data = "\n<thead>\n<tr>\n<th>Field</th>\n<th>Type</th>\n<th>Description</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td>update_id</td>\n<td>Integer</td>\n<td>The update\'s unique identifier. Update identifiers start from a certain positive number and increase sequentially. This ID becomes especially handy if you\'re using <a href=\"#setwebhook\">Webhooks</a>, since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order. If there are no new updates for at least a week, then identifier of the next update will be chosen randomly instead of sequentially.</td>\n</tr>\n<tr>\n<td>message</td>\n<td><a href=\"#message\">Message</a></td>\n<td><em>Optional</em>. New incoming message of any kind — text, photo, sticker, etc.</td>\n</tr>\n<tr>\n<td>edited_message</td>\n<td><a href=\"#message\">Message</a></td>\n<td><em>Optional</em>. New version of a message that is known to the bot and was edited</td>\n</tr>\n<tr>\n<td>channel_post</td>\n<td><a href=\"#message\">Message</a></td>\n<td><em>Optional</em>. New incoming channel post of any kind — text, photo, sticker, etc.</td>\n</tr>\n<tr>\n<td>edited_channel_post</td>\n<td><a href=\"#message\">Message</a></td>\n<td><em>Optional</em>. New version of a channel post that is known to the bot and was edited</td>\n</tr>\n<tr>\n<td>inline_query</td>\n<td><a href=\"#inlinequery\">InlineQuery</a></td>\n<td><em>Optional</em>. New incoming <a href=\"#inline-mode\">inline</a> query</td>\n</tr>\n<tr>\n<td>chosen_inline_result</td>\n<td><a href=\"#choseninlineresult\">ChosenInlineResult</a></td>\n<td><em>Optional</em>. The result of an <a href=\"#inline-mode\">inline</a> query that was chosen by a user and sent to their chat partner. Please see our documentation on the <a href=\"/bots/inline#collecting-feedback\">feedback collecting</a> for details on how to enable these updates for your bot.</td>\n</tr>\n<tr>\n<td>callback_query</td>\n<td><a href=\"#callbackquery\">CallbackQuery</a></td>\n<td><em>Optional</em>. New incoming callback query</td>\n</tr>\n<tr>\n<td>shipping_query</td>\n<td><a href=\"#shippingquery\">ShippingQuery</a></td>\n<td><em>Optional</em>. New incoming shipping query. Only for invoices with flexible price</td>\n</tr>\n<tr>\n<td>pre_checkout_query</td>\n<td><a href=\"#precheckoutquery\">PreCheckoutQuery</a></td>\n<td><em>Optional</em>. New incoming pre-checkout query. Contains full information about checkout</td>\n</tr>\n<tr>\n<td>poll</td>\n<td><a href=\"#poll\">Poll</a></td>\n<td><em>Optional</em>. New poll state. Bots receive only updates about stopped polls and polls, which are sent by the bot</td>\n</tr>\n<tr>\n<td>poll_answer</td>\n<td><a href=\"#pollanswer\">PollAnswer</a></td>\n<td><em>Optional</em>. A user changed their answer in a non-anonymous poll. Bots receive new votes only in polls that were sent by the bot itself.</td>\n</tr>\n</tbody>\n";
        println!(
            "{:?}",
            table_parser(&data)
                .iter()
                .map(|(a, b, c)| (
                    clean_tag(a, "<td>"),
                    clean_tag(b, "<td>"),
                    clean_tag(c, "<td>")
                ))
                .collect::<Vec<_>>()
        );
    }
}