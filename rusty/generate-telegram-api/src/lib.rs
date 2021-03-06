mod datatypes;

use datatypes::{Data, Method, Tableable};

use cssparser::ParseError;
use curl::easy::Easy;
use regex::Regex;
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

/// located the real body of api in
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

/// upper case word is datatype
pub fn is_datatype(s: &str) -> bool {
    if let Some(c) = s.chars().next() {
        c.is_uppercase()
    } else {
        false
    }
}

/// lower case word is method
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
                    //println!("inner_html: {:?}", e.inner_html());
                    //println!("h4: {:?}", &te);
                } else if is_method(&te) {
                    *status = Status::MethodName;
                }
            }
            _ => {}
        };
    }
}

/// pick name from h4
fn pick_name(e: &ElementRef) -> Option<String> {
    Some(e.last_child()?.value().as_text()?.text.clone().into())
}

pub fn p_check(_: &ElementRef, status: &mut Status) {
    match status {
        Status::DataTypeName => *status = Status::DataTypeDoc,
        Status::MethodName => *status = Status::MethodDoc,
        _ => (),
    }
}

fn pick_doc(e: &ElementRef) -> Option<String> {
    Some(e.inner_html())
}

pub fn table_check(_e: &ElementRef, status: &mut Status) {
    match status {
        Status::MethodName | Status::MethodDoc => *status = Status::MethodTable,
        Status::DataTypeName | Status::DataTypeDoc => *status = Status::DataTypeTable,
        _ => (),
    }
}

pub fn generate_structs<'a>(ele_l: Vec<ElementRef<'a>>) -> (Vec<Data>, Vec<Method>) {
    // init
    let mut status = Status::Nil;
    let mut datatype: Data = Default::default();
    let mut method: Method = Default::default();

    let mut data_l = vec![];
    let mut method_l = vec![];

    for ele in ele_l {
        match ele.value().name() {
            "h4" => h4_check(&ele, &mut status),
            "p" => p_check(&ele, &mut status),
            "table" => table_check(&ele, &mut status),
            "hr" => {} // end
            _ => {}
        }

        match status {
            Status::Nil => {}
            Status::DataTypeName => datatype.name = pick_name(&ele).unwrap_or("".to_string()),
            Status::DataTypeDoc => datatype.doc = pick_doc(&ele).unwrap_or("".to_string()),
            Status::DataTypeTable => pick_table(&ele, &mut datatype),
            Status::MethodName => method.name = pick_name(&ele).unwrap_or("".to_string()),
            Status::MethodDoc => method.doc = pick_doc(&ele).unwrap_or("".to_string()),
            Status::MethodTable => pick_table(&ele, &mut method),
        }

        if status.is_table() {
            if let Status::DataTypeTable = status {
                data_l.push(datatype.clone());
                datatype.clear();
            } else if let Status::MethodTable = status {
                method_l.push(method.clone());
                method.clear();
            }

            status = Status::Nil;
        }
    }
    (data_l, method_l)
}

pub fn clean_tag<'a>(s: &'a str, tag: &'a str) -> &'a str {
    let taglen = tag.len();
    let len = s.len();
    &s[taglen..len - taglen - 1]
}

/// clean >text</a, <a> tags
pub fn clean_tag_with_regex(s: &str) -> String {
    let re = Regex::new(r">(?P<n>\w+)</a").unwrap();
    if let Some(ca) = re.captures_iter(s).next() {
        ca["n"].into()
    } else {
        s.into()
    }
}

pub fn table_parser<'a>(e: &'a str) -> Vec<Vec<String>> {
    // Just get tbody, first td in tr is parameter
    let mut a = e.split('\n').filter(|s| *s != "");
    let mut status = 0;
    let mut result = vec![];
    let mut cache = vec![];
    loop {
        match a.next() {
            Some("<tbody>") => status += 1,
            Some("<tr>") => status += 1,
            Some("</tbody>") => status -= 1,
            Some("</tr>") => {
                status -= 1;
                result.push(cache.clone());
                cache.clear();
            }
            Some(v) => {
                if status == 2 && v.starts_with("<td>") {
                    cache.push(v.to_string())
                }
            }
            None => break,
        }
    }

    result
        .iter()
        .filter(|a| !a.is_empty())
        .map(|a| {
            let mut re = Vec::with_capacity(a.len());
            for (c, v) in a.iter().map(|tt| clean_tag(tt, "<td>")).enumerate() {
                if c == 1 {
                    re.push(clean_tag_with_regex(v));
                } else {
                    re.push(v.into());
                }
            }
            re
        })
        .collect()
}

fn pick_table(e: &ElementRef, mut d: impl Tableable<Item = Vec<String>>) {
    let a = table_parser(&e.inner_html());
    //dbg!(&a);
    d.fill_from_table(a.into_iter());
}

#[cfg(test)]
mod tests {
    use super::*;
    use scraper::ElementRef;

    const table_str_data:&'static str = "\n<thead>\n<tr>\n<th>Field</th>\n<th>Type</th>\n<th>Description</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td>update_id</td>\n<td>Integer</td>\n<td>The update\'s unique identifier. Update identifiers start from a certain positive number and increase sequentially. This ID becomes especially handy if you\'re using <a href=\"#setwebhook\">Webhooks</a>, since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order. If there are no new updates for at least a week, then identifier of the next update will be chosen randomly instead of sequentially.</td>\n</tr>\n<tr>\n<td>message</td>\n<td><a href=\"#message\">Message</a></td>\n<td><em>Optional</em>. New incoming message of any kind — text, photo, sticker, etc.</td>\n</tr>\n<tr>\n<td>edited_message</td>\n<td><a href=\"#message\">Message</a></td>\n<td><em>Optional</em>. New version of a message that is known to the bot and was edited</td>\n</tr>\n<tr>\n<td>channel_post</td>\n<td><a href=\"#message\">Message</a></td>\n<td><em>Optional</em>. New incoming channel post of any kind — text, photo, sticker, etc.</td>\n</tr>\n<tr>\n<td>edited_channel_post</td>\n<td><a href=\"#message\">Message</a></td>\n<td><em>Optional</em>. New version of a channel post that is known to the bot and was edited</td>\n</tr>\n<tr>\n<td>inline_query</td>\n<td><a href=\"#inlinequery\">InlineQuery</a></td>\n<td><em>Optional</em>. New incoming <a href=\"#inline-mode\">inline</a> query</td>\n</tr>\n<tr>\n<td>chosen_inline_result</td>\n<td><a href=\"#choseninlineresult\">ChosenInlineResult</a></td>\n<td><em>Optional</em>. The result of an <a href=\"#inline-mode\">inline</a> query that was chosen by a user and sent to their chat partner. Please see our documentation on the <a href=\"/bots/inline#collecting-feedback\">feedback collecting</a> for details on how to enable these updates for your bot.</td>\n</tr>\n<tr>\n<td>callback_query</td>\n<td><a href=\"#callbackquery\">CallbackQuery</a></td>\n<td><em>Optional</em>. New incoming callback query</td>\n</tr>\n<tr>\n<td>shipping_query</td>\n<td><a href=\"#shippingquery\">ShippingQuery</a></td>\n<td><em>Optional</em>. New incoming shipping query. Only for invoices with flexible price</td>\n</tr>\n<tr>\n<td>pre_checkout_query</td>\n<td><a href=\"#precheckoutquery\">PreCheckoutQuery</a></td>\n<td><em>Optional</em>. New incoming pre-checkout query. Contains full information about checkout</td>\n</tr>\n<tr>\n<td>poll</td>\n<td><a href=\"#poll\">Poll</a></td>\n<td><em>Optional</em>. New poll state. Bots receive only updates about stopped polls and polls, which are sent by the bot</td>\n</tr>\n<tr>\n<td>poll_answer</td>\n<td><a href=\"#pollanswer\">PollAnswer</a></td>\n<td><em>Optional</em>. A user changed their answer in a non-anonymous poll. Bots receive new votes only in polls that were sent by the bot itself.</td>\n</tr>\n</tbody>\n";

    const table_str_method: &'static str = "\n<thead>\n<tr>\n<th>Parameter</th>\n<th>Type</th>\n<th>Required</th>\n<th>Description</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td>offset</td>\n<td>Integer</td>\n<td>Optional</td>\n<td>Identifier of the first update to be returned. Must be greater by one than the highest among the identifiers of previously received updates. By default, updates starting with the earliest unconfirmed update are returned. An update is considered confirmed as soon as <a href=\"#getupdates\">getUpdates</a> is called with an <em>offset</em> higher than its <em>update_id</em>. The negative offset can be specified to retrieve updates starting from <em>-offset</em> update from the end of the updates queue. All previous updates will forgotten.</td>\n</tr>\n<tr>\n<td>limit</td>\n<td>Integer</td>\n<td>Optional</td>\n<td>Limits the number of updates to be retrieved. Values between 1-100 are accepted. Defaults to 100.</td>\n</tr>\n<tr>\n<td>timeout</td>\n<td>Integer</td>\n<td>Optional</td>\n<td>Timeout in seconds for long polling. Defaults to 0, i.e. usual short polling. Should be positive, short polling should be used for testing purposes only.</td>\n</tr>\n<tr>\n<td>allowed_updates</td>\n<td>Array of String</td>\n<td>Optional</td>\n<td>A JSON-serialized list of the update types you want your bot to receive. For example, specify [“message”, “edited_channel_post”, “callback_query”] to only receive updates of these types. See <a href=\"#update\">Update</a> for a complete list of available update types. Specify an empty list to receive all updates regardless of type (default). If not specified, the previous setting will be used.<br><br>Please note that this parameter doesn\'t affect updates created before the call to the getUpdates, so unwanted updates may be received for a short period of time.</td>\n</tr>\n</tbody>\n";

    #[test]
    fn table_parser_test() {
        println!("{:?}", table_parser(table_str_data));
    }

    #[test]
    fn pick_table_test() {
        let mut data: Data = Default::default();
        let a = table_parser(table_str_data);
        (&mut data).fill_from_table(a.into_iter());
        dbg!(data);
        //dbg!(data.fields);
        //dbg!(data.types);
        //dbg!(data.descriptions);
    }

    #[test]
    fn pick_table_method_test() {
        let mut data: Method = Default::default();
        let a = table_parser(table_str_method);
        (&mut data).fill_from_table(a.into_iter());
        dbg!(data);
        //dbg!(data.parameters);
        //dbg!(data.types);
        //dbg!(data.requireds);
        //dbg!(data.descriptions);
    }

    #[test]
    fn pick_data_test() {
        //:= TODO: test whole html
        Html::parse_fragment("./tests/api.html");
    }
}
