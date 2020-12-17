use generate_telegram_api::*;
use scraper::ElementRef;

fn main() {
    let fragment = get_telegram_html();
    let find_doc_parser = vec![
        r#"div[class="dev_page_wrap"]"#,
        r#"div[class="container clearfix"]"#,
        r#"div[id="dev_page_content"]"#,
    ];

    let body = find_doc_real_body(&find_doc_parser, &fragment).unwrap();
    //println!("{:?}", body.inner_html());
    let set = vec!["p", "h4", "h3", "table", "ol"];
    let mut started = false;
    let mut pridicate = |s: &ElementRef| {
        if started == true {
            return true;
        };
        let x = s.value();

        if x.name() == "p" {
            if s.inner_html()
                == r##"The majority of bots will be OK with the default configuration, running on our servers. But if you feel that you need one of <a href="#using-a-local-bot-api-server">these features</a>, you're welcome to switch to your own at any time."##
            {
                started = true;
                return true;
            }
        }

        false
    };

    let data = pick_all_informations_with_filter(&set, &body, &mut pridicate);

    for i in 6..7 {
        //println!("{:?}", data[i].inner_html());
    }

    let mut status = Status::Nil;
    h4_check(&data[4], &mut status);
    println!("{:?}", status);
}
