use generate_telegram_api::*;
use scraper::ElementRef;
use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let fragment = get_telegram_html();
    let find_doc_parser = vec![
        r#"div[class="dev_page_wrap"]"#,
        r#"div[class="container clearfix"]"#,
        r#"div[id="dev_page_content"]"#,
    ];

    // point to real body's general location
    let body = find_doc_real_body(&find_doc_parser, &fragment).unwrap();

    // this is all tags with content inside
    let set = vec!["p", "h4", "h3", "table"];
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

    // pick all data
    let data = pick_all_informations_with_filter(&set, &body, &mut pridicate);

    let (all_data_types, all_methods) = generate_structs(data);

    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
    let mut file = File::create(&args[1])?;

    file.write(b"[")?;
    for d in all_data_types {
        file.write_all(d.to_string()?.as_bytes());
        file.write(b",")?;
    }
    file.write(b"]")?;

    // for i in 6..10 {
    //     println!("{:?}", data[i].inner_html());
    // }

    //let mut status = Status::Nil;
    //h4_check(&data[4], &mut status);
    //println!("{:?}", status);
    Ok(())
}
