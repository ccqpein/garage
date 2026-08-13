---
title: "Lisp-RPC example: accountant"
date: "2026-08-29"
slug: "lisp-rpc-example-accountant"
tags: ["lisp-rpc", "rust", "llm"]
---

I made [lisp-rpc](https://github.com/ccqpein/lisp-rpc/). Then I thought I should be the first one to use it. I needed a demo project using lisp-rpc, and it also had two other benefits:

1. I can find something I missed in lisp-rpc and then fix it
2. It serves as an example for people who are going to use `lisp-rpc`

So here we go: I tried to write a simple accountant server app behind my Telegram bot.

# Design

I want to use a Telegram bot as the "frontend" of this app. I had made two versions before. But this time I would like to rewrite it with Antigravity. 

The Telegram bot's job in this situation is like an API gateway that translates my language to a transaction in lisp-rpc format with an LLM. 

So it can prove two things: lisp-rpc can be understood easily, and LLMs can be the killer app for APIs, like I said [before](https://ccqpein.me/posts/some-thoughts-of-pairing-with-llms-1).

> I've always thought LLMs are the killer abstraction layer for API calls since ChatGPT 3.5. They filled the gap between natural language and APIs.

## Accountant app

Since the Telegram bot just sends raw lisp-rpc data, I want to use the other spec mode in the accountant app. So the accountant app receives the data and inserts it into, well, a file. 

I had done this before—using a pure Lisp file as the data storage. I would like to use this trick this time too, for some future features.

# Telegram bot part

This is my third time writing a Telegram bot. This time was easy because I just asked Antigravity to write it for me. This Telegram accountant app will call the LLM API and send the lisp-rpc data. 

So the most unique part now is the lisp-rpc prompt. After chatting with Gemini for a while, I got the prompt. It is long, so I'll just post the summary here:

> # Accountant Role and Instructions
> You are a helpful accountant bot. Your goal is to gather transaction details from the user to construct a Lisp-RPC transaction data payload.
> 
> ...
>
> ## Example Final Output:
> If the user says "/tx Apple Music subscription for $52.99 on my Apple Card" and all details are gathered, the output must be exactly:
> (transaction :timestamp "2026-07-06T18:46:00-04:00" :account "apple-card" :tx-type "expense" :amount 52.99 :category '("entertainment" "subscriptions"))__DONE__

Then I can detect if the response from the LLM ends with `__DONE__`, trim it, and get the data I can send to the accountant app.

# Accountant part

The accountant part will use the spec mode. It will parse the lisp-rpc data from the Telegram bot and insert it into a single file.

## Spec and generate

So the first step is to define the spec:

```lisp
;; accountant.lisprpc file
(def-rpc-package accountant-rpc)

(def-rpc transaction
    '(:timestamp 'string
      :account 'string
      :tx-type 'string
      :amount 'float
      :category (list 'string)
      :target-account (optional 'string)
      :tx-id (optional 'string))
  'string)
```

Then I ran `lisp-rpc-rust-generator -i ./accountant.lisprpc -o .` to generate the `accountant-rpc` lib:

```rust
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Transaction {
    pub timestamp: String,
    pub account: String,
    pub tx_type: String,
    pub amount: f64,
    pub category: Vec<String>,
    pub target_account: Option<String>,
    pub tx_id: Option<String>,
}

impl_to_rpc!(Transaction, RPCType::RPC("transaction".to_string()));
```

After the lib was generated, I needed to register it in the server:

```rust
#[post("/rpc")]
async fn rpc_handler(body: String, server: web::Data<RPCServer>) -> impl Responder {
    match server.handle(&body) {
        Ok(response) => HttpResponse::Ok().body(response),
        Err(e) => HttpResponse::BadRequest().body(format!("RPC Error: {}", e)),
    }
}

/// main.rs
#[actix_web::main]
async fn main() -> anyhow::Result<()> {
    ...
    // 1. Setup the RPC Engine
    let server = RPCServer::new()
        .register::<Transaction, _>(move |mut tx: Transaction| {
            println!("Received Transaction via Actix: {:?}", tx);
            entry(data_folder_path.clone(), &mut tx)?;
            Ok(format!("Processed transaction: {:?}", tx.serialize_lisp()))
        })
        .map_err(|e| anyhow::anyhow!("RPC Registration Error: {}", e))?;

    println!("Starting Actix-web RPC Server on http://127.0.0.1:3388");

    // 2. Setup Actix-web Server
    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(server.clone()))
            .route("/", web::get().to(hello))
            .service(rpc_handler)
    })
    .bind(("127.0.0.1", 3388))?
    .run()
    .await?;

    Ok(())
}
```

## Side quest: Raw data lib

Then I was thinking that I needed a config file, and that I should use the lisp-rpc format too. So I needed a lib that could handle the raw data. After finishing [lisp-rpc raw-data](https://github.com/ccqpein/lisp-rpc/tree/main/raw-data/lisp-rpc-rust-raw-data), I just made a config file including all the config fields I needed and read it in the main function: 

```rust
let args = Args::parse();
let config_file = args
    .config_file
    .unwrap_or_else(|| PathBuf::from("./accountant-config.lisprpc"));

let config_file = DataFile::new(config_file)?;
let data_folder = config_file
    .gen_table() // generate the whole table
    .get("config")
    .context("Cannot get expr data")?
    .get("data-folder")
    .context("Cannot get data-folder")?
    .to_string();
```

Then the data server and data parsing parts were done. That's it. Now the lisp-rpc job is done. The accountant repo is [here](https://github.com/ccqpein/garage/tree/master/rusty/accountant).

# Wrap-Up 

I truly ran into a lot of features I wanted to have but didn't implement yet when I wrote the accountant app. So I actually added features while developing the accountant app, like raw data, optional data, float numbers, return values, etc. So it reached the targets I wanted before I made this app.

Using a Lisp file as the file DB gives me more flexibility to deal with them in my Lisp REPL. I can just read the s-expressions and calculate all my costs.
