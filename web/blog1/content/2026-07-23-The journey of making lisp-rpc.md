---
title: "The journey of making lisp-rpc"
date: "2026-07-23"
slug: "the-journey-of-making-lisp-rpc"
tags: ["rust", "lisp", "llm"]
---

Once upon a time, I was exploring the internet for some LLM stuff, and I found a protocol called JSON-RPC. It sparked an idea: why not write it in Lisp?

To my understanding, JSON-RPC definitely has some benefits:

+ JSON is a very common format used everywhere
+ It is readable for human beings (compared to binary protocols)

Because of those two benefits, JSON-RPC looks like a very good fit for LLM usage.

Thinking about Lisp, it is pretty readable to me. And Lisp's dark magic is that the language format is the data itself—like `(a b c)` is a list of `a`, `b`, and `c`, and can also be the function `a` called with `b` and `c` as arguments.

So if the data I receive is Lisp, I can just evaluate it with `(eval (read (make-string-input-stream some-data)))`. So I thought it might be fun and cool if I could use Lisp as the RPC format.

A few days ago, I found one more benefit: a pure string format payload can evolve faster for APIs. For example, `{"tools":[{"a": {}}]}` can change to `{"tools":[{"type": "a"}]}` through simple JSON magic. In typed serialization, like in Rust, request types that serialize to this JSON need a lot of changes, and that isn't fun (unless there are tools that can generate structures from JSON, like generating Golang code from a proto file).

# Design

At the beginning, I wanted to make it as simple as possible. There were only a few things I was considering:

+ Dict/JSON/map-like structures extracted from data
+ It has to use valid Common Lisp syntax and grammar

Like: 

+ JSON-RPC: `{"jsonrpc": "2.0", "method": "getUser", "params": {"id": 42}}`
+ Lisp-RPC: `(rpc-call-name :argument0 "getUser" :argument1 '(:id 42))`

In spec definition mode, write the declaration in Lisp and generate native code, just like gRPC generates Golang code from proto files.

The whole lisp-rpc spec is [here](https://lisp-rpc.ccqpein.me).

# Parser

[source code](https://github.com/ccqpein/lisp-rpc/tree/main/parsers)

The parser was written entirely by hand, at least for the first few months. I had written many demos before, so it didn't take me long to write it.

# Raw Data

[source code](https://github.com/ccqpein/lisp-rpc/tree/main/raw-data)

This part just makes the lisp-rpc data behave like a dict/JSON/map. I can handle it straightforwardly in lightweight use cases.

# Generator

After the parser, I started writing the generator. `def-rpc` and `def-msg` are valid Lisp syntax, so I could just use the parser alongside a template engine to generate the code. It took me a while to decide which template engine I wanted. Then I introduced `gemini-cli` (now `agy`) to join the party.

Learning a template engine would take a lot of time, and I just needed it to generate pretty simple code. So I wanted Gemini to teach me (and write some templates for me, of course). It worked pretty well: I designed the other parts and let Gemini reformat the template. This fun journey inspired me to provide an LLM-friendly spec [here](https://github.com/ccqpein/lisp-rpc/blob/main/example-specs/lisp-rpc-prompt.md) to help LLMs understand lisp-rpc more easily. So if I write an app and want an LLM to generate lisp-rpc format data, it can make it happen. And yes, I actually use it in my personal project [accountant](https://github.com/ccqpein/garage/blob/153548d437e90e008ab769ff90a8bc7af482f994/rusty/accountant/accountant.lisprpc).

## Derive Macro?

After the generator generated structures with `def-msg` or `def-rpc`, I definitely needed to implement some methods for the structures. I considered two solutions:

+ Derive macro
+ Write another template

TL;DR: Derive macros made things more complex. So I just used templates, wrote some simple macros in the `lisp-rpc-server` package, and imported them.

## lisp-rpc-server?

[source code](https://github.com/ccqpein/lisp-rpc/tree/main/server)

I really like the Golang gRPC server workflow: get the protocol, generate the server-side code, and just run it. `lisp-rpc` is a bit different: RPC structs are marked, and then the server library can register functions that accept the RPC structure as handlers.

## Serializer

`lisp-rpc` is a pure string data format (at least for now), so all generated structures need to be serializable to and deserializable from strings.

The good news is that Rust's `serde` library is convenient enough; I just needed to add `Serialize`/`Deserialize` to the derive section in my template.

The bad news is that I needed to write my own custom serializer/deserializer. Thanks to Gemini for helping with this part too.

# What's Next?

So, what's next? I actually have a lot of ideas in mind. First, gRPC supports backward compatibility, and I want to implement that too. Second, maybe I need a binary format converter? I'm still thinking about it.

Most of all, I need to implement it in other languages like Common Lisp (which I am working on now) and Golang.

# Conclusion

I feel like this is a great coding exercise project. Even though I paired with Gemini (`agy`), I still had a lot of fun and gained new insights on how to pair with LLMs.

Making a 100% perfect design is hard. The best approach is still: build an MVP, use it, refine it, and keep going. For instance, I noticed I needed to support float types while building my [accountant](https://github.com/ccqpein/garage/tree/153548d437e90e008ab769ff90a8bc7af482f994/rusty/accountant) app. Fortunately, since the architecture was already established, it wasn't hard to add.
