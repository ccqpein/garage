---
title: "Rewriting a Lib into Another Language with an LLM"
date: "2026-10-14"
slug: "re-write-one-lib-to-another-language-by-LLM"
tags: ["lisp-rpc", "llm", "antigravity", "golang", "rust"]
---

I built the Rust and Lisp versions of lisp-rpc. These two versions cleared my thoughts, and the design kind of settled down after them.

Then I thought I needed a Golang version. The Golang version was totally vibed. Through this, I gained some experience with how to totally rewrite a project into another language using an LLM.

# Vibe it

The prompt was pretty simple: I had Antigravity read the README, spec, and tests of each component one by one.

Since I knew how I developed the original version, I let agy do it from the parser to raw data, then the server and generator.

## The problems with the vibed code

The LLM did it pretty well and fast (compared to writing it by hand, of course). But there were a few things I noticed after it finished.

### Too strict

There is a lot of over-design in Golang. Some of the features I kept in Rust because they were easier for me to read and made the Rust compiler happy. But Golang has a runtime, so I don't need to worry about those things. 

The LLM carried over designs that were only meant for Rust 100% into Golang. I'm probably 50% responsible for this problem because I asked it to translate.

### Server side

Golang is great on the server side, so the async and sync parts in Rust aren't needed at all in Golang. Everything is already async in Golang. 

# Tips

> LLMs can handle general things, but they still just translate.

Antigravity copied the entire design to Golang, even things that could be totally abandoned because of how Golang works.

> Slop code happens; seed spawning causes a lot of diff churn.

I usually check all the code changes and every commit. And I found that some `AI slop` not only affects the current code, but also affects the code after. I think it is a fundamental problem with LLMs because "attention" makes the LLM care more about tokens close to the current one than older ones. So the slop it made in this session is treated as a bit more important than the rest. 

It feels like a slop seed, and it will keep growing if I don't prune it early.

# Wrap up

This rewrite gave me some good takeaways:

+ It proves that I need to check the code myself.
+ I need to understand both languages.
+ I need to ask the right questions. 
+ I need to tell it exactly what I want, why, and what should be avoided.
